#!/usr/bin/python
#################################################################
# Script to automate processing of the GBT350 PLANE pointings	#
# by Jason Hessels, modified by P.Lazarus for PALFA 		#
#								#
# Modified for use with the NUPPI survey			#
# Will preprocess the files to be search at CC-IN2P3            #
#	Gregory Desvignes					#
#################################################################


from sys import stdout, argv, exit
from os import system, popen
import sys
import optparse
import os.path
import MySQLdb
import time
import glob
from tools import scan_path

# get configurations from config file
from NUPPI_config import *

def DBconnect(Host,DBname,Username,Password):
    # creates DB connection to extract pointing info from database
    connection = MySQLdb.connect(host=Host,db=DBname,user=Username,passwd=Password)
    cursor = connection.cursor()
    return cursor, connection


def grab_pointings2process():
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    # TODO
    #DBcursor.execute("SELECT obs_id, basefilename, pointing_name, numfiles, add_date FROM full_processing WHERE (proc_stat='requested') ORDER BY add_date" % institution)
    DBcursor.execute("SELECT p.obs_id, p.basefilename, p.numfiles, p.add_date FROM full_processing AS f LEFT JOIN processing AS p ON f.obs_id = p.obs_id WHERE (f.status='requested') ORDER BY p.add_date")
    query = DBcursor.fetchall()
    DBconn.close()
    observations = []
    for value in query:
	observations.append([value[0],value[1],value[2],value[3]])
    return observations

def check_free_stagging_space():
    """Return the free space size in the stagging area directory"""
    files = glob.glob(STAGGING_AREA)
    used_space = 0
    for file in files:
      used_space +=  os.path.getsize(file)

    # Check if we have more than 10 GB available
    if (MAX_STAGGING_SPACE_DISK - used_space > 10 * 1024 * 1024 * 1024):
        return True
    else: return False	



def check_free_resources():
    #Determines if any of the survey pointing jobs are queued.
    #If so it returns that the system is busy.
   
    global qflag
   
    (numjobs, numqueued) = jobs_running()
    
    newqflag = bool(numqueued)
    free_resources = (not qflag) and (not newqflag) and (numjobs < max_jobs)
    qflag = newqflag
    
    return free_resources
    
    
def create_batch_script((obs_id, basefilename, numfiles, add_date)):
    #create a batch script to be submitted to the queue
 
    scratch = '%s/%s/' % (working_dir, basefilename) # working directory names

    preprocess_error = "%s/error/%s.PREPROCESS_error_output" % (scratch, basefilename)

    batchfile = open("NUPPI_preprocess_for_%s.sh" % basefilename,"w")
    batchfile.write("#!/bin/bash\n")
    batchfile.write("#PBS -M %s\n" % email)
    batchfile.write("#PBS -m a\n")
    batchfile.write("#PBS -V\n")
    batchfile.write("#PBS -q BON\n")
    batchfile.write("#PBS -N %s\n" % basefilename )
    batchfile.write("#PBS -l %s\n" % node_req)
    batchfile.write("#PBS -o %s/logs/%s.out\n" % (pipeline_loc, basefilename) )
    batchfile.write("#PBS -e %s/logs/%s.err\n" % (pipeline_loc, basefilename) )
    if options.debug:
	batchfile.write("#PBS -j oe\n") # combine stdout and stderr output streams for debugging
    batchfile.write("\n")

    batchfile.write("""echo PROCESSING: `date` >> %s\n""" % log_file)
    batchfile.write("""echo -e "\t Starting processing of: %s" >> %s\n""" % (basefilename, log_file))
    batchfile.write("""echo -e "\t\t Using node: `uname -n`\n" >> %s\n""" % log_file)

    batchfile.write("uname -n\n")
    batchfile.write("date\n")

    batchfile.write("#make scratch directory to work in\n")
    batchfile.write("rm -rfd %s\n" % scratch)
    for dir in (scratch, scratch + "/error/"):
	batchfile.write("mkdir -m 777 -p %s\n" % dir) # everything happening on the node will take place in scratch 
    batchfile.write("echo line 1 - made and cleaned directories\n") 
    batchfile.write("date\n")
  
    for error_file in (preprocess_error):
	batchfile.write("uname -n >> %s\n" % error_file)
    
    batchfile.write("#move into scratch directory\n")
    batchfile.write("cd %s\n\n" % scratch)
    #batchfile.write("cp %s %s\n\n" % (os.path.join(scripts_loc, 'NUPPI_preprocess_script.py'), scratch) )	#copy over presto
    #batchfile.write("cp %s %s\n\n" % (os.path.join(scripts_loc, 'NUPPI_config.py'), scratch) )	#copy over presto
    #batchfile.write("echo line 2 - copied preprocess_script and config\n") 
    #batchfile.write("date\n")

    batchfile.write("#copy psrfits files\n")

    # Copy the numfiles fitsfiles
    batchfile.write("scp -p clairvaux:%s*/%s*.fits %s\n" % (DATADISK, basefilename, scratch ))
    batchfile.write("echo line 4 - remote copied files to scratch\n\n")
    batchfile.write("date\n")
    
    # merge NUPPI files
    batchfile.write("# run merge_psrfits\n")
    batchfile.write("%s -o merged %s* >> %s\n" % (merge_psrfits_prog, basefilename, preprocess_error) )
    batchfile.write("echo line 5 - merged\n\n") 
    batchfile.write("date\n")
    
    # clean the data
    # TODO

    # Copy the merged (and possibly cleaned) file
    batchfile.write("scp -p merged/*.fits clairvaux:%s \n" % (STAGGING_AREA) )

    # Cleanup
    batchfile.write("rmdir -rf %s\n" % (scratch) ) # Clean up

    batchfile.write("""echo -e "\t Finished processing of: %s" >> %s\n""" % (basefilename, log_file))
    batchfile.close()

    system("chmod 777 NUPPI_preprocess_for_%s.sh" % basefilename) 
    
    return "NUPPI_preprocess_for_%s.sh" % basefilename

    
def process_beam(observation):
    log_file_obj = open(log_file, 'a')
    log_file_obj.write("BACKGROUND SCRIPT: %s\n" % time.asctime())
    log_file_obj.write("\tProcessing observation file: %s\n" % observation[1])
    
    batchfile = create_batch_script(observation)
    log_file_obj.write("\t\tBatchfile created: %s\n" % batchfile)
    
    log_file_obj.flush()
    log_file_obj.close()
    
    #Submit the job to openPBS
    jobid = os.popen("qsub -l other=bon %s" % batchfile).readline()
    system("""echo -e "\t\tJob submitted to queue: job_id=`qstat -f | grep 'Job Id' | tail -n 1 | awk -F': ' '{print $2}'`\n" >> %s""" % log_file)

    #Change database to indicate the pointing has been submitted for processing and the current time
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    DBcursor.execute("UPDATE full_processing SET status='submitted to clairvaux', updated_at=NOW() WHERE obs_id=%d" % (observation[0]))
    DBconn.close()
   
    return jobid.strip()

def processing_table_summary():
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    DBcursor.execute("select status, count(*) as n from full_processing group by status order by status asc;")
    A = dict(DBcursor.fetchall())
    DBconn.close()
    
    keys = A.keys()
    keys.sort()
    summ = []
    for k in keys:
	summ.append('%s:%d' % (k, A[k]))
    return '; '.join(summ)
    

def jobs_running():
    """
    numjobs = jobs_running()
    
    Returns number of jobs currently running and the number of jobs currently queued.
    """
    
    numjobs = int(os.popen(r'qstat -f | grep -e "Job_Name" -e "Job_Owner" | sed -ne "/Job_Name = .*fb/{N;s/\n/ /g;s/  */ /g;/Job_Owner = $USER/p}" | wc -l').readline())    
    numqueued = int(os.popen(r'qstat -f | grep -e "Job_Name" -e "Job_Owner" -e "job_state" | sed -ne "/Job_Name = .*fb/{N;N;s/\n/ /g;s/  */ /g;/Job_Owner = $USER.*Q$/p}" | wc -l').readline())    
    return (numjobs, numqueued)


def check_stagging_area():
    """
    numbeams = beams_done()
    
    Returns number of finished beams
    with .infos file, waiting to be loaded.
    """
    
    Files = scan_path("%s/nuppi*fits"%STAGGING_AREA)
    if not Files:
        return
    basefilenames = Files.keys()
    basefilenames.sort()

    DBcursor, DBconn = DBconnect(host, database, usrname, pw)


    for basefilename in basefilenames:

	QUERY = "SELECT P.obs_id, F.status FROM processing as P LEFT JOIN full_processing AS F ON F.obs_id=P.obs_id WHERE P.basefilename='%s'"%basefilename
        DBcursor.execute(QUERY)
	result_query = [list(row) for row in DBcursor.fetchall()]

	obs_id, cur_status = result_query[0]

	# Has this file been previously marked as restored ?
	#QUERY = "SELECT status FROM full_processing WHERE obs_id=%d"%(obs_id)
	#DBcursor.execute(QUERY)
	#cur_status = DBcursor.fetchone()[0]

	if cur_status == "submitted to clairvaux":

	    # First get the size of the fileY
	    QUERY = "INSERT INTO raw_files (obs_id, path, filename, datasize) VALUES (%d, '%s', '%s', %ld);"%(obs_id, path, filename, datasize)
	    DBcursor.execute(QUERY)

	    # then, mark it as restored

	    QUERY = "UPDATE full_processing SET status='restored', updated_at=NOW() WHERE obs_id=%d" % (obs_id)
	    DBcursor.execute(QUERY)

    # Close the connexion
    DBconn.close()



def main():
    global qflag
    qflag = False # Initially assume no jobs are queued
    
    count = 0
    while (1):
	#Get a list of pointings that need to be searched
	observations = grab_pointings2process()
	#if not observations:
	#    time.sleep(30)
   
        # If pointings are requested, try submit them
	for observation in observations:
	    jobs, queued = jobs_running()
	    summ = processing_table_summary()

	    # Free ressources, we can submit the job
	    if check_free_resources() and check_free_stagging_space():
		jobid = process_beam(observation)
		action = 'Submitting job: %s  job_id: %d' % (jobid, observation[0])
	    else:
	        if check_free_resources():
		    print "Not enough space available on the stagging area"
		    
	    time.sleep(30)

	# Now, check the stagging area for available beams    
	check_stagging_area()

	time.sleep(30)
    
if __name__=='__main__':
    parser = optparse.OptionParser()
    parser.add_option('-d', '--debug', dest='debug', action='store_true', help='Turn debugging mode on. (Default: Debugging mode off', default=False)
    (options, sys.argv) = parser.parse_args()
    main()
