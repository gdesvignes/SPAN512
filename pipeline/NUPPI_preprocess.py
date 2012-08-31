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
    DBcursor.execute("SELECT obs_id, basefilename, pointing_name, numfiles, add_date FROM processing WHERE (proc_stat='o' AND institution='%s') ORDER BY add_date" % institution)
    query = DBcursor.fetchall()
    DBconn.close()
    observations = []
    for value in query:
	observations.append([value[0],value[1],value[2],value[3],value[4]])
    return observations


def check_free_resources():
    #Determines if any of the survey pointing jobs are queued.
    #If so it returns that the system is busy.
   
    global qflag
   
    (numjobs, numqueued) = jobs_running()
    
    newqflag = bool(numqueued)
    free_resources = (not qflag) and (not newqflag) and (numjobs < max_jobs)
    qflag = newqflag
    
    return free_resources
    
    
def create_batch_script((obs_id, basefilename, pointing_name, numfiles, add_date)):
    #create a batch script to be submitted to the queue
 
    scratch = '%s/%s/' % (working_dir, obs_id) # working directory names

    files = []
    filterfiles = ""
    
    preprocess_error = "%s/error/%s.PREPROCESS_error_output" % (scratch, filename)

    for i in range(0, int(numfiles), 1): #filenames. both the existing names and the new names after copying
    	files.append(filename)
    batchfile = open("NUPPI_batch_for_%s.sh" % filename,"w")
    batchfile.write("#!/bin/bash\n")
    batchfile.write("#PBS -M %s\n" % email)
    batchfile.write("#PBS -m a\n")
    batchfile.write("#PBS -V\n")
    batchfile.write("#PBS -q NUPPI\n")
    batchfile.write("#PBS -N %s\n" % filename )
    batchfile.write("#PBS -l %s\n" % node_req)
    batchfile.write("#PBS -o %s/logs/%s.out\n" % (pipeline_loc,filename) )
    batchfile.write("#PBS -e %s/logs/%s.err\n" % (pipeline_loc,filename) )
    if options.debug:
	batchfile.write("#PBS -j oe\n") # combine stdout and stderr output streams for debugging
    batchfile.write("\n")
    batchfile.write("echo line 0 - got past the headers\n")

    batchfile.write("""echo PROCESSING: `date` >> %s\n""" % log_file)
    batchfile.write("""echo -e "\t Starting processing of: %s" >> %s\n""" % (filename, log_file))
    batchfile.write("""echo -e "\t\t Using node: `uname -n`\n" >> %s\n""" % log_file)

    batchfile.write("uname -n\n")
    batchfile.write("date\n")

    batchfile.write("time_before=`date +%s`\n")
    batchfile.write("date_started=`date +%y%m%d`\n")

    batchfile.write("#make scratch directory to work in\n")
    batchfile.write("rm -rfd %s\n" % scratch)
    for dir in (scratch, scratch + "/error/"):
	batchfile.write("mkdir -m 777 -p %s\n" % dir) # everything happening on the node will take place in scratch 
    batchfile.write("echo line 1 - made and cleaned directories\n") 
    batchfile.write("date\n")
  
    for error_file in (fil_error, presto_error, getResults_error):
	batchfile.write("uname -n >> %s\n" % error_file)
    
    batchfile.write("#move into scratch directory\n")
    batchfile.write("cd %s\n\n" % scratch)
    batchfile.write("cp %s %s\n\n" % (os.path.join(scripts_loc, 'NUPPI_preprocess_script.py'), scratch) )	#copy over presto
    batchfile.write("cp %s %s\n\n" % (os.path.join(scripts_loc, 'NUPPI_config.py'), scratch) )	#copy over presto
    batchfile.write("echo line 2 - copied preprocess_script and config\n") 
    batchfile.write("date\n")

    # TODO : survey specific mode !!

    batchfile.write("#copy psrfits files\n")

    # Copy the numfiles fitsfiles
    batchfile.write("scp -p clairvaux:%s %s\n" % (os.path.join(path, file), os.path.join(scratch, file) ))
    batchfile.write("echo line 4 - remote copied files to scratch\n\n")
    batchfile.write("date\n")
    
    # merge NUPPI files
    batchfile.write("# run merge_psrfits\n")
    batchfile.write("%s %s* >> %s\n" % (merge_psrfits_prog, basefilename, fil_error) )
    batchfile.write("echo line 5 - merged\n\n") 
    batchfile.write("date\n")
    
    # clean the data
    # TODO



    batchfile.write("""echo PROCESSING: `date` >> %s\n""" % log_file)
    batchfile.write("""echo -e "\t Finished processing of: %s" >> %s\n""" % (filename, log_file))
    batchfile.write("""echo -e "\t\t Using node: `uname -n`\n" >> %s\n""" % log_file)
    
    batchfile.close()
    system("chmod 777 NUPPI_preprocess_for_%s.sh" % filename) 
    
    return "NUPPI_preprocess_for_%s.sh" % filename

    
def process_beam(observation):
    log_file_obj = open(log_file, 'a')
    log_file_obj.write("BACKGROUND SCRIPT: %s\n" % time.asctime())
    log_file_obj.write("\tProcessing observation file: %s\n" % observation[1])
    
    batchfile = create_batch_script(observation)
    log_file_obj.write("\t\tBatchfile created: %s\n" % batchfile)
    
    log_file_obj.flush()
    log_file_obj.close()
    
    #Submit the job to openPBS
    jobid = os.popen("qsub -l other=nbpp %s" % batchfile).readline()
    system("""echo -e "\t\tJob submitted to queue: job_id=`qstat -f | grep 'Job Id' | tail -n 1 | awk -F': ' '{print $2}'`\n" >> %s""" % log_file)

    #Change database to indicate the pointing has been submitted for processing and the current time
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    DBcursor.execute("UPDATE processing SET proc_stat='r', proc_date=NOW() WHERE basefilename = '%s' AND institution = '%s'" % (observation[1], institution) )
    DBconn.close()
   
    return jobid.strip()

def processing_table_summary():
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    DBcursor.execute("select proc_stat, count(*) as n from processing group by proc_stat order by proc_stat asc;")
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


def beams_done():
    """
    numbeams = beams_done()
    
    Returns number of finished beams
    with .infos file, waiting to be loaded.
    """
    
    beams = glob.glob(os.path.join(dbinfo_dir, "*.infos"))
    return len(beams)


def main():
    global qflag
    qflag = False # Initially assume no jobs are queued
    
    count = 0
    while (1):
	#Get a list of pointings that need to be searched
	observations = grab_pointings2process()
	if not observations:
	    """
	    jobs, queued = jobs_running()
	    summ = processing_table_summary()
	    beams = 'Beams finished: %d ' % beams_done()
	    status = 'No jobs'
	    action = 'Waiting'
	    print '- '*40
	    info = "%d/%d: %s | %s | %s | %s | %s " % (jobs, max_jobs, status, summ, action, beams, time.ctime())
	    print info
	    """
	    time.sleep(30)
   
	for observation in observations:
	    jobs, queued = jobs_running()
	    summ = processing_table_summary()
	    beams = 'Beams finished: %d ' % beams_done()
	    if check_free_resources():
		status = 'Free'
		jobid = process_beam(observation)
		action = 'Submitting job: %s' % jobid
	    else:
		status = 'Busy'
		if queued:
		    status = 'No nodes'
		action = 'Waiting'
	    print '- '*40
	    info = "%d/%d: %s | %s | %s | %s | %s " % (jobs, max_jobs, status, summ, action, beams, time.ctime())
	    print info
	    time.sleep(30)
    
if __name__=='__main__':
    parser = optparse.OptionParser()
    parser.add_option('-d', '--debug', dest='debug', action='store_true', help='Turn debugging mode on. (Default: Debugging mode off', default=False)
    (options, sys.argv) = parser.parse_args()
    main()
