#!/usr/bin/python
#################################################################
# Script to automate processing of the GBT350 PLANE pointings	#
# by Jason Hessels, modified by P.Lazarus for PALFA 		#
#								#
# Modified for use with the NUPPI survey				#
#	Gregory Desvignes					#
#################################################################


from sys import stdout, argv, exit
from os import system, popen
from monitor_cluster import * 
from mysendmail import sendMail
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
    DBcursor.execute("SELECT obs_id, basefilename, pointing_name, numfiles, add_date FROM processing WHERE (proc_stat='o' AND institution='%s') ORDER BY obs_id" % institution)
    #DBcursor.execute("SELECT obs_id, basefilename, pointing_name, numfiles, add_date FROM processing WHERE (proc_stat='o' AND institution='%s') ORDER BY add_date" % institution)
    query = DBcursor.fetchall()
    DBconn.close()
    observations = []
    for value in query:
	observations.append([value[0],value[1],value[2],value[3],value[4]])
    return observations


def check_free_resources(PBS):
    #Determines if any of the survey pointing jobs are queued.
    #If so it returns that the system is busy.
   
    global qflag
   
    (numjobs, numqueued) = PBS.jobs_running()

    num_free_cpus = PBS.get_num_free_cpus()
    
    newqflag = bool(numqueued)
    new_num_free_cpus = bool(num_free_cpus)
    free_resources = (not qflag) and (not newqflag) and (numjobs < max_jobs) and new_num_free_cpus
    qflag = newqflag
    
    return free_resources
    
    
def create_batch_script((obs_id, basefilename, pointing_name, numfiles, add_date)):
    #create a batch script to be submitted to the queue
 
    scratch = '%s/%s/' % (working_dir, basefilename ) # working directory names

    merge_psrfits_error = "%s/error/%s.MERGE_PSRFITS_error_output" % (scratch, basefilename)
    presto_error = "%s/error/%s.PRESTO_error_output" % (scratch, basefilename)
    getResults_error = "%s/error/%s.getResults_error_output" % (scratch, basefilename) 

    batchfile = open("NUPPI_batch_for_%s.sh" % basefilename,"w")
    batchfile.write("#!/bin/bash\n")
    batchfile.write("#PBS -M %s\n" % email)
    batchfile.write("#PBS -m a\n")
    batchfile.write("#PBS -V\n")
    batchfile.write("#PBS -q BON\n")
    batchfile.write("#PBS -N %s\n" % basefilename )
    batchfile.write("#PBS -l %s\n" % node_req)
    batchfile.write("#PBS -o %s/logs/%s.out\n" % (pipeline_loc,basefilename) )
    batchfile.write("#PBS -e %s/logs/%s.err\n" % (pipeline_loc,basefilename) )
    if options.debug:
	batchfile.write("#PBS -j oe\n") # combine stdout and stderr output streams for debugging
    batchfile.write("\n")
    batchfile.write("echo line 0 - got past the headers\n")

    batchfile.write("""echo PROCESSING: `date` >> %s\n""" % log_file)
    batchfile.write("""echo -e "\t Starting processing of: %s" >> %s\n""" % (basefilename, log_file))
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
  
    for error_file in (merge_psrfits_error, presto_error, getResults_error):
	batchfile.write("uname -n >> %s\n" % error_file)
    
    batchfile.write("#move into scratch directory\n")
    batchfile.write("cd %s\n\n" % scratch)
    batchfile.write("ls -ltr\n")
 
    batchfile.write("\n#copy over birdie list, zaplist and presto to both working directories\n")
    #batchfile.write("cp %s %s\n" % (os.path.join(misc_loc, 'NUPPI.birds'), scratch) )		#copy over birdie list
    batchfile.write("cp %s %s\n" % (os.path.join(scripts_loc, 'NUPPI.zaplist'), scratch) )		#copy over zaplist
    batchfile.write("cp %s %s\n" % (os.path.join(scripts_loc, 'NUPPI_presto_search.py'), scratch) )	#copy over presto
    batchfile.write("cp %s %s\n" % (os.path.join(scripts_loc, 'NUPPI_quick_presto_search.py'), scratch) )	#copy over presto
    batchfile.write("cp %s %s\n" % (os.path.join(scripts_loc, 'NUPPI_config.py'), scratch) )	#copy over presto
    batchfile.write("echo line 2 - copied birds, zaplist and presto to \n") 
    batchfile.write("date\n")

    # TODO : survey specific mode !!

    batchfile.write("#copy psrfits files\n")

    # Copy the numfiles fitsfiles
    batchfile.write("scp -p clairvaux:%s?/%s* %s\n" % (DATADISK, basefilename, scratch) ) # Don't know exactly in which the data are
    batchfile.write("echo line 4 - remote copied files to scratch\n\n")
    batchfile.write("date\n")
    
    # merge NUPPI files
    batchfile.write("# run merge_psrfits\n")
    batchfile.write("%s %s*_0001.fits >> %s\n" % (merge_psrfits_prog, basefilename, merge_psrfits_error) )
    batchfile.write("echo line 5 - merged\n\n") 
    batchfile.write("date\n")
    

    batchfile.write("#create results directories\n")
    batchfile.write("outdir=%s/%s\n"%(final_dir, basefilename))
    batchfile.write("mkdir -p -m775 ${outdir}\n\n")


    # Remove the first 8 seconds of the observation (January, 20th 2012)
    #batchfile.write("fitsdelrow %s/%s_0001.fits[1] 1 %d\n"% (scratch, basefilename, rows2del))


    batchfile.write("#now call the pipelines on beams in the background and wait\n")
    if options.debug:
	batchfile.write("python %sNUPPI_quick_presto_search.py %s_000?.fits %s 2>> %s &\n" % (scripts_loc, basefilename, scratch, presto_error) ) # Removed redirect to /dev/null for DEBUG purposes 
	#batchfile.write("python %sNUPPI_presto_search.py %s_000?.fits %s 2>> %s &\n" % (scripts_loc, basefilename, scratch, presto_error) ) # Removed redirect to /dev/null for DEBUG purposes 
    else:
	batchfile.write("python %sNUPPI_quick_presto_search.py %s_000?.fits %s >> %s 2>> %s &\n" % (scripts_loc, basefilename, scratch, presto_error, presto_error) ) 
	#batchfile.write("python %sNUPPI_presto_search.py %s_000?.fits %s >> %s 2>> %s &\n" % (scripts_loc, basefilename, scratch, presto_error, presto_error) ) 
    batchfile.write("wait\n") # wait for child processes to finish before continuing
    batchfile.write("echo line 6 - presto is done\n\n") 
    batchfile.write("date\n")

    # get results from the observation
    batchfile.write("python %s --dir='%s' 2>> %s\n" % (os.path.join(scripts_loc, 'NUPPI_getResults.py'), scratch, getResults_error))

    batchfile.write("rm -rfd %s\n" % scratch) #clean up scratch directory

    batchfile.write("""echo PROCESSING: `date` >> %s\n""" % log_file)
    batchfile.write("""echo -e "\t Finished processing of: %s" >> %s\n""" % (basefilename, log_file))
    batchfile.write("""echo -e "\t\t Using node: `uname -n`\n" >> %s\n""" % log_file)
    
    batchfile.close()
    system("chmod 777 NUPPI_batch_for_%s.sh" % basefilename) 
    
    return "NUPPI_batch_for_%s.sh" % basefilename

    
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
    
def beam_done(basefilename):
    """
    numbeams = beams_done()
    
    Returns True if the beam has finished 
    with .infos file, waiting to be loaded.
    """
    
    info_path = os.path.join(dbinfo_dir, "%s_hdr.infos"%(basefilename))
    if os.path.exists(info_path):
        return True
    else: return False	


def beams_done():
    """
    numbeams = beams_done()
    
    Returns number of finished beams
    with .infos file, waiting to be loaded.
    """
    
    beams = glob.glob(os.path.join(dbinfo_dir, "*.infos"))
    return len(beams)


def update_DB(PBS):
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    DBcursor.execute("SELECT basefilename FROM processing WHERE (proc_stat='r') ORDER BY proc_date")
    query = DBcursor.fetchall()
    for basefilenames in query:
        basefilename = basefilenames[0]

        # First mark finished beams with an 'a' for 'analyzed'	 
	if beam_done(basefilename):
	    QUERY = "UPDATE processing SET proc_stat='f' WHERE basefilename='%s';\n"%(basefilename)
	    #print QUERY
	    DBcursor.execute(QUERY)
	    continue


	# Then look for beams which are still 'r' but with errors
	if PBS.had_errors(basefilename) or (not PBS.is_running(basefilename)):
	    QUERY = "UPDATE processing SET proc_stat='e' WHERE basefilename='%s';\n"%(basefilename)
	    #print QUERY
	    DBcursor.execute(QUERY)

	    # Send Email in case of errors
	    title = "Survey Pulsar - Error with %s"%(basefilename)
	    attached_files = []
	    sendMail(RECIPIENTS_short, title, PBS.get_errors(basefilename), attached_files)

    DBconn.close()
        


def main():
    global qflag
    qflag = False # Initially assume no jobs are queued

    count = 0
    while (1):
        
	# Torque class 
        PBS = PBSManager()

	#Get a list of pointings that need to be searched
	observations = grab_pointings2process()
	update_DB(PBS)
	if not observations:
	    jobs, queued = PBS.jobs_running()
	    summ = processing_table_summary()
	    beams = 'Beams finished: %d ' % beams_done()
	    status = 'No jobs'
	    action = 'Waiting'
   
	#for observation in observations:
	else:
	    jobs, queued = PBS.jobs_running()
	    summ = processing_table_summary()
	    beams = 'Beams finished: %d ' % beams_done()
	    if check_free_resources(PBS):
		status = 'Free'
		jobid = process_beam(observations[0])
		action = 'Submitting job: %s' % jobid
	    else:
		status = 'Busy'
		if queued:
		    status = 'No nodes'
		action = 'Waiting'

	print '- '*40
	info = "%d/%d: %s | %s | %s | %s | %s " % (jobs, max_jobs, status, summ, action, beams, time.ctime())
	print info

	PBS.close()
	time.sleep(280)
    
if __name__=='__main__':
    parser = optparse.OptionParser()
    parser.add_option('-d', '--debug', dest='debug', action='store_true', help='Turn debugging mode on. (Default: Debugging mode off', default=False)
    (options, sys.argv) = parser.parse_args()
    main()
