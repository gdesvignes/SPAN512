import glob, sys, os, tempfile
import os.path
import MySQLdb, shutil
from NBPP_config import *

PROG = "/home/desvignes/src/autopsr/bin/autos2.exe"
tmpfile = "/tmp2/lkj.ascii" 

def DBconnect(Host,DBname,Username,Password):
    connection = MySQLdb.connect(host=Host,db=DBname,user=Username,passwd=Password)
    cursor = connection.cursor()
    return cursor, connection



def apply_likelyhood():
    """
    """
    # First connect to DB
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)

    # Result file
    resultfile = "/home/desvignes/nbpp.likelyhood"

    # Scan all directories
    directory = glob.glob(final_dir+"/????????")

    skipped_missing = []
    skipped_toobig = []

    for ii,dir in enumerate(directory):
        os.chdir(dir)

	beam = dir.split("/")[-1]

	# Check if archive is really here
	if not os.path.isfile("%s/%s.tar.bz2"%(dir,beam)):
	    skipped_missing.append(dir)
	    continue

	# Check size of the archive if > 50MB
	if os.path.getsize("%s/%s.tar.bz2"%(dir,beam)) > 60*1024*1024:
	    skipped_toobig.append(dir)
	    continue

        # Create temp dir to put PFDs
        tmpdir = tempfile.mkdtemp(suffix="_tmp", prefix=beam, dir="/tmp2")
        cmd = "tar xfj %s.tar.bz2 -C %s"%(beam,tmpdir)
	os.system(cmd)

        pfdfiles = glob.glob("%s/*.pfd"%tmpdir)

        print '%d/%d : Loading %d files in beam %s'%(ii,len(directory),len(pfdfiles), beam)

	# Loop over all PFDs in this beam
        for pfdfile in pfdfiles:
            cmd = "%s -f %s -ostd > %s"%(PROG, pfdfile, tmpfile)
	    os.system(cmd)

	    cmd = "cat %s >> %s"%(tmpfile, resultfile)
	    os.system(cmd)

	    # Retrieve the pdm_Cand_id
	    cmd = "SELECT pdm_cand_id from pdm_candidate_plots WHERE png_filename='%s.png'"%pfdfile.split("/")[-1]
	    DBcursor.execute(cmd)
	    try:
	        pdm_cand_id = DBcursor.fetchone()[0]
	    except:
	        print "Error inserting candidate %s in %s"%(pfdfile,beam)
	        continue


	    # Update the myqsl entry of the DB
	    pfi = open(tmpfile, 'r')
	    line = pfi.readline()
	    lkj1, lkj2, lkj3 = line.split()[-3:]
	    cmd = "UPDATE pdm_candidates SET lkj1=%f, lkj2=%f, lkj3=%f WHERE pdm_cand_id=%s;" % (float(lkj1), float(lkj2), float(lkj3), pdm_cand_id)
	    DBcursor.execute(cmd)

        shutil.rmtree(tmpdir)

    # Finisehd, close the connection to the DB	
    DBconn.close() 

    for dir in skipped_toobig:
        print "Skipped %s because of size %dMB"%(dir,os.path.getsize(dir)/1024/1024)
    for dir in skipped_missing:
        print "Skipped %s because of missing archive"%(dir)
    print "Skipped %d beams"%(len(skipped_toobig)+len(skipped_missing))	

	
def main():

    apply_likelyhood()


if __name__ == '__main__':
    main()


