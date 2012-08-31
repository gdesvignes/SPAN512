#########################################################################
# NBPP_updateVersion.py						#
#									#
# Generates current version_number, adds new entry to the versions	#
# table in the database.						#
#									#
# Patrick Lazarus, May 29th 2007					#
#########################################################################

import os, os.path, sys, glob
import MySQLdb
from NBPP_config import *


def DBconnect(Host,DBname,Username,Password):
    # creates DB connection to extract pointing info from database
    connection = MySQLdb.connect(host=Host,db=DBname,user=Username,passwd=Password)
    cursor = connection.cursor()
    return cursor, connection		

def getCurrentVersion():
    """
    returns version_number by combining current version
    numbers from sigproc, PRESTO and the NBPP pipeline
    """

    sigproc = os.popen("%s -version | grep version" % filterbank_prog).readline().split()[-1]
    presto = os.popen("svn info $PRESTO | grep Revision").readline().split()[-1]
    pipeline = os.popen("svn info /homes/borgii/alfa/svn/workingcopy_PL/NBPP | grep Revision").readline().split()[-1]

    return "%s-%s-%s" % (sigproc, presto, pipeline)

def updateVersionsTable():
    curr_version = getCurrentVersion()
    
    # First get the most recent entry.
    DBcursor.execute("SELECT version_number FROM versions WHERE institution='%s' and pipeline='PRESTO' ORDER BY version_id DESC LIMIT 1" % institution)
    record = DBcursor.fetchone()
    if not record or record[0] != curr_version:
	DBcursor.execute("INSERT INTO versions SET institution='%s', pipeline='PRESTO', version_number='%s', added_date=NOW()" % (institution, curr_version))
	print 'added new entry'
    DBcursor.execute("SELECT max(version_id) FROM versions WHERE institution='%s' and pipeline='PRESTO'" % institution)
    return DBcursor.fetchone()[0]
   
def main():
    global DBcursor, DBconn
    DBcursor, DBconn = DBconnect(host, database_v2, usrname, pw)

    curr_version = updateVersionsTable()

    DBconn.close()

if __name__ == '__main__':
    main()
    

