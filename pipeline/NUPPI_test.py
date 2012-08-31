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
#from monitor_cluster import jobs_running
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


def main():
    
    DBcursor, DBconn = DBconnect(distant_host, database, usrname, pw)
    DBcursor.execute("SELECT obs_id, basefilename, pointing_name, numfiles, add_date FROM processing")
    query = DBcursor.fetchall()
    DBconn.close()


if __name__=='__main__':
    parser = optparse.OptionParser()
    parser.add_option('-d', '--debug', dest='debug', action='store_true', help='Turn debugging mode on. (Default: Debugging mode off', default=False)
    (options, sys.argv) = parser.parse_args()
    main()
