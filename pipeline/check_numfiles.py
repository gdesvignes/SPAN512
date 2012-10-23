#!/usr/bin/env python

import struct, sys
import numpy as np 
#from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *
import os.path
import glob


full_usage = """
usage : check_numfiles.py [options] 

  [-h, --help]        : Display this help


"""
usage = "usage: %prog [options]"


def main():
  parser = OptionParser(usage)

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SPAN512")
  DBconn = db.conn
  DBcursor = db.cursor

  #files = glob.glob("/survey_data/survey?/*.fits")

  QUERY = "SELECT basefilename,numfiles FROM processing WHERE numfiles != 8"
  #print QUERY
  DBcursor.execute(QUERY)
  basefilenames = DBcursor.fetchall()

  for data in basefilenames:
      print data,
      files = glob.glob("/survey_data/survey?/%s*.fits"%(data[0]))
      num_files = len(files)
      for ifile in files:
          if os.path.getsize(ifile) < 1024*1024:
	      num_files = numfiles-1
      print "Found %d files"%num_files,

      if num_files != data[1]:
          print "Fix it ? [y/n] ",
	  ch = sys.stdin.readline()
	  ch = ch.strip()
	  if ch=='y':
              QUERY = "UPDATE processing set numfiles=%d WHERE basefilename='%s'"%(len(files), data[0])
              DBcursor.execute(QUERY)
      print
  
  DBconn.close()

if __name__ == '__main__':
  main()
