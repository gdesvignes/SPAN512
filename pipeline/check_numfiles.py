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

  files = glob.glob("/survey_data/survey?/*.fits")

  QUERY = "SELECT basefilename,numfiles FROM processing WHERE numfiles != 8"
  #print QUERY
  DBcursor.execute(QUERY)
  basefilenames = DBcursor.fetchall()

  for data in basefilenames:
      print data
  
  DBconn.close()

if __name__ == '__main__':
  main()
