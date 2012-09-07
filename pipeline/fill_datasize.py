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
usage : pltasc.py [options] .asc files

  [-h, --help]        : Display this help
  [-r, --rot]         : Rotate the profile


"""
usage = "usage: %prog [options] files"


def main():
  parser = OptionParser(usage)

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SPAN512")
  DBconn = db.conn
  DBcursor = db.cursor

  files = glob.glob("/survey_data/survey?/*.fits")
  files.sort()
  #print files

  for fullfilename in files:
      #QUERY = "UPDATE NRT_grid set is_SPAN2=true WHERE right_ascension='%s' AND declination='%s'"%(ra, dec)
      #print QUERY
      #DBcursor.execute(QUERY)
      path, filename = os.path.split(fullfilename)

      QUERY = "SELECT obs_id FROM processing WHERE basefilename='%s'"%filename[:28]
      #print QUERY
      DBcursor.execute(QUERY)
      obs_id = DBcursor.fetchone()[0]
      #print obs_id

      datasize = os.path.getsize(fullfilename)


      QUERY = "INSERT INTO raw_files (obs_id, path, filename, datasize) VALUES (%d, '%s', '%s', %ld);"%(obs_id, path, filename, datasize)
      #print QUERY
      DBcursor.execute(QUERY)
  #result_query = [list(row) for row in DBcursor.fetchall()]
  #print result_query
  
  DBconn.close()

if __name__ == '__main__':
  main()
