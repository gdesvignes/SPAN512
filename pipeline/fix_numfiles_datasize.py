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
  db = Database(db="local-SBON512")
  DBconn = db.conn
  DBcursor = db.cursor

  files = glob.glob("/survey_data/survey?/*.fits")
  files.sort()
  #print files
  files2 = [fi[-43:-15] for fi in files]
  files = list(set(files2))
  files.sort()
  print files

  #"""
  for basename in files:
      #QUERY = "UPDATE NRT_grid set is_SPAN2=true WHERE right_ascension='%s' AND declination='%s'"%(ra, dec)

      QUERY = "SELECT obs_id FROM processing WHERE basefilename='%s'"%basename
      #print QUERY
      DBcursor.execute(QUERY)
      try:
          obs_id = DBcursor.fetchone()[0]
      except:
          print basename
	  raise "Error "
      #print obs_id

      allfiles = glob.glob("/survey_data/survey?/%s*.fits"%basename)
      allfiles.sort()

      QUERY = "UPDATE processing SET numfiles=%d"%len(allfiles)
      print QUERY
      #DBcursor.execute(QUERY)

      for ifile in allfiles:
          datasize = os.path.getsize(ifile)
	  path, filename = os.path.split(ifile)


          QUERY = "INSERT INTO raw_files (obs_id, path, filename, datasize) VALUES (%d, '%s', '%s', %ld);"%(obs_id, path, filename, datasize)
          print QUERY
          #DBcursor.execute(QUERY)
  #"""    
  #result_query = [list(row) for row in DBcursor.fetchall()]
  #print result_query
  
  DBconn.close()

if __name__ == '__main__':
  main()
