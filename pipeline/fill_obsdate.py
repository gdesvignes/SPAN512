#!/usr/bin/env python

import struct, sys
import numpy as np 
#from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *
from psrfits import *
import os.path
import glob
import slalib


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

  files = glob.glob("/survey_data/survey?/*1261_0001.fits")
  files.sort()

  files_list = []
  
  #print files

  for fullfilename in files:
      path, filename = os.path.split(fullfilename)
      pointing_name = filename.split("_")[2]

      print filename

      try:
          pfits = Psrfits(fullfilename)
      except:
          continue
      mjd = pfits.get_MJD()

      gregdate = slalib.sla_djcl(mjd)
      yr, month, day, fracday = gregdate[:4]
      h, m, s = MJDfrac_to_hms(fracday)

      QUERY = "UPDATE processing SET obs_date=\"%d-%d-%d %d:%d:%d\" where pointing_name='%s';"%(yr, month, day,h, m, s, pointing_name)
      print QUERY
      DBcursor.execute(QUERY)
  #result_query = [list(row) for row in DBcursor.fetchall()]
  #print result_query
  
  DBconn.close()

if __name__ == '__main__':
  main()
