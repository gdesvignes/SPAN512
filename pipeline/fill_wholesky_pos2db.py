#!/usr/bin/env python

import struct, sys
import numpy as np 
#from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *
from psr_constants import *


full_usage = """
usage : pltasc.py [options] .asc files

  [-h, --help]        : Display this help
  [-r, --rot]         : Rotate the profile


"""
usage = "usage: %prog [options] files"

def read_scan(filename):
    ra, dec = np.loadtxt(filename, usecols=(3,5), unpack=True, dtype=np.string0)
    return zip(ra, dec)
    #print len(np.unique(dec)) 


def main():
  parser = OptionParser(usage)

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SBON512")
  DBconn = db.conn
  DBcursor = db.cursor

  positions = read_scan(args[0])

  for ra, dec in (positions):
      QUERY = "INSERT IGNORE INTO NRT_grid (right_ascension, declination, ra_deg, dec_deg) VALUES ('%s', '%s', %f, %f)"%(ra, dec, ra_to_rad(ra)*RADTODEG, dec_to_rad(dec)*RADTODEG)
      #print QUERY
      DBcursor.execute(QUERY)

  #QUERY = "SELECT * FROM headers LIMIT 10"
  #QUERY = "SHOW tables"
  #DBcursor.execute(QUERY)
  #result_query = [list(row) for row in DBcursor.fetchall()]
  #print result_query
  
  DBconn.close()

if __name__ == '__main__':
  main()
