#!/usr/bin/env python

import struct, sys
import numpy as np 
#from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
import psr_utils, coordconv
from psr_constants import *


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

  QUERY = "SELECT header_id, right_ascension, declination FROM headers"
  #QUERY = "SHOW tables"
  DBcursor.execute(QUERY)
  result_query = [list(row) for row in DBcursor.fetchall()]
  print result_query

  for header in result_query:

      # Convert in degrees
      ra_deg = psr_utils.ra_to_rad(header[1]) * RADTODEG
      dec_deg = psr_utils.dec_to_rad(header[2]) * RADTODEG

      # Convert in galactic coord
      galactic_longitude, galactic_latitude = coordconv.eqdeg_to_galdeg(ra_deg, dec_deg)

      QUERY = "UPDATE headers SET n_bits=4, galactic_longitude=%f, galactic_latitude=%f, ra_deg=%f, dec_deg=%f WHERE header_id=%d;\n"%(galactic_longitude, galactic_latitude, ra_deg, dec_deg, header[0])
      print QUERY
      DBcursor.execute(QUERY)

  
  DBconn.close()

if __name__ == '__main__':
  main()
