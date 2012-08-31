#!/usr/bin/env python

import struct, sys
import numpy as np 
from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *
from pylab import *
from ppgplot import *


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

  QUERY = "SELECT H.right_ascension, H.declination, H.galactic_longitude, H.galactic_latitude FROM headers as H LEFT JOIN processing as P ON P.obs_id=H.obs_id"
  QUERY = "SELECT H.galactic_longitude, H.galactic_latitude FROM headers as H LEFT JOIN processing as P ON P.obs_id=H.obs_id"
  #print QUERY
  DBcursor.execute(QUERY)
  result_query = [list(row) for row in DBcursor.fetchall()]
  print result_query
  """

  results_query = DBcursor.fetchall()
  ra = []; dec = []; l = []; b = []
  for row in results_query:
      ra.append(row[0])
      dec.append(row[1])
      l.append(row[2])
      b.append(row[3])

  print l
  #QUERY = "SELECT * FROM headers LIMIT 10"
  #QUERY = "SHOW tables"
  #DBcursor.execute(QUERY)
  #result_query = [list(row) for row in DBcursor.fetchall()]
  #print result_query
  """

  ar = subplot(111, projection="aitoff")
  plot(*zip(*result_query), marker='o', color='r', ls='')
  title("SPAN512")
  grid(True)
  print ar.get_xlim(),ar.get_ylim()
  print ar.set_xlim((-180,180)),ar.set_ylim((-90,90))
  print ar.get_xlim(),ar.get_ylim()
  show()
  
  DBconn.close()

if __name__ == '__main__':
  main()
