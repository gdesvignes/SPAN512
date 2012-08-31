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

  QUERY = "SELECT obs_id FROM processing WHERE (proc_stat='o' OR proc_stat='f' OR proc_stat='r' OR proc_stat='e' OR proc_stat='d' OR proc_stat='w')"
  DBcursor.execute(QUERY)
  obs_ids = [list(row) for row in DBcursor.fetchall()]
  print obs_ids

  for obs_id in obs_ids:

      QUERY = "INSERT INTO full_processing (obs_id, proc_stat) VALUES (%d, 'o');"%(obs_id[0])
      print QUERY
      DBcursor.execute(QUERY)
  
  DBconn.close()

if __name__ == '__main__':
  main()
