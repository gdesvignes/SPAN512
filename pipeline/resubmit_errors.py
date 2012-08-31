#!/usr/bin/env python

import struct, sys
import numpy as np 
import datetime
#from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *


full_usage = """
usage : pltasc.py [options] .asc files

  [-h, --help]        : Display this help
  [-r, --rot]         : Rotate the profile


"""
usage = "usage: %prog [options] files"


def main():
  parser = OptionParser(usage)

  #parser.add_option("-d", "--days", type="float", dest="nb_days", default=1.0,
  #                         help="Minimum number of days for plannified observations")

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SBON512")
  DBconn = db.conn
  DBcursor = db.cursor


  QUERY = "SELECT obs_id FROM processing WHERE proc_stat='e'"
  DBcursor.execute(QUERY)
  result_query = [list(row) for row in DBcursor.fetchall()]


  for obs_id in result_query:
      QUERY = "UPDATE processing set proc_stat='o' WHERE obs_id=%s"%(obs_id[0])
      #print QUERY
      DBcursor.execute(QUERY)
  
  DBconn.close()

if __name__ == '__main__':
  main()
