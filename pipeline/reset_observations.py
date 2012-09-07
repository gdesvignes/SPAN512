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

  parser.add_option("-d", "--days", type="float", dest="nb_days", default=1.0,
                          help="Minimum number of days for plannified observations")

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SPAN512")
  DBconn = db.conn
  DBcursor = db.cursor


  QUERY = "SELECT pointing_name, grid_id, planned_date from processing WHERE proc_stat='p' AND DATEDIFF(NOW(), planned_date) > %f"%(opts.nb_days)
  DBcursor.execute(QUERY)
  result_query = [list(row) for row in DBcursor.fetchall()]

  print " Pointing  grid_id   planned_date"
  print " --------------------------------"
  print "",

  for obs in result_query:
      print "%s %7d %s"%(obs[0], obs[1], obs[2]),
      print "  Do you want to reset proc_stat? [y/n] ",
      ch = sys.stdin.readline()
      ch = ch.strip()
      if ch=='y':
          QUERY = "UPDATE processing set proc_stat=NULL WHERE pointing_name='%s'"%(obs[0])
	  DBcursor.execute(QUERY)
  
  DBconn.close()

if __name__ == '__main__':
  main()
