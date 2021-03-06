#!/usr/bin/env python

import struct, sys
import numpy as np 
from ppgplot import *
from math import *
from optparse import OptionParser
from database import *
from psr_utils import *
from ppgplot import *
from  aitoff import *
from psr_constants import *
import slalib


full_usage = """
usage : plot_survey.py [options]

  [-h, --help]        : Display this help
  [-a, --all]         : Display all pointings on the map


"""
usage = "usage: %prog [options]"



def main():
  parser = OptionParser(usage)

  parser.add_option("-a", "--all", action="store_true", dest="all_pointings",
		    default=False, help="Display all pointings on the map")

  (opts, args) = parser.parse_args()			
	
  # Scan files
  db = Database(db="local-SBON512")
  DBconn = db.conn
  DBcursor = db.cursor

  #QUERY = "SELECT H.right_ascension, H.declination, H.galactic_longitude, H.galactic_latitude FROM headers as H LEFT JOIN processing as P ON P.obs_id=H.obs_id"
  if opts.all_pointings:
      QUERY = "SELECT G.right_ascension, G.declination FROM NRT_grid as G RIGHT JOIN processing as H ON H.grid_id=G.grid_id WHERE (H.proc_stat='o' OR H.proc_stat='f' OR H.proc_stat='r' OR H.proc_stat='e' OR H.proc_stat='d' OR H.proc_stat='w')"
  else:
      QUERY = "SELECT G.right_ascension, G.declination FROM NRT_grid as G RIGHT JOIN processing as H ON H.grid_id=G.grid_id WHERE G.is_SPAN=TRUE AND (H.proc_stat='o' OR H.proc_stat='f' OR H.proc_stat='r' OR H.proc_stat='e' OR H.proc_stat='d' OR H.proc_stat='w')"
  DBcursor.execute(QUERY)

  results_query = DBcursor.fetchall()
  l = []; b = []
  for row in results_query:
      ra = ra_to_rad(row[0])
      dec = dec_to_rad(row[1])

      lon, lat = slalib.sla_eqgal(ra, dec)
      if lon > np.pi: lon-= 2*np.pi
      if lon < -np.pi: lon+= 2*np.pi
      l.append(lon)
      b.append(lat)


  pgopen("/xw")
  #pgopen("plot.ps/cps")
  pgsvp(0., 1., 0.15, .85)
  #pgwnad(180., -180., -90., 90.)
  #pgswin(180., -180., -60., 60.)
  pgswin(180., -10., -60., 60.)
  pltframe()

  print "Number of observed beams:", len(l)
  pgmtxt('BL', 2., 0., 0., "Number of observed beams: %d"%len(l))

  # Display the pointings
  x, y = aitoff(np.asarray(l), np.asarray(b))
  #pgpt(x*90, y*90, 21)
  pgsch(0.7)
  pgpt(x, y, 17)
  #for i,j,k,l in zip(l,b,x*90, y*90):
      #print i,j,k,l

  # 1st zone
  xlim = np.array([72,150,150,72,72]) * DEGTORAD
  ylim = np.array([3.5,3.5,5.,5.,3.5])  * DEGTORAD
  x, y = aitoff(xlim, ylim) 
  pgline(x,y)
  # 2nd zone
  xlim = np.array([79,150,150,79,79]) * DEGTORAD
  ylim = np.array([-3.5,-3.5,-5.,-5.,-3.5])  * DEGTORAD
  x, y = aitoff(xlim, ylim) 
  pgline(x,y)

  pgend()

  DBconn.close()

if __name__ == '__main__':
  main()
