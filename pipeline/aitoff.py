import numpy as np
import numpy.ma as ma
from psr_constants import *
from ppgplot import *
import slalib

def aitoff(lon, lat):
    """
    aitoff(lon, lat):
        Given galactic position (longitude and latitude) in radians,
        return the Aitoff projection (0->180, -180->0)
    """
    half_lon = np.asarray(lon)/2.0
    alpha = np.sqrt(1. + np.cos(lat)*np.cos(half_lon))
    # Mask this array, or we'll get divide-by-zero errors
    alpha = ma.masked_where(alpha == 0.0, alpha)
    x = 2.0 * np.cos(lat) * np.sin(half_lon) / alpha
    y = np.sin(lat)/alpha
    return x*90,y*90

def arc_lon(lon):
    """
    arc_lon(lon):
        Draw an arc of constant longitude 'lon' in degrees,
	return x, y
    """  
    lon_rad = lon * DEGTORAD
    x, y = aitoff(lon_rad, np.arange(-np.pi/2., np.pi/2., 0.01))
    return x, y

    
def arc_lat(lat):
    """
    arc_lat(lat):
        Draw an arc of constant latitude 'latn' in degrees,
	return x, y
    """  
    lat_rad = lat * DEGTORAD
    x, y = aitoff(np.arange(-np.pi, np.pi, 0.01), lat_rad)
    return x, y


def arc_RA(ra):
    """
    arc_lat(lat):
        Draw an arc of constant RA 'latn' in degrees,
	return x, y
    """  
    ra_rad = ra * DEGTORAD
    L = []; B = []
    for dec_rad in np.arange(-np.pi/2, np.pi/2, 0.01):
        l,b =  slalib.sla_eqgal(ra_rad, dec_rad)
	L.append(l) ; B.append(b) 
    L = np.asarray(L) ; B = np.asarray(B)

    L[L>np.pi] -= 2*np.pi

    X, Y = aitoff(L, B)

    x1 = X[0]; y1 = Y[0]
    pgmove(x1, y1)
    for x,y in zip (X,Y):
        d = np.sqrt((x-x1)*(x-x1) + (y-y1)*(y-y1))
	if d < 10.:
	    pgdraw(x,y)
	pgmove(x,y)
	x1 = x; y1 = y


 
def arc_DEC(dec):
    dec_rad = dec* DEGTORAD
    L = []; B = []
    for ra_rad in np.arange(-np.pi, np.pi, 0.01):
        l,b =  slalib.sla_eqgal(ra_rad, dec_rad)
        L.append(l) ; B.append(b)
    L = np.asarray(L) ; B = np.asarray(B)

    L[L>np.pi] -= 2*np.pi

    X, Y = aitoff(L, B)

    x1 = X[0]; y1 = Y[0]
    pgmove(x1, y1)
    for x,y in zip (X,Y):
        d = np.sqrt((x-x1)*(x-x1) + (y-y1)*(y-y1))
	if d < 10.:
	    pgdraw(x,y)
	pgmove(x,y)
	x1 = x; y1 = y


def pltlegend():

    pgsch(0.7)

    # Draw legend on declination arcs: list is combination of hour angles and label angles
    #for ra,angle in [ [0,55], [3,-15], [6,-90], [9,-45], [12,-5], [15,60], [18,60], [21,60]]:
    for ra,angle in [ [0,55], [3,-15], [6,-90], [9,-45], [12,-5], [18,60], [21,60]]:
    #for ra,angle in [ [0,55], [3,-15], [6,-90], [9,-45], [12,-5], [15,60],  [21,60]]:
        l,b =  slalib.sla_eqgal(ra*360/24. * DEGTORAD, 35.0 * DEGTORAD) # origi was 10
	if l>np.pi: l -= 2*np.pi
	x, y = aitoff(l, b)
	if ra != 12: pgptxt(x, y, angle, 0.5, "%dhr"%ra)

    # Draw legend on declination arcs
    for deg in [-60., -30., 0., 30., 60.]:
        l,b =  slalib.sla_eqgal(260 * DEGTORAD, deg * DEGTORAD) # origi was 270
	if l>np.pi: l -= 2*np.pi
	x, y = aitoff(l, b)
	pgptxt(x, y, 60., 0., "%ddeg"%deg)
    pgsch(1.)


def pltframe(use_colors=True):

    # Draw the outer arcs
    for l in [-180., 180.]:
	x, y = arc_lon(l); pgline(x, y)

    # Draw the inner arcs
    #if use_colors:
    #    pgsci(5)
    #else:
    pgsci(15)
    for l in [-120., -60., 0., 60., 120.]:
	x, y = arc_lon(l); pgline(x, y)
    for l in [-60., -30., 0., 30., 60.]:
	x, y = arc_lat(l); pgline(x, y)
    pgsci(1)

    # Draw the RA arcs
    if use_colors:
        pgsci(5)
    else:
        pgsci(15)
    for ra in range(0, 360, 15):
	arc_RA(ra)
    # Draw the DEC arcs
    for dec in range(-80, 90, 10):
	arc_DEC(dec)
    pgsci(1)

    # Draw the legend
    pltlegend()

def plt_Arecibo():
    # 32 - 77
    pgsls(2)
    arc_DEC(38)
    arc_DEC(-1)
    pgsls(1)


if __name__ == '__main__':
    pgopen("/xw")
    pgsvp(0., 1., 0., 1.0)
    pgwnad(180., -180., -90., 90.)
    pltframe()
    pgend()

