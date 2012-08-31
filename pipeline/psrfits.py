import sys
import pyfits

class Psrfits():

    def __init__(self, filename):
        " Comment"    
	self.hdu = pyfits.open(filename)
	#print self.hdu.info()

        # Header
	self.hdr = self.hdu[0].header

	if self.get_mode() == 'PSR':
	    self.polyco_h = self.hdu[self.hdu.index_of("POLYCO")].header

	    self.subint_head = self.hdu[self.hdu.index_of("SUBINT")].header
	    #self.subint_data = self.hdu[self.hdu.index_of("SUBINT")].data

	elif self.get_mode() == 'SEARCH':
	    self.subint_head = self.hdu[self.hdu.index_of("SUBINT")].header
	    #self.subint_data = self.hdu[self.hdu.index_of("SUBINT")].data
	
    def close(self):
        del self.subint_head
	#del self.subint_data
	del self.hdr
	try: del self.polyco_h
	except: pass
        self.hdu.close()

    def get_telescope(self):
        return self.hdr['TELESCOP']

    def get_project(self):
        return self.hdr['PROJID']

    def get_mode(self):
        return self.hdr['OBS_MODE']

    def get_observer(self):
        return self.hdr['OBSERVER']

    def get_srcname(self):
        return self.hdr['SRC_NAME']

    def get_DM(self):
        try:
	    dm = self.hdr['CHAN_DM']
	except:
	    pass

	try:
	    dm = self.subint_head['DM']
	except:
	    pass

        return dm 

    #def get_tint(self):
    #	return sum(self.subint_data.field('tsubint'))

    def get_obsfreq(self):
        return self.hdr['OBSFREQ']

    def get_RA(self):
        return self.hdr['RA']

    def get_DEC(self):
        return self.hdr['DEC']

    def get_MJD(self):
        return self.hdr['MJD']

    def get_BW(self):
        return self.hdr['OBSBW']

    def get_nchan(self):
        return self.hdr['OBSNCHAN']

    def get_chanBW(self):
        return self.hdr['OBSBW']/self.hdr['OBSNCHAN']

    def get_MJD(self):
        return self.hdr['STT_IMJD']+self.hdr['STT_SMJD']/86400.

    def get_tsamp(self):
        return self.subint_head['TBIN']

    def get_npol(self):
        return self.subint_head['NPOL']

    def get_nsamp(self):
        return self.subint_head['NSBLK'] * self.subint_head['NAXIS2']

    def get_obslen(self):
        return self.subint_head['NSBLK'] * self.subint_head['NAXIS2'] * self.subint_head['TBIN']

    def get_nbits(self):
        return self.subint_head['NBITS']

    def list_values(self):
        print "TELESCOP :", self.get_telescope()
        print " SRCNAME :", self.get_srcname()
        print "   PRJID :", self.get_project()
        print "OBSERVER :", self.get_observer()
	print "      DM :", self.get_DM()
	print " OBSFREQ :", self.get_obsfreq()
	print "OBSNCHAN :", self.get_nchan()
	print "   OBSBW :", self.get_BW()
	print "      RA :", self.get_RA()
	print "     DEC :", self.get_DEC()
	print "     MJD :", self.get_MJD()
	print "   TSAMP :", self.get_tsamp()
	print "    NPOL :", self.get_npol()
	print "   NSAMP :", self.get_nsamp()
	print "  OBSLEN :", self.get_obslen()
	print "   NBITS :", self.get_nbits()

if __name__ == '__main__':

    pfits = Psrfits(sys.argv[1])
    pfits.list_values()

