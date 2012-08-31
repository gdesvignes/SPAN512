import os, os.path, sys, glob, optparse
import psr_utils 
import coordconv 
import tarfile
from psr_constants import *
from NUPPI_config import *
from elementtree.SimpleXMLWriter import XMLWriter
from psrfits import Psrfits

class Obs:

    def __init__(self, dir):
	self.obs = {}

	self.dir = dir

	# SPECIFIC to the NUPPI survey !!
	print 'Looking for ', os.path.join(self.dir, 'nuppi_?????_*_0001.fits')
	obsfiles = glob.glob(os.path.join(self.dir, 'nuppi_?????_*_0001.fits')) # Assume this structure for merged filename
	obsfiles.sort()
	self.pfits = Psrfits(obsfiles[0])
	if obsfiles:
	    obsfile = os.path.split(obsfiles[0])[1]
	    #self.obs["obs_id"] = obsfile.split('_')[2]
	    self.obs["scan_id"] = obsfile.split('_')[3]
	    obsfilename = obsfile.rstrip('.fits')
	    self.obs["obsfile"] = obsfilename
	    self.obs["basefilename"] = '_'.join(obsfilename.split('_')[0:4])
	    self.obs["format"] = "psrfits"

	    self.basefilename = self.obs["basefilename"]

    def getHDR(self):
        """
	Get HDR infos
	"""


	self.obs["sample_time"] = self.pfits.get_tsamp()
	self.obs["observation_time"] = self.pfits.get_obslen()
	self.obs["timestamp_mjd"] = self.pfits.get_MJD()
	self.obs["num_samples"] = self.pfits.get_nsamp()
	self.obs["center_freq"] = self.pfits.get_obsfreq()
	self.obs["channel_bandwidth"] = self.pfits.get_chanBW() 
	self.obs["num_channels"] = self.pfits.get_nchan()
	self.obs["num_ifs"] = self.pfits.get_npol()
	self.obs["right_ascension"] = self.pfits.get_RA()
	self.obs["declination"] = self.pfits.get_DEC()
	self.obs["n_bits"] = self.pfits.get_nbits()

	# Get file size
	#self.obs["file_size"] = str(os.path.getsize(os.path.join(self.dir, self.obs["obsfile_fb"])))
	#self.obs["data_size"] = header_lines['Data size (bytes)']

	# Convert in degrees
	self.obs["ra_deg"] = psr_utils.ra_to_rad(self.obs["right_ascension"]) * RADTODEG
	self.obs["dec_deg"] = psr_utils.dec_to_rad(self.obs["declination"]) * RADTODEG

	# Convert in galactic coord
	self.obs["galactic_longitude"], self.obs["galactic_latitude"] = coordconv.eqdeg_to_galdeg(self.obs["ra_deg"], self.obs["dec_deg"])

	# Convert into string for NUPPI_results2DB mysql
        self.obs["right_ascension"] = "'%s'"%self.obs["right_ascension"]
        self.obs["declination"] = "'%s'"%self.obs["declination"]
        self.obs["basefilename"] = "'%s'"%self.obs["basefilename"]
	self.obs["obsfile"] = "'%s'"%self.obs["obsfile"]
	self.obs["project_id"] = "'%s'"%self.pfits.get_project()    
	self.obs["observers"] = "'%s'"%self.pfits.get_observer() 
	self.obs["source_name"] = "'%s'"%self.pfits.get_srcname()

	# sum_id
        if int(self.obs['num_ifs']) > 1:
	    self.obs["sum_id"] = '0'
	elif int(self.obs['num_ifs']) == 1:
	    self.obs["sum_id"] = '1'


    def getRFI(self):
        """
        Get RFI infos
        """

        rfifind_out_file = os.path.join(self.dir, self.obs["obsfile"] + '_rfifind.out')
	rfi_line = os.popen('grep "Number of  bad   intervals" %s' % rfifind_out_file).readline()
	self.obs["rfi_percentage"] = rfi_line.split('(')[1].split('%')[0]

  
    def getCANDS(self):
        """
        Get Cands informations
	"""
	accelcand_file = os.path.join(self.dir, self.obs["obsfile"] + '.accelcands')
	bestprof_files = glob.glob(os.path.join(self.dir, self.basefilename + '*bestprof'))

	self.obs["cands"] = [] 

	for i,bestprof_file in enumerate(bestprof_files):
	  
	    self.obs["cands"].append({})

	    # Search into the .accelcand file for more informations
	    accelcand = bestprof_file.rstrip(".pfd.bestprof").replace("Z", "ACCEL_").replace("_ACCEL_Cand_", ":")
	    accelcand = accelcand.split("/")[-1]
	    accelcand_line= os.popen('grep %s %s' % (accelcand, accelcand_file)).readline()

	    # Get cand filename
	    self.obs["cands"][i]["ps_filename"] = "'%s'"%bestprof_file.split("/")[-1].replace(".bestprof", ".ps.gz")
	    self.obs["cands"][i]["png_filename"] = "'%s'"%bestprof_file.split("/")[-1].replace(".bestprof", ".png")

	    #print accelcand_line
	    file_candnum, discovery_dm, self.obs["cands"][i]["snr"],\
	    self.obs["cands"][i]["presto_sigma"], self.obs["cands"][i]["num_harmonics"],\
	    self.obs["cands"][i]["incoherent_power"], self.obs["cands"][i]["coherent_power"],\
	    period, r, z, self.obs["cands"][i]["num_hits"] = accelcand_line.split()[0:11]

	    # Read the bestprof informations
	    self.obs["cands"][i]["dm"] = os.popen("grep '# Best DM' %s" % bestprof_file).readline().split('=')[1].split()[0]
	    p_topo = float(os.popen("grep '# P_topo (ms)' %s" % bestprof_file).readline().split('=')[1].split()[0])/1000.
	    pdot_topo = float(os.popen("grep '# P._topo (s/s)' %s" % bestprof_file).readline().split('=')[1].split()[0])
	    p_bary = float(os.popen("grep '# P_bary (ms)' %s" % bestprof_file).readline().split('=')[1].split()[0])/1000
	    pdot_bary = float(os.popen("grep '# P._bary (s/s)' %s" % bestprof_file).readline().split('=')[1].split()[0])

	    # now compute f and fdot for bary and topo
	    self.obs["cands"][i]["freq_topo"] = str(1.0/p_topo)
	    self.obs["cands"][i]["freq_topo_dot"] = str(-pdot_topo/(p_topo**2))
	    self.obs["cands"][i]["freq_bary"] = str(1.0/p_bary)
	    self.obs["cands"][i]["freq_bary_dot"] = str(-pdot_bary/(p_bary**2))
    
	    # convert p and pdot to strings
	    self.obs["cands"][i]["period_topo"] = str(p_topo)
	    self.obs["cands"][i]["period_topo_dot"] = str(pdot_topo)
	    self.obs["cands"][i]["period_bary"] = str(p_bary)
	    self.obs["cands"][i]["period_bary_dot"] = str(pdot_bary)

	    self.obs["cands"][i]["path"] = "'%s'"%os.path.join(final_dir, self.basefilename) 

    def getSPS(self):
        """
        Get Single Pulses informations
	"""
	sp_plot_files = glob.glob(os.path.join(self.dir, self.basefilename + '*singlepulse.png'))

	self.obs["sps"] = [] 

	for i,sp_plot_file in enumerate(sp_plot_files):
	  
	    self.obs["sps"].append({})

	    # Get cand filename
	    self.obs["sps"][i]["ps_filename"] = "'%s'"%sp_plot_file.split("/")[-1].replace(".png", ".ps.gz")
	    self.obs["sps"][i]["png_filename"] = "'%s'"%sp_plot_file.split("/")[-1]

	    # sp_plot_type_id = 1  for all DMs
	    # sp_plot_type_id = 2  for low DM part
	    # sp_plot_type_id = 3  for medium DM part
	    # sp_plot_type_id = 4  for high DM part
	    if 'DMs0-120' in sp_plot_file:
	        self.obs["sps"][i]["sp_plot_type_id"] = '2'
	    elif 'DMs90-340' in sp_plot_file:
	        self.obs["sps"][i]["sp_plot_type_id"] = '3'
	    elif 'DMs280-1000' in sp_plot_file:
	        self.obs["sps"][i]["sp_plot_type_id"] = '4'
	    else:
	        self.obs["sps"][i]["sp_plot_type_id"] = '1'

    def write_hdr(self):
        """
	Write header infos in a xml file
	"""
	self.beam_info_dir = os.path.join(dbinfo_dir, self.basefilename)
        infos_file = os.path.join(dbinfo_dir, self.basefilename + '_hdr.infos')

	# First try to create directory if not already done
	try:    os.mkdir(dbinfo_dir)
	except:    pass

	try:    os.mkdir(self.beam_info_dir)
	except:    pass

        w = XMLWriter(infos_file)
	xml = w.start("hdr")

	for key, value in self.obs.items():
	    w.element(key, str(value))
	w.close(xml)	


    def write_cands(self):
        """
	Write cand infos in a xml file
	"""
	# Loop over all candidates
	for i,cand in enumerate(self.obs["cands"]):
	    # Open a xml file per candidate
	    cand_file = os.path.join(self.beam_info_dir, "%s"%cand['png_filename'].replace('.pfd.png','.infos').replace("'",""))
	    w = XMLWriter(cand_file)
	    xml = w.start("cand")
	    
	    for key, value in cand.items():
	        w.element(key, value)
	    w.close(xml)		

    def write_sps(self):
        """
	Write singlepulse infos in a xml file
	"""
	# Loop over all candidates
	for i,sp in enumerate(self.obs["sps"]):
	    # Open a xml file per candidate
	    sp_file = os.path.join(self.beam_info_dir, "%s"%sp['png_filename'].replace('.png','.infos').replace("'",""))
	    w = XMLWriter(sp_file)
	    xml = w.start("sp")
	    
	    for key, value in sp.items():
	        w.element(key, value)
	    w.close(xml)		


    def tar_results(self):
        """
	Tar all results and move the .tar.gz in the results directory
	"""
	report_file = glob.glob(os.path.join(self.dir, '*report'))
	ps_files = glob.glob(os.path.join(self.dir, '*ps.gz'))
	png_files = glob.glob(os.path.join(self.dir, '*png'))
        bestprof_files = glob.glob(os.path.join(self.dir, '*bestprof*'))
	output_files = glob.glob(os.path.join(self.dir, '*.out'))
	inf_files = glob.glob(os.path.join(self.dir, '*.inf'))
	accelcand_file = glob.glob(os.path.join(self.dir, '*accelcands'))
	accel_files = glob.glob(os.path.join(self.dir, '*ACCEL*'))
	singlepulse_files = glob.glob(os.path.join(self.dir, '*.singlepulse'))
	pfd_files = glob.glob(os.path.join(self.dir, '*.pfd'))

	files_to_tar = report_file + ps_files + png_files + bestprof_files + output_files + inf_files + accelcand_file + accel_files + singlepulse_files + pfd_files

	# Create the tarball
	cwd = os.getcwd()
	os.chdir(self.dir)
	results_tarball = tarfile.open("%s.tar.bz2" % self.basefilename, "w:bz2")
	for file in files_to_tar:
	    #results_tarball.add(file)
	    results_tarball.add(os.path.split(file)[1])
	results_tarball.close()

	# Move the tarball to the results directiry
	try:    os.mkdir(os.path.join(final_dir, self.basefilename))
	except:    pass
	os.system("mv %s.tar.bz2 %s" % (self.basefilename, os.path.join(final_dir, self.basefilename)))
	os.system("cp -p *.png *.ps.gz %s" % (os.path.join(final_dir, self.basefilename)))
	os.chdir(cwd)

    def show(self):
	for key, value in self.obs.items():
	    print "'%24s' :"%key, value



def main():
    parser = optparse.OptionParser()

    parser.add_option('-d', '--dir', dest='dir', help='Will process directory dir.', default='')
    (opts, args) = parser.parse_args()

    obs = Obs(opts.dir)

    obs.getHDR()
    obs.getRFI()
    obs.write_hdr()

    obs.getCANDS()
    obs.write_cands()

    obs.getSPS()
    obs.write_sps()

    obs.tar_results()

if __name__ == '__main__':
    main()
