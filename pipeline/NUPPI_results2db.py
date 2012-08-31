###################################################
# Takes *.infos files created by the pipeline
#    and updates database		
# with information contained in them.						#
###################################################

import sys, os, random
import os.path
import glob, traceback, optparse, shutil, tarfile
from elementtree import ElementTree as ET

import MySQLdb


# get configurations from config file
from NUPPI_config import *

debug = 0

def DBconnect(Host,DBname,Username,Password):
    # creates DB connection to extract pointing info from database
    connection = MySQLdb.connect(host=Host,db=DBname,user=Username,passwd=Password)
    cursor = connection.cursor()
    return cursor, connection		



class load_infos():

    def __init__(self, obs_id, DBcursor, DBconn):

        self.obs_id = obs_id
	self.DBcursor = DBcursor
	self.DBconn = DBconn

	# Load header infos into DB
	self.header_id = self.load_hdr_infos()

	# Load cands infos into DB
	self.min_cand, self.max_cand = self.load_cands_infos()

	# Load sps infos into DB 
	self.min_sp, self.max_sp = self.load_sps_infos()

    def load_hdr_infos(self):

	# Put header infos in dict
	hdr_info_file = os.path.join(dbinfo_dir, "%s_hdr.infos"%self.obs_id)
	infos = ET.parse(hdr_info_file)
	header = {}
	for element in infos.getiterator():
	    header[element.tag] = element.text

	# Get obs_id
        query = 'SELECT obs_id FROM processing WHERE pointing_name=%s;'%(header['source_name'])
        self.DBcursor.execute(query)
        header['obs_id'] = str(self.DBcursor.fetchone()[0])
	#print query, header['obs_id']

        # Header infos
	#key = ['obs_id', 'basefilename', 'sample_time', 'observation_time', 'num_samples', 'center_freq', 'timestamp_mjd', 'channel_bandwidth', 'num_channels', 'num_ifs', 'source_name', 'sum_id', 'project_id', 'observers', 'right_ascension', 'declination', 'galactic_longitude', 'galactic_latitude', 'ra_deg', 'dec_deg']
	key = ['obs_id', 'basefilename', 'sample_time', 'observation_time', 'num_samples', 'center_freq', 'timestamp_mjd', 'channel_bandwidth', 'num_channels', 'num_ifs', 'source_name', 'sum_id', 'project_id', 'observers', 'right_ascension', 'declination', 'galactic_longitude', 'galactic_latitude', 'ra_deg', 'dec_deg', 'n_bits']
        query = 'INSERT IGNORE INTO headers (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]))
	if DEBUG: print query
        self.DBcursor.execute(query)

	# Get header_id
        query = 'SELECT LAST_INSERT_ID() FROM headers;'
        self.DBcursor.execute(query)
        header_id = self.DBcursor.fetchone()[0]

	# Now update diagnostics
        header['version_id'] = "1"
        header['header_id'] = str(header_id)
	key = ['header_id', 'rfi_percentage', 'version_id']
	query = 'INSERT IGNORE INTO diagnostics (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]) )
	if DEBUG: print query
        self.DBcursor.execute(query)

	return header_id


    def load_cands_infos(self):
	"""
	Load cands infos
	"""

	# Put header infos in dict
	cand_ids = []
	cand_info_files = glob.glob("%s/*ACCEL*.infos"%os.path.join(dbinfo_dir, self.obs_id))
	for ii,cand_info_file in enumerate(cand_info_files):
  	    infos = ET.parse(cand_info_file)
	    header = {}
	    header['header_id'] = "%d"%self.header_id
	    header['cand_num'] = "%d"%(ii+1)
	    header['version_id'] = "1"
	    header['pdm_plot_type_id'] = "1"
	    for element in infos.getiterator():
	        header[element.tag] = element.text
	    	

	    key = ['header_id', 'cand_num', 'freq_topo', 'freq_bary', 'period_topo', 'period_bary', 'freq_topo_dot', 'freq_bary_dot', 'dm', 'snr', 'coherent_power', 'incoherent_power', 'num_hits', 'num_harmonics', 'version_id', 'presto_sigma' ]
	    cmd = 'INSERT IGNORE INTO pdm_candidates (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]))
	    #print cmd
	    self.DBcursor.execute(cmd)
	    cmd = 'SELECT LAST_INSERT_ID() FROM pdm_candidates;'
	    self.DBcursor.execute(cmd)

	    cand_ids.append(self.DBcursor.fetchone()[0])
	    header['pdm_cand_id'] = "%d"%cand_ids[ii]


	    #insert into pdm_plot_pointers table
	    key = ['pdm_cand_id', 'pdm_plot_type_id', 'path', 'ps_filename']
	    cmd = 'INSERT IGNORE INTO pdm_plot_pointers (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]))
	    #print cmd
	    self.DBcursor.execute(cmd)

	    #insert into pdm_candidate_plots table
	    key = ['pdm_cand_id', 'pdm_plot_type_id', 'png_filename']
	    cmd = 'INSERT INTO pdm_candidate_plots (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]))
	    #print cmd
	    self.DBcursor.execute(cmd)

	if cand_info_files:
	    return cand_ids[0], cand_ids[-1]
	else:
	    return 0,0


    def load_sps_infos(self):
	"""
	Load sps infos
	"""

	# Put header infos in dict
	sp_ids = []
	sp_info_files = glob.glob("%s/*singlepulse.infos"%os.path.join(dbinfo_dir, self.obs_id))
	for ii,sp_info_file in enumerate(sp_info_files):
  	    infos = ET.parse(sp_info_file)
	    header = {}
	    header['header_id'] = "%d"%self.header_id
	    header['sp_num'] = "%d"%(ii+1)
	    header['version_id'] = "1"
	    for element in infos.getiterator():
	        header[element.tag] = element.text
	    	

	    key = ['header_id', 'sp_num', 'sp_plot_type_id', 'ps_filename', 'png_filename' ]
	    cmd = 'INSERT IGNORE INTO sp_plots (%s) VALUES (%s);' % (','.join(key), ','.join([header[k] for k in key]))
	    #print cmd
	    self.DBcursor.execute(cmd)
	    cmd = 'SELECT LAST_INSERT_ID() FROM sp_plots;'
	    self.DBcursor.execute(cmd)

	    sp_ids.append(self.DBcursor.fetchone()[0])

	if sp_info_files:
	    return sp_ids[0], sp_ids[-1]
	else:
	    return 0,0


    def get_db_stats(self):
        return self.header_id, self.min_cand, self.max_cand, self.min_sp, self.max_sp

def loadInfoBeam(infobeam, DBcursor, DBconn):
    """
    Loads info files
    Marks beam status as 'd' in processing table.
    """
    
    print "Loading File: %s" % infobeam

    # Load header and candidates
    Infos = load_infos(infobeam, DBcursor, DBconn)
    header_id, min_cand, max_cand, min_sp, max_sp = Infos.get_db_stats()
    print "header_id: %d" % header_id
    print "pdm_cand_ids: %d-%d (%d cands)" % (min_cand, max_cand, max_cand - (min_cand-1))
    #print "sp_cand_ids: %d-%d (%d sps)" % (min_sp, max_sp, max_sp - (min_sp-1))
   
    markBeam('d', header_id, DBcursor, DBconn)
    return max_cand - (min_cand-1)


def markBeam(status, header_id, DBcursor, DBconn):
    """
    Mark the beam associated with the given header_id
    with status provided.
    """

    DBcursor.execute("SELECT basefilename FROM headers WHERE header_id = %s" % header_id) # header_id is of type str
    basefilename = DBcursor.fetchone()[0]
    cmd = "UPDATE processing SET proc_stat='%s', results_date=NOW() WHERE basefilename='%s';\n" % (status, basefilename)
    DBcursor.execute(cmd)
    

def print_report(att, comp, cands):
    """
    Print report given number of directories
    attempted, completed and number of cands
    loaded.
    """
    
    just = len(str(max((att, comp, cands))))
    
    print "\n" + "-"*30
    print "%s info files attempted" % str(att).rjust(just)
    print "%s info files loaded" % str(comp).rjust(just)
    print "%s candidates loaded" % str(cands).rjust(just)
    print "-"*30 + "\n"

def main():
    DBcursor, DBconn = DBconnect(host, database, usrname, pw)
    
    files = glob.glob(os.path.join(dbinfo_dir, "*_hdr.infos"))
    random.shuffle(files)
    succ_count = 0 # beam succesfully loaded
    cand_count = 0

    # Create done directory
    try:
        os.mkdir("%s/done"%dbinfo_dir)
    except OSError:
        #print "Assume %s/done is already created" % dbinfo_dir
        pass
        
    # Create done directory
    try:
        os.mkdir("%s/done/cands"%dbinfo_dir)
    except OSError:
        #print "Assume %s/done/cands is already created" % dbinfo_dir
        pass

    for file in files:
        loaded = False
	print "\nLoading beam %d of %d" % (succ_count+1, len(files))
	try:
            cand_count += loadInfoBeam(file.rstrip("_hdr.infos"), DBcursor, DBconn)
	    loaded = True
	except:
	    print "Error:", sys.exc_info()
	    pass

        
	#try:
	if loaded:
	    cmd = "mv %s %s/done/." % (os.path.join(dbinfo_dir,file), dbinfo_dir)
	    #print cmd
	    os.system(cmd)

	    # Tar the cands .infos files and move them
	    cands_infos_dir = file.replace("_hdr.infos","")
	    cands_infos_tarball = tarfile.open("%s.tar.gz" % os.path.join(dbinfo_dir,cands_infos_dir), "w:gz")
	    cands_infos_tarball.add(os.path.join(dbinfo_dir,cands_infos_dir))
	    cands_infos_tarball.close()
	    cmd = "mv %s.tar.gz %s/done/cands/." % (os.path.join(dbinfo_dir,cands_infos_dir), dbinfo_dir)
	    #print cmd
	    os.system(cmd)

	    # Remove the directory containing the cands and sp .infos files
	    shutil.rmtree(os.path.join(dbinfo_dir,cands_infos_dir))

	    succ_count += 1
	#except:
	#    print "skipping..."
	#    pass

    print_report(len(files), succ_count, cand_count)
    
    DBconn.close()
    
parser = optparse.OptionParser()
parser.add_option('-d', '--debug', dest='debug', action='store_true', default=False, help="Turn on debugging mode. (Default: Debugging off).")
(options, sys.argv) = parser.parse_args()

if __name__ == '__main__':							
    main()
