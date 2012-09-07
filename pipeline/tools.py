import os, glob
import slalib
from database import *
from mysendmail import sendMail
from subprocess import Popen, PIPE 
from NUPPI_config import *
from time import sleep
from psrfits import Psrfits
from psr_utils import *
from psr_constants import *
from numpy import cos

def unique(seq):
    # Not order preserving     
    Set = set(seq) 
    return list(Set)

def check_space_size(disk):

    stats = os.statvfs(disk)
    return stats.f_bsize * stats.f_bavail

def check_space_size_wait(disk):
    old_size = check_space_size(disk)
    while 1:
	sleep(5)
	new_size = check_space_size(disk)
	#print new_size
	if new_size < old_size:
	   #print '\rFiles are being transfered',
	   old_size = new_size 
	elif new_size == old_size:
	    break
    return new_size


def scan_path(path):
    """
    Check for new files, waiting for unfinished data transfer
    return the list of basefilename with number of frequency subbands
    """
    wait = True
    # While there is new file or size is changing, wait !
    while wait:
        previous_size = []
	wait = False

	# Scan Dir
	files = glob.glob(path)
	files.sort()
	#if DEBUG: print "Scanning %s: %s"%(path, files)

	# If there is no files, exit immediatelly and return nothing
	if not files:
	    return None 

	# Record size
	for ii, file in enumerate(files):
	    previous_size.append( os.path.getsize(file) )

	# Sleep
	sleep(5) #Increased due to the usage of new switch

	# Check again
	files2 = glob.glob(path)
	files2.sort()

	# If new files appeared, wait !
	if len(files) != len(files2):
	    wait = True
	    continue

	# If size changed, wait !
	for ii, file in enumerate(files2):
	    if previous_size[ii] != os.path.getsize(file):
		wait = True
	# New !!        
	sleep(5)

    # Return unique basefilenames	
    #files3 = []
    Files = {}
    for ii, filename in enumerate(files2):
        #print filename
	if os.path.getsize(filename) > 0:
	    #files3.append(os.path.split(filename[:-15])[1]) # Remove the last part of the filename and the path
	    key = os.path.split(filename[:-15])[1]
	    if Files.has_key(key):
	        Files[key] += 1
	    else:
	        Files[key] = 1
    #files3 = unique(files3)	

    # Count the number of files
    #nb_files = []
    #for filename in files3:
        #print filename+"*fits"
       # nb_files.append(len(glob.glob(os.path.join(LINKDISK, filename)+"*fits")))
        
    return Files	





def check_new_obs(disk):    
    """Return a list of new observations found in 'disk'
    Only take the first psrfits file for earch observation
    """

    #basefilenames, nbfiles = scan_path(os.path.join(disk, "nuppi*_0001.fits"))
    Files = scan_path(os.path.join(disk, "nuppi*_0001.fits"))
    if DEBUG: print "Found %s"%(Files.keys())
    if not Files:
        return None, None	

    db = Database(db=DB_NAME)
    DBconn = db.conn
    DBcursor = db.cursor

    new_files = []
    new_files_nb = []
    # Will sort the files who are not already in the DB in stat o:observed, a:analyzing or r:with results, f:processing finished, e:error
    basefilenames = Files.keys()
    basefilenames.sort()
    for basefilename in basefilenames:
    #for basefilename, file_nb in zip(basefilenames, nbfiles):
        sfilename = os.path.split(basefilename)[1]
	QUERY = "SELECT * FROM processing WHERE basefilename = '%s' AND (proc_stat='o' OR proc_stat='f' OR proc_stat='r' OR proc_stat='e' OR proc_stat='d')"%sfilename
	DBcursor.execute(QUERY)
	result_query = [list(row) for row in DBcursor.fetchall()]
	if DEBUG: print QUERY, result_query
	if not result_query:
	    new_files.append(sfilename)
	    new_files_nb.append(Files[basefilename])

    DBconn.close()	
    return new_files, new_files_nb

def get_disk_label(disk):
    """Return the disk label of a mounted disk""" 
    df = Popen("df -h", shell=True, stdout=PIPE)
    log = df.communicate()[0]
    log_lines = log.splitlines()

    devicename = None
    for log_line in log_lines:
        if disk in log_line:
	    devicename = log_line.split()[0]

    if not devicename: return None
	    
    e2label = Popen("/sbin/e2label %s"%devicename, shell=True, stdout=PIPE)
    label = e2label.communicate()[0]
    label_lines = label.splitlines()
    label = label_lines[0]

    return label 
    

def get_disk_label_nfs(disk):
    """Return the disk label of a mounted disk""" 
    df = Popen("df -h", shell=True, stdout=PIPE)
    log = df.communicate()[0]
    log_lines = log.splitlines()

    devicename = None
    for log_line in log_lines:
        if disk in log_line:
	    devicename = log_line.split()[0]

    if not devicename: return None
    devicename = devicename.split(":")[1]

    # Find the disk over ssh
    df = Popen("ssh -XY root@spacetree \"df -h\"", shell=True, stdout=PIPE)
    log = df.communicate()[0]
    log_lines = log.splitlines()

    device_nfs = None
    for log_line in log_lines:
        if devicename in log_line:
	    device_nfs = log_line.split()[0]

    if not device_nfs: return None

	    
    e2label = Popen("ssh -XY root@spacetree \"/sbin/e2label %s\""%device_nfs, shell=True, stdout=PIPE)
    label = e2label.communicate()[0]
    label_lines = label.splitlines()
    label = label_lines[0]

    return label 

def get_grid_id(basefilename):
    """Return grid_id of the NRT_grid table
    from the position of a psrfits file
    """

    # Get RA and DEC in degrees from psrfits file
    first_file = glob.glob("%s*fits"%os.path.join(LINKDISK, basefilename))[0]
    pfits = Psrfits(first_file)
    ra_deg = ra_to_rad(pfits.get_RA()) * RADTODEG
    dec_deg = dec_to_rad(pfits.get_DEC()) * RADTODEG
    pfits.close()

    # Connect to the DB
    db = Database(db=DB_NAME)
    DBconn = db.conn
    DBcursor = db.cursor

    QUERY = "SELECT grid_id FROM NRT_grid WHERE ABS(ra_deg-%f)<%f AND ABS(dec_deg-%f) < %f"%(ra_deg, NRT_RA_DEG/2./cos(dec_deg*DEGTORAD), dec_deg, NRT_DEC_DEG/2)
    if DEBUG: print QUERY
    DBcursor.execute(QUERY)
    result_query = [list(row) for row in DBcursor.fetchall()]

    # No observation was planned, do an INSERT
    #print ra_deg, dec_deg, result_query
    return result_query[0][0]

def get_MJD_obs(basefilename):
    """Return mjd from the psrfits file
    """

    # Get RA and DEC in degrees from psrfits file
    first_file = glob.glob("%s*fits"%os.path.join(LINKDISK, basefilename))[0]
    pfits = Psrfits(first_file)
    mjd = pfits.get_MJD()
    pfits.close()
    return mjd

def add_obs2DB(pointing_name, basefilename, numfiles, disk):

    grid_id = get_grid_id(basefilename)

    # Connect to the DB
    db = Database(db=DB_NAME)
    DBconn = db.conn
    DBcursor = db.cursor

    QUERY = "SELECT * FROM processing WHERE pointing_name = '%s'"%(pointing_name)
    DBcursor.execute(QUERY)
    result_query = [list(row) for row in DBcursor.fetchall()]

    # Get Date
    mjd = get_MJD_obs(basefilename)
    gregdate = slalib.sla_djcl(mjd)
    yr, month, day, fracday = gregdate[:4]
    h, m, s = MJDfrac_to_hms(fracday)

    # No observation was planned, do an INSERT
    if not result_query:
	QUERY = "INSERT IGNORE INTO processing (basefilename, numfiles, institution, diskname, obs_date, add_date, grid_id, proc_stat) VALUES ('%s', %d, '%s', '%s', '%d-%d-%d %d:%d:%d', NOW(), %d, 'o');" % (basefilename, numfiles, INSTITUTION, get_disk_label_nfs(disk), yr, month, day,h, m, s, grid_id)
    # Found that the observation was planned, do an UPDATE
    else:	    
	QUERY = "UPDATE processing SET basefilename='%s', numfiles=%d, institution='%s', diskname='%s', obs_date='%d-%d-%d %d:%d:%d', add_date=NOW(), proc_stat='o' WHERE pointing_name='%s' ;" % (basefilename, numfiles, INSTITUTION, get_disk_label_nfs(disk), yr, month, day,h, m, s, pointing_name)

    if DEBUG: print QUERY
    DBcursor.execute(QUERY)

    # Get the obs_id number
    QUERY = "SELECT obs_id FROM processing WHERE basefilename='%s'"%(basefilename)
    DBcursor.execute(QUERY)
    obs_id = DBcursor.fetchone()[0]

    # Update the full search table
    QUERY = "INSERT IGNORE INTO full_processing (obs_id, status) VALUES (%d, 'available');" % (obs_id)
    DBcursor.execute(QUERY)


    # Now update the rawdata table
    files = glob.glob("/survey_data/survey?/%s*.fits"%basefilename)
    files.sort()
    for fullfilename in files:
        path, filename = os.path.split(fullfilename)

	datasize = os.path.getsize(fullfilename)
	QUERY = "INSERT INTO raw_files (obs_id, path, filename, datasize) VALUES (%d, '%s', '%s', %ld);"%(obs_id, path, filename, datasize)
	DBcursor.execute(QUERY)



    DBconn.close()	

if __name__ == "__main__":
    #check_space_size_wait("/")
    print get_disk_label_nfs("/survey_data/current_disk/")
