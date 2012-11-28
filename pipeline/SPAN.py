import os
from tools import *
from mysendmail import sendMail
from NUPPI_config import *
from time import sleep
from psrfits import Psrfits


# Start script assuming disk0  
disk_id=4
datadisk = DATADISK+str(disk_id)

try:
    os.unlink(LINKDISK)
    os.symlink(datadisk, LINKDISK)
except:
    pass


# Main Loop
while True:
    
    # Check for free space on current disk
    free_space = check_space_size_wait(LINKDISK) # Check that a transfer is not occuring
    if DEBUG: print "Free Space : %dGB"%(free_space >> 30)
    if free_space < MIN_FREE_SPACE:
        # First send mail
	title = "Survey Pulsar - Disk full"
	msg = "Disk %s full"%(datadisk)
	print title, msg
	attached_files = [] 
	sendMail(RECIPIENTS, title, msg, attached_files)

	# Change disk - Loop over the 5 disks/bays available
	disk_id = (disk_id+1)%NB_DATA_DISK
	datadisk = DATADISK+str(disk_id)

	os.unlink(LINKDISK)
        os.symlink(datadisk, LINKDISK)
	continue
   	

    # Check for new files
    new_obsnames, new_files_nb = check_new_obs(LINKDISK)
    #print "GD", new_obsnames, new_files_nb
    if new_obsnames:

        for obsname, numfiles in zip(new_obsnames, new_files_nb):

	    # TODO Add file to DB
	    print "Adding %s [%d] to the database - Disk %s: %dGB free"% (obsname, numfiles, datadisk, free_space >> 30)
	    #pfits = Psrfits(os.path.join(LINKDISK,obsname))

	    # pointing_name is the source name
	    pointing_name = obsname.split('_')[2]

	    # Assume we have only one file per frequency band 
	    #numfiles = 1

	    add_obs2DB(pointing_name, obsname, numfiles, datadisk)
	    #add_obs2DB(pointing_name, LINKDISK, obsname, numfiles, datadisk, nbfile)

	    # Send mail
	    title = "Survey Pulsar - New observation %s added to the database"%obsname
	    msg = "Observation %s added to the database"%(obsname)
	    attached_files = [] 
	    sendMail(RECIPIENTS, title, msg, attached_files)
	    
    sleep(120)
