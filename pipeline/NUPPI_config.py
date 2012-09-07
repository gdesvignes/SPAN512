# Basic parameters
MIN_FREE_SPACE = 7 * 1024 * 1024 * 1024  # 7 GB free for now
LINKDISK = "/survey_data/current_disk"
DATADISK = "/survey_data/survey"
DB_NAME = "local-SPAN512"
NB_DATA_DISK = 4 
RECIPIENTS = ["gdesvignes.astro@gmail.com", "icognard@cnrs-orleans.fr"]
RECIPIENTS_short = ["gdesvignes.astro@gmail.com"]
INSTITUTION = "Nancay"
DEBUG = False 
#DEBUG = True

# For the full_proc pipeline
MAX_STAGGING_SPACE_DISK = 100 * 1024 * 1024 * 1024 # 100 GB
STAGGING_AREA = "/data/NFS2/stagging_area"

# NRT
NRT_RA_DEG = 4/60.
NRT_DEC_DEG = 22/60.

# institution is one of: 'UBC', 'NRAOCV', "McGill", "Columbia", "Cornell"
institution           = "Nancay" 
pipeline	      = "PRESTO"
survey		      = "BON"

# Local database
host                  = "localhost" 
distant_host          = "clairvaux" 
database	      = "SBON512" # current version of MySQL database
usrname               = "pulsar" 
pw                    = "0244bon"


# Common database
#common_host	      = "arecibosql.tc.cornell.edu" # Common DB host 
#common_database	      = "[palfa-common]" # Common DB name 
#common_usrname	      = "desvignes" 
#common_pw	      = "0244bon" 


# Locations of scripts
pipeline_loc	      = "/data/NFS2/NUPPI/" 
scripts_loc           = pipeline_loc + "pipeline/"
misc_loc	      = pipeline_loc + "miscellaneous/"
logs_loc	      = pipeline_loc + "logs/"

# Results location
final_dir             = "/data/NFS2/NUPPI/results" 
dbinfo_dir	      = final_dir + "/database_info/"
error_dir	      = final_dir + "/error/"
log_file              = final_dir + "/NUPPI_log.txt"
max_jobs              = 50000 # max no. jobs running or queued at any one time
email                 = "gdesvignes.astro@gmail.com" 


# Configurations for processing
use_subbands          = False
use_ressources	      = "bon"
use_fast_SP	      = False
email_comp	      = "clairvaux" #INSERT VALUE# # 
working_dir	      = "/scratch/NUPPI" # Name of working directory on nodes
merge_psrfits_prog    = "/usr/local/src/psrfits_utils/merge_psrfits"
#base_output_directory = "/data/NBPP/NBPP-results/"
node_req	      = "nodes=1:ppn=1:%s"%(use_ressources) # PBS request
#rows2del              = 8    # Corresponds to 8 sec of data to remove at the beginning of the observation (with nuppi databuf=256MB)


# Tunable parameters for searching and folding
# (you probably don't need to tune any of them)
rfifind_timesig       = 9
rfifind_freqsig       = 3.5
rfifind_chunk_time    = 0.5  # 0.5 second 
singlepulse_threshold = 5.0  # threshold SNR for candidate determination
singlepulse_plot_SNR  = 6.0  # threshold SNR for singlepulse plot
singlepulse_maxwidth  = 0.1  # max pulse width in seconds
to_prepfold_sigma     = 6.0  # incoherent sum significance to fold candidates
max_cands_to_fold     = 120  # Never fold more than this many candidates
numhits_to_fold       = 2    # Number of DMs with a detection needed to fold
low_DM_cutoff         = 2.0  # Lowest DM to consider as a "real" pulsar
lo_accel_numharm      = 16   # max harmonics
lo_accel_sigma        = 3.0  # threshold gaussian significance
lo_accel_zmax         = 0    # bins
lo_accel_flo          = 1.0  # Hz
hi_accel_numharm      = 8    # max harmonics
hi_accel_sigma        = 3.0  # threshold gaussian significance
hi_accel_zmax         = 50   # bins
hi_accel_flo          = 1.0  # Hz
low_T_to_search       = 20.0 # sec
MAX_DM_2_ZAP_SP       = 50.0 # Maximum DM under which the time series are zapped for the 50 Hz signal
bad_chans	      = "0,645,646,647,648,649,650,651,652,653,654,781,782,783,784,785,786,787,788,789,790,791,792"


def classification_map(x):
    # map from character to classification rank.
    # return None if x is not mappable to a rank.
    try:
	rank = int(x)
    except:
	rank = None
    return rank
