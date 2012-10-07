#!/usr/bin/env python
import glob, os, os.path, shutil, socket, struct, sys, time, tarfile
import numpy, psr_utils, presto, sifting

import psrfits

from NUPPI_config import *


# Sifting specific parameters (don't touch without good reason!)
sifting.sigma_threshold = to_prepfold_sigma-1.0  # incoherent power threshold (sigma)
sifting.c_pow_threshold = 100.0                  # coherent power threshold
sifting.r_err           = 1.1    # Fourier bin tolerence for candidate equivalence
sifting.short_period    = 0.0008 # Shortest period candidates to consider (s)
sifting.long_period     = 15.0   # Longest period candidates to consider (s)
sifting.harm_pow_cutoff = 8.0    # Power required in at least one harmonic

def get_baryv(ra, dec, mjd, T, obs="AO"):
    """
    get_baryv(ra, dec, mjd, T):
         Determine the average barycentric velocity towards 'ra', 'dec'
             during an observation from 'obs'.  The RA and DEC are in the
             standard string format (i.e. 'hh:mm:ss.ssss' and
             'dd:mm:ss.ssss').  'T' is in sec and 'mjd' is (of course) in MJD.
   """
    tts = psr_utils.span(mjd, mjd+T/86400.0, 100)
    nn = len(tts)
    bts = numpy.zeros(nn, dtype=numpy.float64)
    vel = numpy.zeros(nn, dtype=numpy.float64)
    presto.barycenter(tts, bts, vel, nn, ra, dec, obs, "DE200")
    avgvel = numpy.add.reduce(vel)/nn
    return avgvel

def find_masked_fraction(obs):
    """
    find_masked_fraction(obs):
        Parse the output file from an rfifind run and return the
            fraction of the data that was suggested to be masked.
    """
    rfifind_out = obs.basefilename + "_rfifind.out"
    for line in open(rfifind_out):
        if "Number of  bad   intervals" in line:
            return float(line.split("(")[1].split("%")[0])/100.0
    # If there is a problem reading the file, return 100%
    return 100.0

def get_all_subdms(ddplans):
    """
    get_all_subdms(ddplans):
        Return a sorted array of the subdms from the list of ddplans.
    """
    subdmlist = []
    for ddplan in ddplans:
        subdmlist += [float(x) for x in ddplan.subdmlist]
    subdmlist.sort()
    subdmlist = numpy.asarray(subdmlist)
    return subdmlist

def find_closest_subbands(obs, subdms, DM):
    """
    find_closest_subbands(obs, subdms, DM):
        Return the basename of the closest set of subbands to DM
        given an obs_info class and a sorted array of the subdms.
    """
    subdm = subdms[numpy.fabs(subdms - DM).argmin()]
    return "subbands/%s_DM%.2f.sub[0-6]*"%(obs.basefilename, subdm)

def timed_execute(cmd): 
    """
    timed_execute(cmd):
        Execute the command 'cmd' after logging the command
            to STDOUT.  Return the wall-clock amount of time
            the command took to execute.
    """
    sys.stdout.write("\n'"+cmd+"'\n")
    sys.stdout.flush()
    start = time.time()
    os.system(cmd)
    end = time.time()
    return end - start

def get_folding_command(cand, obs, ddplans):
    """
    get_folding_command(cand, obs, ddplans):
        Return a command for prepfold for folding the subbands using
            an obs_info instance, a list of the ddplans, and a candidate 
            instance that describes the observations and searches.
    """
    # Folding rules are based on the facts that we want:
    #   1.  Between 24 and 200 bins in the profiles
    #   2.  For most candidates, we want to search length = 101 p/pd/DM cubes
    #       (The side of the cube is always 2*M*N+1 where M is the "factor",
    #       either -npfact (for p and pd) or -ndmfact, and N is the number of bins
    #       in the profile).  A search of 101^3 points is pretty fast.
    #   3.  For slow pulsars (where N=100 or 200), since we'll have to search
    #       many points, we'll use fewer intervals in time (-npart 30)
    #   4.  For the slowest pulsars, in order to avoid RFI, we'll
    #       not search in period-derivative.
    zmax = cand.filename.split("_")[-1]
    outfilenm = obs.basefilename+"_DM%s_Z%s"%(cand.DMstr, zmax)

    # Note:  the following calculations should probably only be done once,
    #        but in general, these calculation are effectively instantaneous
    #        compared to the folding itself
    if use_subbands:  # Fold the subbands
        subdms = get_all_subdms(ddplans)
        subfiles = find_closest_subbands(obs, subdms, cand.DM)
        foldfiles = subfiles
    else:  # Folding the downsampled psrfits files instead
        hidms = [x.lodm for x in ddplans[1:]] + [2000]
        dfacts = [x.downsamp for x in ddplans]
        for hidm, dfact in zip(hidms, dfacts):
            if cand.DM < hidm:
                downsamp = dfact
                break
        filfile = obs.filename
        foldfiles = filfile
    p = 1.0 / cand.f
    nsub = 64
    if p < 0.002:
        Mp, Mdm, N = 2, 2, 24
        otheropts = "-npart 50 -ndmfact 3"
    elif p < 0.05:
        Mp, Mdm, N = 2, 1, 50
        otheropts = "-npart 40 -pstep 1 -pdstep 2 -dmstep 3"
    elif p < 0.3:
        Mp, Mdm, N = 1, 1, 100
        otheropts = "-npart 30 -pstep 1 -pdstep 2 -dmstep 1"
    else:
        Mp, Mdm, N = 1, 1, 200
        otheropts = "-npart 24 -nopdsearch -pstep 1 -pdstep 2 -dmstep 1 -nodmsearch"
    return "prepfold -psrfits -noxwin -accelcand %d -accelfile %s.cand -dm %.2f -o %s %s -n %d -npfact %d -ndmfact %d %s -nsub %d -mask %s" % \
           (cand.candnum, cand.filename, cand.DM, outfilenm,
            otheropts, N, Mp, Mdm, foldfiles, nsub, obs.maskfilenm)

class obs_info:
    """
    class obs_info(filename)
        A class describing the observation and the analysis.
    """
    def __init__(self, filename):
        self.filename = filename

	self.basefilename = filename.replace(".fits","")
	pfits = psrfits.Psrfits(self.filename)
	self.MJD = pfits.get_MJD()
	self.nchans = pfits.get_nchan()
	self.BW = pfits.get_BW()
	self.dt = pfits.get_tsamp()
	self.ra_string = pfits.get_RA()
	self.dec_string = pfits.get_DEC()
	self.orig_N = pfits.get_nsamp()
	self.orig_T = pfits.get_obslen()
        self.N = psr_utils.choose_N(self.orig_N)
        self.T = self.N * self.dt
        # Determine the average barycentric velocity of the observation
        self.baryv = get_baryv(self.ra_string, self.dec_string,
                               self.MJD, self.T, obs="NC")
        # Figure out which host we are processing on
        self.hostname = socket.gethostname()
        # The fraction of the data recommended to be masked by rfifind
        self.maskfilenm = self.basefilename + "_rfifind.mask"
        self.masked_fraction = 0.0
        # Initialize our timers
        self.rfifind_time = 0.0
        self.downsample_time = 0.0
        self.subbanding_time = 0.0
        self.dedispersing_time = 0.0
        self.FFT_time = 0.0
        self.lo_accelsearch_time = 0.0
        self.hi_accelsearch_time = 0.0
        self.singlepulse_time = 0.0
        self.sifting_time = 0.0
        self.folding_time = 0.0
        self.total_time = 0.0
        # Inialize some candidate counters
        self.num_sifted_cands = 0
        self.num_folded_cands = 0
        self.num_single_cands = 0

    def write_report(self, filenm):
        report_file = open(filenm, "w")
        report_file.write("---------------------------------------------------------\n")
        report_file.write("%s was processed on %s\n"%(self.filename, self.hostname))
        report_file.write("Ending UTC time:  %s\n"%(time.asctime(time.gmtime())))
        report_file.write("Total wall time:  %.1f s (%.2f hrs)\n"%\
                          (self.total_time, self.total_time/3600.0))
        report_file.write("Fraction of data masked:  %.2f%%\n"%\
                          (self.masked_fraction*100.0))
        report_file.write("---------------------------------------------------------\n")
        report_file.write("          rfifind time = %7.1f sec (%5.2f%%)\n"%\
                          (self.rfifind_time, self.rfifind_time/self.total_time*100.0))
        if use_subbands:
            report_file.write("       subbanding time = %7.1f sec (%5.2f%%)\n"%\
                              (self.subbanding_time, self.subbanding_time/self.total_time*100.0))
        else:
            report_file.write("     downsampling time = %7.1f sec (%5.2f%%)\n"%\
                              (self.downsample_time, self.downsample_time/self.total_time*100.0))
        report_file.write("     dedispersing time = %7.1f sec (%5.2f%%)\n"%\
                          (self.dedispersing_time, self.dedispersing_time/self.total_time*100.0))
        report_file.write("     single-pulse time = %7.1f sec (%5.2f%%)\n"%\
                          (self.singlepulse_time, self.singlepulse_time/self.total_time*100.0))
        report_file.write("              FFT time = %7.1f sec (%5.2f%%)\n"%\
                          (self.FFT_time, self.FFT_time/self.total_time*100.0))
        report_file.write("   lo-accelsearch time = %7.1f sec (%5.2f%%)\n"%\
                          (self.lo_accelsearch_time, self.lo_accelsearch_time/self.total_time*100.0))
        report_file.write("   hi-accelsearch time = %7.1f sec (%5.2f%%)\n"%\
                          (self.hi_accelsearch_time, self.hi_accelsearch_time/self.total_time*100.0))
        report_file.write("          sifting time = %7.1f sec (%5.2f%%)\n"%\
                          (self.sifting_time, self.sifting_time/self.total_time*100.0))
        report_file.write("          folding time = %7.1f sec (%5.2f%%)\n"%\
                          (self.folding_time, self.folding_time/self.total_time*100.0))
        report_file.write("---------------------------------------------------------\n")
        report_file.close()

class dedisp_plan:
    """
    class dedisp_plan(lodm, dmstep, dmsperpass, numpasses, numsub, downsamp)
        A class describing a de-dispersion plan for prepsubband in detail.
    """
    def __init__(self, lodm, dmstep, dmsperpass, numpasses, numsub, downsamp):
        self.lodm = float(lodm)
        self.dmstep = float(dmstep)
        self.dmsperpass = int(dmsperpass)
        self.numpasses = int(numpasses)
        self.numsub = int(numsub)
        self.downsamp = int(downsamp)
        # Downsample less for the subbands so that folding
        # candidates is more accurate
        self.sub_downsamp = self.downsamp / 2
        if self.sub_downsamp==0: self.sub_downsamp = 1
        # The total downsampling is:
        #   self.downsamp = self.sub_downsamp * self.dd_downsamp
        if self.downsamp==1: self.dd_downsamp = 1
        else: self.dd_downsamp = 2
        self.sub_dmstep = self.dmsperpass * self.dmstep
        self.dmlist = []  # These are strings for comparison with filenames
        self.subdmlist = []
        for ii in range(self.numpasses):
            self.subdmlist.append("%.2f"%(self.lodm + (ii+0.5)*self.sub_dmstep))
            lodm = self.lodm + ii * self.sub_dmstep
            dmlist = ["%.2f"%dm for dm in \
                      numpy.arange(self.dmsperpass)*self.dmstep + lodm]
            self.dmlist.append(dmlist)

# Create our de-dispersion plans (for 100MHz WAPPs)
# The following are the "optimal" values for the 100MHz
# survey.  It keeps the total dispersive smearing (i.e.
# not counting scattering <1 ms up to a DM of ~600 pc cm^-3
ddplans = []
if (1):
    ddplans.append(dedisp_plan(   0.0,   0.3,     700,     1,       32,       4))
    ddplans.append(dedisp_plan( 210.0,   0.3,     700,     1,       32,       4))
    ddplans.append(dedisp_plan( 420.0,   0.3,     700,     1,       32,       4))
    ddplans.append(dedisp_plan( 630.0,   0.5,     758,     1,       32,       8))
    ddplans.append(dedisp_plan(1009.0,   1.0,     791,     1,       32,      16))
    # The values here are:       lodm dmstep dms/call #calls #subbands downsamp
    """
    ddplans.append(dedisp_plan(   0.0,   0.1,     500,     1,       32,       1))
    ddplans.append(dedisp_plan(  50.0,   0.1,     500,     1,       32,       1))
    ddplans.append(dedisp_plan( 100.0,   0.1,     500,     1,       32,       1))
    ddplans.append(dedisp_plan( 150.0,   0.1,     500,     1,       32,       1))
    ddplans.append(dedisp_plan( 200.0,   0.2,     750,     1,       32,       2))
    ddplans.append(dedisp_plan( 350.0,   0.3,     750,     1,       32,       4))
    ddplans.append(dedisp_plan( 575.0,   0.5,     500,     1,       32,       8))
    ddplans.append(dedisp_plan( 825.0,   0.5,     500,     1,       32,       8))
    ddplans.append(dedisp_plan(1075.0,   1.0,     225,     1,       32,      16))
    """

else: # faster option that sacrifices a small amount of time resolution at the lowest DMs
    # The values here are:       lodm dmstep dms/call #calls #subbands downsamp
    ddplans.append(dedisp_plan(   0.0,   0.5,      22,    21,       32,       1))
    ddplans.append(dedisp_plan( 231.0,   0.5,      24,     6,       32,       2))
    ddplans.append(dedisp_plan( 303.0,   1.0,      24,    11,       32,       4))
    ddplans.append(dedisp_plan( 567.0,   3.0,      24,     7,       32,       8))
    
def main(filename, workdir):

    # Change to the specified working directory
    os.chdir(workdir)

    # Get information on the observation and the jbo
    job = obs_info(filename)
    if job.T < low_T_to_search:
        print "The observation is too short (%.2f s) to search."%job.T
        sys.exit()
    job.total_time = time.time()
    
    # Use whatever .zaplist is found in the current directory
    default_zaplist = glob.glob("*.zaplist")[0]

    # Create a directory to hold all the subbands
    if use_subbands:
        try:
            os.makedirs("subbands")
        except: pass
    
    print "\nBeginning NUPPI search of '%s'"%job.filename
    print "UTC time is:  %s"%(time.asctime(time.gmtime()))

    # rfifind the psrfits file
    cmd = "rfifind -zapchan %s -zapints %s -psrfits -timesig %.2f -freqsig %.2f -time %.17g -o %s %s > %s_rfifind.out"%\
          (bad_chans, bad_ints, rfifind_timesig, rfifind_freqsig, rfifind_chunk_time, job.basefilename,
           job.filename, job.basefilename)
    job.rfifind_time += timed_execute(cmd)
    # Find the fraction that was suggested to be masked
    # Note:  Should we stop processing if the fraction is
    #        above some large value?  Maybe 30%?
    job.masked_fraction = find_masked_fraction(job)
    
    # Iterate over the stages of the overall de-dispersion plan
    dmstrs = []
    for ddplan in ddplans:

        # Iterate over the individual passes through the .fil file
        for passnum in range(ddplan.numpasses):
            subbasenm = "%s_DM%s"%(job.basefilename, ddplan.subdmlist[passnum])

            if use_subbands:
                # Create a set of subbands
                cmd = "prepsubband -psrfits -sub -subdm %s -downsamp %d -nsub %d -mask %s -o subbands/%s %s > %s.subout"%\
                      (ddplan.subdmlist[passnum], ddplan.sub_downsamp,
                       ddplan.numsub, job.maskfilenm, job.basefilename,
                       job.filename, subbasenm)
                job.subbanding_time += timed_execute(cmd)
            
                # Now de-disperse using the subbands
                cmd = "prepsubband -psrfits -lodm %.2f -dmstep %.2f -numdms %d -downsamp %d -numout %d -o %s subbands/%s.sub[0-9]* > %s.prepout"%\
                      (ddplan.lodm+passnum*ddplan.sub_dmstep, ddplan.dmstep,
                       ddplan.dmsperpass, ddplan.dd_downsamp, job.N/ddplan.downsamp,
                       job.basefilename, subbasenm, subbasenm)
                job.dedispersing_time += timed_execute(cmd)

            else:  # Not using subbands
                cmd = "prepsubband -psrfits -downsamp %d -mask %s -lodm %.2f -dmstep %.2f -numdms %d -numout %d -o %s %s"%\
                      (ddplan.downsamp, job.maskfilenm, ddplan.lodm+passnum*ddplan.sub_dmstep, ddplan.dmstep,
                       ddplan.dmsperpass, job.N/ddplan.downsamp,
                       job.basefilename, filename)
                job.dedispersing_time += timed_execute(cmd)
            
            # Iterate over all the new DMs
            for dmstr in ddplan.dmlist[passnum]:
                dmstrs.append(dmstr)
                basenm = job.basefilename+"_DM"+dmstr
                datnm = basenm+".dat"
                infnm = basenm+".inf"

                # Do the single-pulse search
                cmd = "single_pulse_search.py -p -m %f -t %f %s"%\
                      (singlepulse_maxwidth, singlepulse_threshold, datnm)
		#if float(dmstr) < MAX_DM_2_ZAP_SP:
		#    cmd += " -z"
		if use_fast_SP:
		    cmd += " -f"
                job.singlepulse_time += timed_execute(cmd)

                
                # Do the low-acceleration search
                cmd = "accelsearch -harmpolish -zaplist %s -baryv %.6g -numharm %d -sigma %f -zmax %d -flo %f %s"%\
                      (default_zaplist, job.baryv, lo_accel_numharm, lo_accel_sigma, lo_accel_zmax, lo_accel_flo, datnm)
                job.lo_accelsearch_time += timed_execute(cmd)
                try:
                    os.remove(basenm+"_ACCEL_%d.txtcand"%lo_accel_zmax)
                except: pass
        
                # Remove the .dat and .fft files
                try:
                    os.remove(datnm)
                except: pass

    # Make the single-pulse plots
    basedmb = job.basefilename+"_DM"
    basedme = ".singlepulse "
    # The following will make plots for DM ranges:
    #    0-120, 90-340, 280-1000+
    dmglobs = [basedmb+"[0-9].[0-9][0-9]"+basedme +
               basedmb+"[0-9][0-9].[0-9][0-9]"+basedme +
               basedmb+"10[0-9].[0-9][0-9]"+basedme,
               basedmb+"[12][0-9][0-9].[0-9][0-9]"+basedme +
               basedmb+"30[0-9].[0-9][0-9]"+basedme,
               basedmb+"[3-9][0-9][0-9].[0-9][0-9]"+basedme +
               basedmb+"1[0-9][0-9][0-9].[0-9][0-9]"+basedme]
    dmrangestrs = ["0-120", "90-340", "280-1000+"]
    psname = job.basefilename+"_singlepulse.ps"
    for dmglob, dmrangestr in zip(dmglobs, dmrangestrs):
        cmd = 'single_pulse_search.py -t %f -g "%s"' % \
              (singlepulse_plot_SNR, dmglob)
        job.singlepulse_time += timed_execute(cmd)
        try:
            os.rename(psname,
                      job.basefilename+"_DMs%s_singlepulse.ps"%dmrangestr)
        except: pass

    # Sift through the candidates to choose the best to fold
    
    job.sifting_time = time.time()

    lo_accel_cands = sifting.read_candidates(glob.glob("*ACCEL_%d"%lo_accel_zmax))
    if len(lo_accel_cands):
        lo_accel_cands = sifting.remove_duplicate_candidates(lo_accel_cands)
    if len(lo_accel_cands):
        lo_accel_cands = sifting.remove_DM_problems(lo_accel_cands, numhits_to_fold,
                                                    dmstrs, low_DM_cutoff)


    all_accel_cands = lo_accel_cands 
    if len(all_accel_cands):
        all_accel_cands = sifting.remove_harmonics(all_accel_cands)
        # Note:  the candidates will be sorted in _sigma_ order, not _SNR_!
        all_accel_cands.sort(sifting.cmp_sigma)
        sifting.write_candlist(all_accel_cands, job.basefilename+".accelcands")

    job.sifting_time = time.time() - job.sifting_time

    # Fold the best candidates

    cands_folded = 0
    for cand in all_accel_cands.cands:
        if cands_folded == max_cands_to_fold:
            break
        if cand.sigma > to_prepfold_sigma:
	    print get_folding_command(cand, job, ddplans)
            job.folding_time += timed_execute(get_folding_command(cand, job, ddplans))
            cands_folded += 1

    # Now step through the .ps files and convert them to .png and gzip them

    psfiles = glob.glob("*.ps")
    for psfile in psfiles:
        if "singlepulse" in psfile:
            # For some reason the singlepulse files don't transform nicely...
            epsfile = psfile.replace(".ps", ".eps")
            os.system("eps2eps "+psfile+" "+epsfile)
            os.system("pstoimg -density 100 -crop a "+epsfile)
            try:
                os.remove(epsfile)
            except: pass
        else:
            os.system("pstoimg -density 100 -flip cw "+psfile)
        os.system("gzip "+psfile)
    
    # And finish up

    job.total_time = time.time() - job.total_time
    print "\nFinished"
    print "UTC time is:  %s"%(time.asctime(time.gmtime()))

    # Write the job report

    job.write_report(job.basefilename+".report")
    
if __name__ == "__main__":
    # Arguments to the search program are
    # sys.argv[1] = psrfits file name
    # sys.argv[2] = working directory name
    filename = sys.argv[1]
    workdir = sys.argv[2]
    main(filename, workdir)
