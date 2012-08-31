#!/usr/bin/env python
#
from NUPPI_config import *
from PBSQuery import PBSQuery
from PBSQuery import PBSError
import os, os.path

import sys

class PBSManager():

	def __init__(self):
	    self.p = PBSQuery()
	    self.queue = self.p.getqueue(survey)
	    self.jobs = self.p.getjobs()
	    self.nodes = self.p.getnodes_with_property(use_ressources)

	    self.queue_names = self.jobs.keys()

	    #print self.nodes
	    #print self.nodes['psr13']['ncpus']

	def jobs_running(self):

	    queue_status = self.queue['state_count'][0]

	    Transit, Queued, Held, Waiting, Running, Exiting = queue_status.split()

	    Queued = int(Queued.split(':')[1])
	    Running = int(Running.split(':')[1])
	    return Running, Queued


	def is_running(self, basefilename):

	    jobnames = []
	    for jobs_name in self.queue_names: 
	        job_info = self.jobs[jobs_name]
		jobnames.append(job_info["Job_Name"][0])

	    return (basefilename in jobnames)


	def get_stderr_path(self, basefilename):
	    stderr_path = os.path.join(logs_loc, "%s.err"%(basefilename))
	    if not os.path.exists(stderr_path):
		raise ValueError("Cannot find error log for job (%s): %s" % \
				(basefilename, stderr_path))
	    return stderr_path

	def had_errors(self, basefilename):    
	    try:
                errorlog = self.get_stderr_path(basefilename)
	    except ValueError:
	        errors = False
	    else:
	        if os.path.getsize(errorlog) > 0:
	            errors = True
		else:
		    errors = False
	    return errors

	def get_errors(self, basefilename):    
	    try:
		errorlog = self.get_stderr_path(basefilename)
	    except ValueError, e:
	        errors = str(e)
	    else:
		if os.path.exists(errorlog):
		    err_f = open(errorlog, 'r')
		    errors = err_f.read()
		    err_f.close()
		    os.remove(errorlog)
	    return errors

	def get_num_free_cpus(self):
	    num_free_cpus = 0 
	    node = None
	    for n in self.nodes.keys():
	        num_jobs = len(self.nodes[n].setdefault('jobs',[]))
		if (self.nodes[n]['state'] != ['free']):
		#if (self.nodes[n]['state'] != ['free']) or (num_jobs >= self.max_jobs_per_node):
		    continue
		num_free_cpus += int(self.nodes[n]['np'][0]) - num_jobs
	    return num_free_cpus	

	def close(self):
	    del self

"""
p = PBSManager()
jobs = p.p.getjobs()
jobs_names = jobs.keys()

for jobs_name in jobs_names: 
    job_info = jobs[jobs_name]
    print job_info["Job_Name"]
"""
