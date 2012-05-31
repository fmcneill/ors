#!/usr/bin/env python

"Works with KIF ontologies"


import time
import optparse
import re
import sys
import os
import os.path
parse_ont_path = os.path.expandvars('$ORS_HOME/ORS/sem-matching/data/')
sys.path.append(parse_ont_path)
import parse_ontologies

start = time.time() # this is a float



#========================#
#   write_nested_lists   #
#========================#



def write_nested_lists(fl): # fl is the absolute *path* to the file which contains the ontology file
  try:
    n_lists = parse_ontologies.nested_lists(fl)
  except:
    
    fl = re.sub('updated', 'original', fl)
    print fl
    n_lists = parse_ontologies.nested_lists(fl)
  f = open(path_to_pa+'updated/nested_lists.py', 'w')
  f.write('nlists = '+str(n_lists))
  f.close()





if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--ont')
  p.add_option('--sce')
  (options, arguments) = p.parse_args()
  scenario_name = options.sce
  global path_to_pa
  path_to_pa = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/'+scenario_name+'/PA-onts/')
  ontology_file = options.ont # This might be something like 'Mid-level-ontology_v34.kif'
  write_nested_lists(path_to_pa+'updated/'+ontology_file)
  """
  One command that we could give is:
  python compute_nested_lists.py --ont Mid-level-ontology_v34.kif
  """

  end = time.time()
  running_time = end - start
  #print '\nRunning time: %.3f seconds\n' % running_time


