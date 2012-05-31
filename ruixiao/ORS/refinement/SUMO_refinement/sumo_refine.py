#!/usr/bin/env python

"""
This module replaces the old lexeme with the
new one in the Planning Agent's ontology after
semantic matching.
"""


import re
import optparse
import os.path




def replace(old_lex, new_lex, kind, ontology): 
  """
  --> *old_lex* and *new_lex* are strings, e.g. 'Cup', 'DrinkingCup'
  --> *kind* has to be either 'class-indiv' or 'relation'
  --> *ontology* has to be a string again; the name of the 
  Planning Agent's ontology file, 'Mid-level-ontology-v-87.kif'
  as seen in the PlanningAgent/ontologies/original/ directory
  """
  try:
    if kind == 'class-indiv':
      old_lex = old_lex[0].upper()+old_lex[1:]
      new_lex = new_lex[0].upper()+new_lex[1:]
    try:
      f = open(pa_path+'updated/'+ontology)

    except:
      f = open(pa_path+'original/'+ontology)    
    text = f.read()
    f.close()
    raw_string = r'([\s|\(])%s([\s|\)])' % old_lex
    pat = re.compile(raw_string)
    sr =  re.search(pat, text)
    new_text = re.sub(pat, r'\1%s\2' % new_lex, text)
    f2 = open(pa_path+'updated/'+ontology, 'w')
    f2.write(new_text)
    f2.close()
  except:
    print """
Something went wrong!
Plase make sure that your 'original lexeme' exists
in the original ontology and that the name of the
ontology file is correctly spelt.
          """





if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--old')
  p.add_option('--new')
  p.add_option('--kind')
  p.add_option('--ont')  
  p.add_option('--sce')  
  (options, arguments) = p.parse_args()
  old_l = options.old
  new_l = options.new
  knd = options.kind
  onto = options.ont
  global path_to_pa
  pa_path = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/'+options.sce+'/PA-onts/')

  replace(old_l, new_l, knd, onto)

  """
  Two example commands:
  python modules/refinement/sem_match_sumo_refine.py --old cup --new drinkingCup --kind class-indiv --ont Mid-level-ontology_v34.kif
  python modules/refinement/sem_match_sumo_refine.py --old lastName --new familyName --kind relation --ont Mid-level-ontology_v34.kif
  """






