#!/usr/bin/env python

"""
This module replaces the old lexeme with the
new one in the Planning Agent's ontology after
semantic matching.
"""


import re
import optparse
import os.path
pa_path = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/')



def add_facts(facts, ontology): 
  """
  ---> *facts* will be a string of the form
        '[involvedInEvent(mugPaintingContest2010,cupStillLifeByJerry), 
          property(jerryTheBot,won),
          not(subField(medicalScience,biology))]'
  ---> *ontology* will be the name of the ontology file, e.g. 'Mid-level-ontology_v34.kif'
  
  """
  f = open(pa_path+'updated/'+ontology)
  st = f.read()
  f.close()
  facts = facts[1:-1].split()
  new_facts = []
  for f in facts:
    if f[-1] == ',':
      f = f[:-1]
    new_f = ''
    bracket = 0
    for i in range(len(f)):
      if bracket == 1:
        new_f += f[i].upper()
        bracket = 0
      else:
        new_f += f[i]
      if f[i] == '(' or f[i] == ',':
        bracket = 1
    new_f = re.sub(r'[\,|\(]', ' ', new_f)
    new_f = re.sub('\)', '', new_f)
    new_f = '('+new_f+')'
    if not new_f[1:4] == 'not':
      if not new_f in st:
        st += '\n'+new_f
    else: 
      negated = '('+new_f[1:5]+'('+new_f[5:]+')'
      not_negated = '('+new_f[5:-1]+')'
      if not not_negated in st:
        if not negated in st:
          st += '\n'+negated
    
    new_facts.append(new_f)
  f = open(pa_path+'updated/'+ontology, 'w')
  f.write(st)
  f.close()
      

        






if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--facts')
  p.add_option('--ont')  
  (options, arguments) = p.parse_args()
  fa= options.facts
  onto = options.ont

  add_facts(fa, onto)


  """
  An example command:
  python modules/refinement/sem_match_sumo_update.py --facts "[involvedInEvent(mugPaintingContest2010,cupStillLifeByJerry), property(jerryTheBot,won), not(subField(medicalScience,biology))]" --ont Mid-level-ontology_v34.kif

  To call it from Sicstus you will need:

  process_create('/bin/sh', ['-c', 'python modules/refinement/sem_match_sumo_update.py --facts "[involvedInEvent(mugPaintingContest2010,cupStillLifeByJerry), property(jerryTheBot,won), not(subField(medicalScience,biology))]" --ont Mid-level-ontology_v34.kif'])

  """






