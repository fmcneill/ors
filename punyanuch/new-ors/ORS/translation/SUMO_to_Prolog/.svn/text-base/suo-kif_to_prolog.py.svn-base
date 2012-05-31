#!/usr/bin/env python


import time
import sys
import os.path
parse_ont_path = os.path.expandvars('$ORS_HOME/ORS/sem-matching/data/')
sys.path.append(parse_ont_path)
import parse_ontologies
import optparse


start = time.time() # this is a float



def trans(kif_name):  
  """
  translate variable names from e.g. ?AGENT --> Agent,
            class names from e.g. 'Process' --> 'process',
            relation names from e.g. 'holdsDuring' --> 'holdsDuring'
  """
  if kif_name[0] == '?':
    try:
      return kif_name[1]+kif_name[2:].lower()
    except:
      return kif_name[1]   # This will be returned if the variable has only one letter, e.g. ?X
  elif kif_name[0].isupper():
    try:
      return kif_name[0].lower()+kif_name[1:]
    except:
      return kif_name[0].lower()
  else:
    return kif_name


#===============#
#   to_prolog   #
#===============#


def to_prolog(formula): # formula in the form of a list e.g. ['instance', 'Michael', 'Human']
  prolog_string = ''
  if formula[0] == 'instance':
    prolog_string += 'class('
  else:
    prolog_string += trans(formula[0])+'('
  for i in formula[1:]:
    if type(i) == list:
      return  # This returns None
    for t in set(['Predicate', 'Attribute', 'Relation', 'Function', 'Role']):
      if t in i:
        return
    else:
      prolog_string += trans(i)+','
  prolog_string = prolog_string[:-1]+')'
  return prolog_string
  


#========================#
#   extract_statements   #
#========================#

def extract_statements(fl, version=0):
  statements = {'instance': set([]), 'subclass': set([]), 'facts': set([]), 'agentNeeded': set([])}
  try:
    sys.path.append(path_to_pa+'updated')
    import nested_lists
    dictio = nested_lists.nlists[0]
  except:
    tup = parse_ontologies.nested_lists(fl, version)
    dictio = tup[0]
    f = open(path_to_pa+'updated/nested_lists.py', 'w')
    f.write('nlists = '+str(tup))
    f.close()
  try:
    if not abb.has_key(fl):
      abbrev = abbr_n_vers[fl][0] # e.g. 'MD' for 'Media.kif'
  except:
    pass
  actions = []
  for tup in dictio:
    formula = dictio[tup]
    # a 'tup' looks like this    -->  (1047647, 1047670): ['subclass', 'Pollen', 'Object']
    # Also, we will notice that everything is a subclass of itself (Naive set theory)
    blacklisted = set(['documentation', '=>', 'domain', 
                       'subAttribute', 'partition', 'range', 'relatedInternalConcept',
                       'disjointRelation', 'rangeSubclass', 'not', 'disjointDecomposition',
                       'synonymousExternalConcept', 'names', 'abbreviation', 'conventionalLongName',
                       'formerName', 'conventionalLongName', 'organizationalObjective',
                       'equal', 'subsumesContentClass', 'termFormat', 'externalImage', 
                       'forall', 'longitude', 'format', 'exhaustiveDecomposition', 'dateEstablished',
                       'modalAttribute', 'lexicon', 'earthAltitude', 'latitude', 'located',
                       'depth', 'landAreaOnly', 'flowsInto',
                       '<=>', 'disjoint', 'subrelation', 'domainSubclass', 'contraryAttribute'])

    if formula[0] == '=>' and formula[1][0] == 'and' and formula[2][0] == 'causesProposition' and formula[2][1][0] == 'and' and formula[2][2][0] == 'and':
      actions.append(formula)
    elif formula[0] == 'serviceProvider':
      form2 = ['agentNeeded', formula[2], formula[1]]
      statements['agentNeeded'].add(to_prolog(form2))
      statements['facts'].add(to_prolog(formula))
    elif not formula[0] in blacklisted: 
      a = to_prolog(formula)
      if a:  # i.e. if to_prolog has not returned None
        if a[:5] == 'class':
          statements['instance'].add(a)
        elif a[:8] == 'subclass':
          statements['subclass'].add(a)
        else:
          statements['facts'].add(a)
    statements['actions'] = find_actions(actions)
  return statements




#==================#
#   find_actions   #
#==================#


def find_actions(lists):  # lists is a list of actions in the form of nested lists
  actions = {}
  for formula in lists:
    if formula[0] == '=>' and formula[1][0] == 'and' and formula[2][0] == 'causesProposition' and formula[2][1][0] == 'and' and formula[2][2][0] == 'and':
      d = actions[formula[1][1][2]] = {'arguments': [], 'preconditions': [], 'effects': []}    # formula[1][1][2] is the name of the action
      for i in formula[1][2:]:
        if i[0] == 'involvedInEvent':
          d['arguments'].append(i[2])
        elif i[0] == 'instance':
          d['preconditions'].append(i)
      for s in formula[2][1][1:]:
        """
        Below I eliminate existential quantification because the
        action is going to be executed by the Service Providing
        Agent, who is going to check preconditions with *queries*.
        As Pease (2009:10-11) notes:
           "The significance of the free variables in a 
            sentence depends on the use of the sentence. 
            When we assert the truth of a sentence with 
            free variables, we are, in effect, saying that 
            the sentence is true for all values of the free 
            variables, i.e. the variables are universally 
            quantified. When we ask whether a sentence with 
            free variables is true, we are, in effect, asking 
            whether there are any values for the free variables 
            for which the sentence is true, i.e. the variables 
            are existentially quantified.
            [...]
            Note that the significance of free (unquantified) 
            variables depends on context. Free variables in an 
            assertion are assumed to be universally quantified. 
            Free variables in a query are assumed to be existentially 
            quantified. The interpretation of an unquantified
            variable is that it is quantified at the outermost 
            scope of the formula. The meaning of free variables 
            is determined by the way in which SUO-KIF is used. It 
            cannot be unambiguously defined within SUO-KIF itself. 
            To be certain of the usage in all contexts,
            use explicit quantifiers"
        """
        if s[0] == 'exists':
          for y in s[2:]:
            d['preconditions'].append(y)
        else:
          d['preconditions'].append(s)
      for t in formula[2][2][1:]:
        d['effects'].append(t) 
  def transformula(formula): # formula should be a list
    """
    input can be a simple (not nested) 
    formula e.g. (instance ?P Process)
    or one with negation e.g.
    not(instance ?P Process)
    """
    if formula[0] == 'instance':
      new_formula = 'class('
      for i in formula[1:]:
        new_formula += trans(i)+','
      new_formula = new_formula[:-1]+')'
      return new_formula
    elif formula[0] == 'not':
      new_formula = 'not('+transformula(formula[1])+')'
      return new_formula
    else:
      new_formula = trans(formula[0])+'('
      for t in formula[1:]:
        new_formula += trans(t)+','
      new_formula = new_formula[:-1]+')'
      return new_formula
  translated_actions = {}
  rules_used = 0
  for a in actions:
    translated = 'rule('+trans(a)+'('
    args = actions[a]['arguments']
    for ar in args:
      translated += trans(ar)+','
    rules_used += 1
    translated = translated[:-1]+'),\n     [rule'+str(rules_used)+',\n      ['
    precs = actions[a]['preconditions']
    for pre in precs:
      translated += transformula(pre)+','
    translated = translated[:-1]+'],\n      ['
    effs = actions[a]['effects']
    for eff in effs:
      translated += transformula(eff)+','
    translated = translated[:-1]+']\n     ])'
    translated_actions[rules_used] = translated
  return translated_actions



#==================#
#   create_files   #
#==================#


def create_files(di): # di should be a dictionary returned by the extract_statements function
  acts = di['actions']
  subcls = di['subclass']
  fas = di['facts']
  insts = di['instance']
  ags = di['agentNeeded']
  f1 = open(path_to_pa+'updated/centralSig.pl', 'w')
  st1 = ''
  l = acts.keys()
  l.sort()
  for a in l:
    st1 += acts[a]+'.\n\n'
  st1 += 'nonFactsList([]).\n\nmyFactsList([]).\n\ntransitivePredsList([]).\n\n'
  for s in subcls:
    st1 += s+'.\n'
  f1.write(st1)
  f1.close()
  f2 = open(path_to_pa+'updated/centralThy.pl', 'w')
  st2 = ':- dynamic fact/1,class/2.\n\n'
  for i in insts:
    st2 += i+'.\n'
  st2 += '\n\n'
  for f in fas:
    st2 += 'fact('+f+').\n'
  f2.write(st2)
  f2.close()
  f3 = open(path_to_pa+'updated/metaOnt.pl', 'w')
  st3 = 'nonFacts([]).\n\nmyFacts([]).\n\ntransitivePreds([]).\n\ninform([]).\n\n'
  for ag in ags:
    st3 += ag+'.\n'
  st3 += '\n'
  f3.write(st3)
  f3.close()




if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--ont')
  p.add_option('--sce')
  (options, arguments) = p.parse_args()
  ontology_file = options.ont # This might be something like 'Mid-level-ontology_v34.kif'
  global path_to_pa
  path_to_pa = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/'+options.sce+'/PA-onts/')
  ont_path = path_to_pa+'updated/'+ontology_file
  create_files(extract_statements(ont_path))

  end = time.time()
  running_time = end - start
  #print '\nRunning time: %.3f seconds\n' % running_time















