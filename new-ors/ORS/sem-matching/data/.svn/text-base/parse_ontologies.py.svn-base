#!/usr/bin/env python

"Works with KIF ontologies"


import re
import urllib
import time
import sys
import os
import os.path

start = time.time() # this is a float

# the absolute path of the directory where the parse_ontologies module is
path_here = os.path.expandvars('$ORS_HOME/ORS/sem-matching/data/')


predicates = {'documentation':{}, 'subclass':{}, 'instance':{}} # will be used by the extract_info function

# abbr_n_vers is a dictionary of abbreviations 
# and number of versions available
# I deliberately omit the upper ontology (SUMO) from 
# the parsing because its descriptions are
# too abstract to help us in our task of constructing
# bags of words for concepts.
abbr_n_vers = {'ArabicCulture.kif': ('AC', 9), 'CCTrep.kif': ('CC', 3), 'Communications.kif': ('CM', 10), 
               'CountriesAndRegions.kif': ('CR', 21), 'domainEnglishFormat.kif': ('DE', 10),
               'Economy.kif': ('EC', 30), 'elements.kif': ('EL', 7), 'engineering.kif': ('EG', 24),
               'english_format.kif': ('EF', 12), 'FinancialOntology.kif': ('FI', 23), 'FOAFmap.kif': ('FO', 4),
               'Geography.kif': ('GG', 38), 'Government.kif': ('GV', 29), 'Justice.kif': ('JU', 3),
               'Languages.kif': ('LA', 3), 'Media.kif': ('MD', 5), 'Merge.kif': ('MG', 68),
               'Mid-level-ontology.kif': ('ML', 90), 'Military.kif': ('MI', 20), 'MilitaryDevices.kif': ('MD', 12),
               'MilitaryPersons.kif': ('MP', 9), 'MilitaryProcesses.kif': ('MC', 16), 'MILO-format.kif': ('MF', 4), 
               'mondial.kif': ('MO', 6), 'naics.kif': ('NA', 23), 'People.kif': ('PE', 17), 'pictureList.kif': ('PL', 31),
               'pictureList-ImageNet.kif': ('PI', 3), 'QoSontology.kif': ('QS', 15), 'Sports.kif': ('SP', 3),
               'TransnationalIssues.kif': ('TN', 5), 'Transportation.kif': ('TP', 29), 
               'VirusProteinAndCellPart.kif': ('VP', 6), 'WMD.kif': ('WM', 11), 'WorldAirports.kif': ('WA', 1), 
               'WorldAirportsA-K.kif': ('WK', 4), 'WorldAirportsL-Z.kif': ('WZ', 4)}

abb = {}
for key in abbr_n_vers:
  abb[abbr_n_vers[key][0]] = key



#==================#
#   nested_lists   #
#==================#

def nested_lists(fl, version=0): # fl is the *path* to the file which contains the ontology or a *url*
                                 # version is an optional argument, which works iff fl is just a filename
                                 # (or its abbreviation) i.e. not a full path
  """
  The function can be called in different ways, e.g.:
  1) nested_lists('ontologies/CountriesAndRegions.kif/1.2/CountriesAndRegions.kif')
  2) nested_lists('CountriesAndRegions.kif', 2)
  3) nested_lists('http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/KBs/CountriesAndRegions.kif?revision=1.2')
  4) nested_lists('CR', 2)
  5) nested_lists('cr', 2)
  """
  try:
    st = open(fl).read()
  except:
    try:
      if abb.has_key(fl):
        fl = abb[fl]
      elif abb.has_key(fl.upper()):
        fl = abb[fl.upper()]
      path = 'ontologies/%s/1.%s/%s' % (fl, version, fl)
      path = path_here + path
      st = open(path).read()
    except:
      try:
        st = urllib.urlopen(fl).read()
      except:
        print '\n-----Please provide a *full local path*, *url* or *filename and its version*.-----\n'
        sys.exit()
  super_dict = {}  
  """
  super_dict is the dictionary of all KIF terms in the text  e.g. (instance NewEngland GeographicArea)
  These terms might have nested structure, e.g. (=> (instance ?CITY EuropeanCity)(part ?CITY Europe))
  but they always have balanced bracketing. As a formal language, this has *context-free* expressivity, 
  so it couldn't be captured by a *regular* expression
  In super_dict the key is a tuple e.g. (4, 13), which means that the term in the value starts at 
  index 4 and ends at index 13. The value is a complex term with closed brackets, e.g.
  ['=>', ['instance', '?SYSTEM', 'TelephoneSystem'], 
         ['exists', ['?PHONE'], ['and', ['instance', '?PHONE', 'Telephone'], 
                                ['engineeringSubcomponent', '?PHONE', '?SYSTEM']]]]
  """
  depth = -1
  d = {}
  c = {}  # dictionary with comments. The key is a tuple of indexes and the value is a string of comments
  word = ''    # e.g. 'subclass'
  phrase = ''  # e.g. "Wallis and Futuna"
  phrase_brackets = 0 
  """ 
  In phrase_brackets, 0 means balanced brackets, -1 means one open bracket etc.
  phrase_brackets is there to prevent bad tokenisation in case
  the closing quote of a phrase has been forgotten and the
  the closing brackets following belong to the phrase (See below)
  """
  index = -1   # the index of the opening bracket of a new term
  comments = ''
  comments_index = -1
  latest_parse = ['cmt', (0, 0)]
  """
  latest_parse[0] takes one of the values 'trm' or 'cmt', i.e. terms or comments. 
  latest_parse[1] is a tuple containing the indices of the last thing appended 
  to d or c. If we have finished parsing a comment and want to append it to c, 
  for example, c[(comments_index, i)] = comments, we check if latest_parse[0] is 'cmt' 
  and if yes, we attach the comment to the previous comments (since no KIF term interferes) 
  and add them to c as one instead of two different keys. See code for details.
  """
  for i in range(len(st)):
    if comments:  # i.e. if 'comments' is not an empty string
      comments += st[i]
      if st[i] == '\n':
        if latest_parse[0] == 'cmt':
          try:
            c[(latest_parse[1][0], i)] = c[latest_parse[1]] + comments
            del c[latest_parse[1]]
            latest_parse = ['cmt', (latest_parse[1][0], i)]
          except:
            c[(comments_index, i)] = comments
            latest_parse = ['cmt', (comments_index, i)]
        else:
          c[(comments_index, i)] = comments
          latest_parse = ['cmt', (comments_index, i)]
        comments = ''
    elif st[i] == ';' and not d: # i.e. if d is empty, which means that all open brackets have closed, therefore the semi-colon is not inside any term 
        comments += st[i]
        comments_index = i
        """
        According to the brief SUO-KIF reference manual (p. 10), comments start with a single semi-colon ; 
        and end with a newline Pease, A. (2009) "Standard Upper Ontology Knowledge Interchange Format", 
        online at http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
        """
    elif st[i] == '(':
      if phrase:
        phrase_brackets -= 1
        phrase += st[i]
      else:
        depth += 1
        if not d: 
          """ 
          i.e. if d is empty, which means that the left bracket is the beginning of a 
          new complex term, we store its index. Later we will also store the index of the 
          right bracket that completes the term before it is appended to the superlist.
          This will be useful when we need to position the terms with respect to comments 
          in the ontology (The starting and ending index of comments will also be stored)
          """
          index = i  
        if not d.has_key(depth):
          d[depth] = []
    elif st[i] == ')':
      if phrase:   # Normally, when a bracket closes, the phrase inside it has closed already.
        pt = re.compile(r'([0-9]+|[i|v|x|I|V|X]+)\)')
        chars = st[i-4:i+1]  
        """
        We look whether we have any number (arabic or latin) before the closing bracket, 
        e.g. iv), IX), 4) etc. If yes, we know that we don't balance any open bracket.
        This is found in the MilitaryDevices.kif ontologies
        http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/KBs/MilitaryDevices.kif?view=log
        """
        if not re.findall(pt, chars): 
          if phrase_brackets == 0:  # i.e. 'phrase' should normally be an empty string
            phrase += '"'           # But this is trying to sidestep some typos, 
            d[depth].append(phrase) # where the closing quotation marks are not there. (See below)
            phrase = ''  
            word = ''
          else:
            phrase += ')'
            phrase_brackets += 1 
      else:
        if word:
          try:
            d[depth].append(word)        
          except:
            pass           
        try:                    
          d[depth-1].append(d[depth])
          """
          Every time we encounter a closing bracket, we will have to go one level 
          up the tree depth, so we append the whole construction to the upper level 
          and delete the lower level, which is useless now. This won't work if depth-1 
          is -1, i.e. when we are already at depth zero (all open brackets have closed)
          So, in the 'except' statement, we append the whole list d[0] in super_list 
          and we empty the dictionary. Before it is emptied, d will look something like 
          this:     d = {0: ['err', ['fr', 'grr']]}
          """
          del d[depth]     
          depth -= 1    
          word = ''
        except:
          try:
            super_dict[(index, i)] = d[0]
            latest_parse = ['trm', (index, i)]
            depth = -1
            del d[0]
          except:
            pass
    elif st[i] == '"':
      if phrase:
        phrase += '"'
        d[depth].append(phrase)
        phrase = ''
      else:
        phrase = '"'
      word = ''
      """
      The function should be robust enough to handle typos such as:
      (documentation ColumbusOhio EnglishLanguage "The capital of Ohio, located in the middle of the state.)
      In the above term, the closing double quotes have accidentally been omitted, but this could be 
      destructive for our programme since everything encountered before the next double quote
      will be included in a string, e.g. 
      (documentation ColumbusOhio EnglishLanguage "The capital of Ohio, located in the middle of the state.)
                                                   (instance Alabama AmericanState)
                                                   (meetsSpatially Alabama Mississippi)
                                                   (documentation Alabama EnglishLanguage "

      example taken from http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/KBs/CountriesAndRegions.kif?revision=1.21
      """
    elif st[i]==' ' or st[i]=='\n' or st[i]=='\t':   
      if word:
        if phrase:
          phrase += ' '
        else:
          try:
            d[depth].append(word)
          except:
            pass
        word = ''
    else:
      word += st[i]
      if phrase:
        phrase += st[i]
  path = 'ontologies/%s/1.%s/%s_nested_lists' % (fl, version, fl)
  path = path_here + path
  # If we want to generate a python file containing the two
  # dictionaries, namely super_dict and c, we can
  # comment out the following code:
  """
  try:
    f = open(path+'.py', 'w')
  except:
    f = open('nested_lists.py', 'w')
  f.write('#!/usr/bin/env python\n\n')
  f.write('super_dict = ')
  f.write(str(super_dict))
  f.write('\n\n\nc = ')
  f.write(str(c))
  f.close()
  """
  # If we want to generate a text file containing sorted
  # keys and values for the two dictionaries (super_dict
  # and c), we can comment out the following code:
  """
  try:
    f2 = open(path+'.txt', 'w')
  except:
    f2 = open(path_here+'nested_lists.txt', 'w')
  keys = super_dict.keys()
  keys.sort()
  for i in keys:
    line = str(i) + ': ' + str(super_dict[i]) + '\n'
    f2.write(line)
  k = c.keys()
  k.sort()
  f2.write('\n\n\n')
  for i in k:
    line = str(i) + ': ' + str(c[i]) + '\n'
    f2.write(line)
  f2.close()
  """
  # The function returns a tuple containing the two dictionaries
  return (super_dict, c)


"""
Some errors in the ontology were difficult to sidestep.
For example, in http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/KBs/Mid-level-ontology.kif?revision=1.80
the first argument of hasPurpose is erroneously enclosed into brackets.

(=>
 (instance ?F Furniture)
 (hasPurpose (?F)
   (exists (?FL)
     (and
       (instance ?FL Floor)
       (meetsSpatially ?F ?FL)))))

The algorithm will inevitably treat (?F) as a nullary predicate and not as just a word.
Note that in the case of 'exists' and 'forall', the first argument can legally be enclosed
in brackets. (Genesereth and Fikes 1992: 10)

Also, we might have commented out code. 
e.g. in http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/KBs/FinancialOntology.kif?revision=1.21
"""


#==================#
#   extract_info   #
#==================#

def extract_info(fl, version=0):
  dictio = nested_lists(fl, version)[0]
  """
  extract_info is a function that returns information found on the 
  roots of the trees represented in nested lists (i.e. values of keys 
  in dictio.), e.g. (subclass LiquefiedPetroleumGas FossilFuel). This
  function will not recursively traverse whole trees to find subclass,
  instance and documentation information because traversing the
  following, for example, would not give us any immediately useful
  subclass information (since 'assertions' are conditional on other
  things being true and my implementation will not make any use of
  reasoning:
  (=>
    (instance ?COIN CurrencyCoin)
    (exists (?METAL)
      (and
        (subclass ?METAL Metal)
        (material ?METAL ?COIN)))) 

  To see what dictio looks like, comment out some code in 
  the nested_lists function and generate a python/text file.
  """
  if not abb.has_key(fl):
    abbrev = abbr_n_vers[fl][0] # e.g. 'MD' for 'Media.kif'
  for tup in dictio:
    # a 'tup' looks like this    -->  (1047647, 1047670): ['subclass', 'Pollen', 'Object']
    # Also, we will notice that everything is a subclass of itself (Naive set theory)
    subj = dictio[tup][1]
    pred = dictio[tup][0]
    obj = dictio[tup][-1]
    """
    obj is the list dictionary[tup] indexed to -1 instead of 2 
    as we might expect in binary relations. The reason is that the 
    'documentation' predicate also appears with arity 3 (when it 
    includes information about the language). In any case, the text 
    that we need to extract is the *last* element; not necessarily
    the *third* element in the list.
    """
    if pred == 'subclass' or pred == 'instance' or pred == 'documentation': 
      """
      We ignore second-order assertions such as (instance angularMeasure BinaryPredicate)
      because they are not useful for our task and may negatively affect our results.
      """
      if not 'Predicate' in obj and not 'Function' in obj and not 'Relation' in obj:
        if type(obj) != list:
          """
          With the above if statement, we ignore cases where fuctional terms occupy the
          object position, e.g. ['subclass' 'MedicalClinicBuilding' ['ComplementFn' 'Residence']]
          This information is not crucial for our task and would only make things more
          complicated.
          """
          try:
            if not predicates[pred].has_key(subj):
              predicates[pred][subj] = [abbrev, version, obj]
            else:
              if not obj in predicates[pred][subj]:
                predicates[pred][subj].append(obj)
          except:
            pass
  # To see what the *predicates* dictionary looks like,
  # comment out the following code, which generates a
  # python file.  
  """
  fl = open(path_here+'hierarchy.py', 'w')
  fl.write('#!/usr/bin/env python\n\n')
  fl.write('predicates = ')
  fl.write(str(predicates))
  fl.close() 
  """
  # To generate a more human readable file
  # comment out the following code:
  """
  f = open(path_here+'hierarchy', 'w')
  docum = ''
  for i in predicates:
    line = i + ': ' + str(predicates[i]) + '\n\n\n\n'
    f.write(line)
    try:
      docum += predicates[i]['documentation'] + '  '
    except:
      pass
  f.close()
  """
  
#===================#
#   generate_info   #
#===================#

def generate_info(onto, versions): 
  """
  *onto* should be a string such as 'FinancialOntology.kif' or 'FO'
  *versions* can be an integer or a list of integers
  e.g. generate_info('Merge.kif', [37, 25, 12, 2, 3, 27, 33, 1])
  """
  if type(versions) == int:
    versions = [versions]
  f = open(path_here+'docum_subcl_inst', 'w')
  for ve in versions:
    try:
      extract_info(onto, ve)
      """
      Every time the extract_info function is called, it
      gives more information to the *predicates* dictionary
      which is external to the function.
      """  
      print onto, ve, 'parsed!'
    except:
      print onto, ve, 'failed!'
  for key in predicates: # Remember that keys are 'instance', 'subclass' and 'documentation'
    short_key = key[0] # i.e. 's' for 'subclass' etc.
    for k in predicates[key]: # the values of predic are themselves dictionaries
      l = predicates[key][k]
      """
      l is a list whose first element is the ontology name,
      second element is the ontology version and the rest of 
      the elements are the suject's *superclasses* (given 
      multiple inheritance), *types* or *documentation*
      """
      line = str(l[0]) + ' ' + str(l[1]) + ' ' + short_key + ' ' + k + ' '
      f.write(line)
      for word in l[2:]:
        f.write(str(word)+' ') 
      f.write('\n')
  f.close()


#===============#
#   parse_all   #
#===============#

def parse_all():
  li = abbr_n_vers.keys()
  li.sort()
  for ontology in li:
    versions_range = reversed(range(1, abbr_n_vers[ontology][1]+1))
    """
    The range is reversed because ontologies will be parsed from
    the most recent to the oldest. This will have the advantage that
    if the same piece of information is found in many versions, only 
    the most recent ontology will be referenced. See the
    generate_info function for details.
    """
    generate_info(ontology, versions_range)



#==============================#
#   remove_unnecessary_files   #
#==============================#

def remove_unnecessary_files():
  """
  This function can be called (under the
  if __name__ == '__main__' statement)
  if we want to delete generated files 
  from all the versions of all the 
  ontologies, which would be laborious 
  to do manually.
  """
  for ontology in abbr_n_vers:
    for i in range(1, abbr_n_vers[ontology][1]+1):
      directory = 'ontologies/%s/1.%s/' % (ontology, str(i))
      diretory = path_here + directory
      try:
        command = 'rm ' + directory + ontology + '_predicates'
        os.system(command)
      except:
        pass
      try:
        command = 'rm ' + directory + ontology + '_nested_lists.txt'
        os.system(command)
      except:
        pass
        




if __name__ == '__main__':

  parse_all()
  #remove_unnecessary_files()
  end = time.time()
  running_time = end - start
  print '\nRunning time: %.3f seconds\n' % running_time















