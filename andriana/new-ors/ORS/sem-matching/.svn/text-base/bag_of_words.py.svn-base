#!/usr/bin/env python

"""
This module creates a *bag of words* for each on of the
'concepts' in the ontology (i.e. classes, relations,
individuals etc).
"""


import time
import re
import sys
import os.path
parse_ont_path = os.path.expandvars('$ORS_HOME/ORS/sem-matching/data/')
sys.path.append(parse_ont_path)
import parse_ontologies

start = time.time() # this is a float

# the absolute path of the directory where the bag_of_words module is
path_here = os.path.expandvars('$ORS_HOME/ORS/sem-matching/')

stemmed = {}
"""
stemmed is a dictionary which will keep track of all the
inflected words (e.g. those ending in -ied, -ing, -ed, -s etc.)
and their base forms obtained from the stemming algorithm.
This has the advantage that if a word has been stemmed once, 
it doesn't have to be stemmed again; we can only look it up in 
the 'stemmed' dictionary.
A key-value pair will look like this:
'parties': 'party'
"""



#====================#
#   read_databases   #
#====================#

def read_databases():
  synsets = {}
  d = {}
  words_n_synsets = {} 
  sumo_wordnet_mappings = {}
  irregular = {}
  # 1) synsets, their hypernyms and relevant text
  sns = open(path_here+'data/synsets_info').read()
  p = re.compile(r'(([0-9]{8}\s)+)([^\n]+)\n')
  for tup in re.findall(p, sns):
    l = tup[0].split()
    if not synsets.has_key(l[0]):
      synsets[l[0]] = []
    try:
      synsets[l[0]] += l[1:]
    except:
      pass
    synsets[l[0]].append(tup[2])
  # 2) documentation, subclass and instance information
  dsi = open(path_here+'data/docum_subcl_inst').read()
  pat = re.compile(r'[A-Z]{2}\s[0-9]+\s[i|s|d]\s([^\s]+)\s([^\n]+)\n')
  l = re.findall(pat, dsi)
  for tup in l:
    if not d.has_key(tup[0]):
      d[tup[0]] = ''
    d[tup[0]] += ' ' + tup[1]
  # 3) words and their synsets
  wsyns = open(path_here+'data/words_n_synsets')
  for line in wsyns:
    l = line.split()
    words_n_synsets[l[0]] = l[1:]
  # 4) sumo-wordnet mappings
  sm = open(path_here+'data/sumo_wordnet')
  for line in sm:
    l = line.split()
    if not sumo_wordnet_mappings.has_key(l[0]):
      sumo_wordnet_mappings[l[0]] = l[1:]
    else:
      sumo_wordnet_mappings[l[0]].append(l[1:])
  # 5) set of stop-words
  stop_words = set(open(path_here+'data/WordNet/stopwords.txt').read().split())
  # 6) set of all words in WordNet excluding exceptions (i.e. irregular forms)
  all_words = set(open(path_here+'data/all_words').read().split())
  # 7) irregular forms and their base forms

  for file_obj in [open(path_here+'data/WordNet/WordNet-3.0/exc/noun.exc'), 
                   open(path_here+'data/WordNet/WordNet-3.0/exc/verb.exc'), 
                   open(path_here+'data/WordNet/WordNet-3.0/exc/adj.exc'), 
                   open(path_here+'data/WordNet/WordNet-3.0/exc/adv.exc')]:
    for line in file_obj:
      l = line.split()
      if not irregular.has_key(l[0]):
        irregular[l[0]] = []
      irregular[l[0]] = l[1:]
  return (synsets, d, words_n_synsets, sumo_wordnet_mappings, stop_words, all_words, irregular)


"""
The read_databases() function returns a 7-tuple, which contains the
following elements: (I use the names assigned later)

[0] ---- synsets ----
    This is a dictionary whose key-value pairs look like this:
    '09680504': ['09679925', 'Roman_Catholic | a member of the Roman Catholic Church  ']
    '01358231': ['draining exhausting | having a debilitating effect ']
    The key is a synset offset and the value is a list whose last item is some
    natural language text about the synset (words that define the synset + gloss).
    Before the last element, we may have other synsets, which are hypernyms of
    the synset in the key.

[1] ---- dsi ----
    This is a dictionary whose key-value pairs look like this:
    'ChannelIslands':  'DependencyOrSpecialSovereigntyArea  "A dependency of the &%UnitedKingdom"'
    The key is a concept in SUMO, MILO or the domain ontologies derived from them. The
    value is a string which subclass and/or instance and/or documentation information 
    found in all versions of all ontologies.

[2] ---- words_n_synsets ----
    This is a dictionary whose key-value pairs look like this:
    'chatter': ['02185861', '01553869', '01038666', '01036804', '01037303']
    The key is an entry is WordNet and the value is a list of synsets, i.e.
    senses for this word, sorted from the most to the least frequently occurring.

[3] ---- sumo_wordnet_mappings ----
    This is a dictionary whose key-value pairs look like this:
    'LinguisticExpression': ['06284225', '06723908', '07012534', '02842445']
    The key is a concept in SUMO and the value is a list of synsets that the
    concept is synonymous to.
    Niles, I. and Pease, A. (2003) "Linking lexicons and ontologies: 
    Mapping WordNet to the Suggested Upper Merged Ontology", in 
    Proceeding of the 2003 International Conference on Information 
    and Knowledge Engineering (IKE 03), Las Vegas.

[4] ---- stop_words ----
    This is a set (i.e. unsorted list) of stop words (i.e. words such as 'and', 
    'or' etc. that are ignored because they don't carry any semantic meaning).
    It looks like this:
    set(['and', 'or', 'that', 'she', 'on' ...etc])

[5] ---- all_words ----
    This is a set of all WordNet entries, excluding exceptions.
    It looks like this:
    set(['human_right', 'lamp', 'hundred-and-fifteenth', 'husband-wife_privilege'...etc])

[6] ---- irregular ----
    This is a dictionary whose key-value pairs look like this:      
    'proglottides': ['proglottid', 'proglottis']
    The key is an inflected or irregular form whose base form could not
    be predicted by any stemming algorithm (e.g. bought ---> buy).
    The value is a list of possible base forms.
"""

db = read_databases()

synsets = db[0]
dsi = db[1]
words_n_synsets = db[2]
sumo_wordnet_mappings = db[3]
stop_words = db[4]
all_words = db[5]
irregular = db[6]


#===============#
#   is_number   #
#===============#

def is_number(string): # returns True for strings such as '5', '64' etc.
  try:
    float(string)
    return True
  except:
    return False


#===================#
#   is_camel_case   #
#===================#

def is_camel_case(word):
  counter = 0
  no_first_letter = word[1:]
  for char in no_first_letter:
    if char.islower():
      if counter == 0:
        counter += 1
      elif counter == 2:
        return True
    else:
      if counter == 0:
        counter += 2
      if counter == 1:
        return True
  if word[0].islower() and counter == 2:
    is_camel_case = True
  return False


#======================#
#   split_camel_case   #
#======================#

def split_camel_case(string, separator=''):
  """
  separator is an optional argument, If it is set
  to ' ', '-' or '_', then the function does not 
  return a list of tokens but a string in which the 
  tokens are separated by spaces, hyphens or 
  underscores respectively. For example,
  split_camel_case('SwissStyleMuesli') returns
  ['Swiss', 'Style', 'Muesli']
  split_camel_case('SwissStyleMuesli', '-') returns  
  'Swiss-Style-Muesli'
  """
  if not is_camel_case(string):
    return string
  tokens = []
  last_upper = 0
  for i in range(len(string)):
    if string[i].isupper() and not last_upper == i:  
      """
      if last_upper == i, an empty string will be appended to *tokens*
      """
      tokens.append(string[last_upper:i])
      last_upper = i
  tokens.append(string[last_upper:]) # last part of the string
  if separator == '_':
    tokens = '_'.join(tokens)
  elif separator == '-':
    tokens = '-'.join(tokens)
  elif separator == ' ':
    tokens = ' '.join(tokens)
  return tokens


#====================#
#   normalise_text   #
#====================#

def normalise_text(string): # input is some raw/unprocessed text
  # remove urls
  pat1 = re.compile(r'(http[^\s]*|www.[^s]*)') 
  # remove dates etc.
  pat2 = re.compile(r'-?[0-9]+-?')    
  # remove possessive 's
  pat3 = re.compile(r'\'[s|S]')          
  # remove punctuation
  pat4 = re.compile(r'[;|,|<|>|:|/|\.|\[|\]|\(|\)|#|\\|"|&|%|\?|\']') 
  string = re.sub(pat1, '', string)
  string = re.sub(pat2, '', string)
  string = re.sub(pat3, '', string)
  string = re.sub(pat4, '', string)
  words = string.split()
  words = [split_camel_case(w, '_').lower() for w in words]
  """
  check if a word is in camel case and, if yes (e.g. CzechRepublic), separate 
  the words with underscores. Then convert all words in lower case. Underscores 
  are useful when searching if the phrase (e.g. czech_republic) is a WordNet 
  entry because in multi-word entries spaces are replaced by underscores 
  in WordNet. Also, all the stopwords are in lower case. After all, case information 
  is not crucial for our task and can be dropped. On the contrary, we want to know 
  that the word, say, 'contact' appears twice in a bag of words, but we wouldn't be 
  able to conclude that if the two words were in different cases.
  """
  tokens = []
  for wo in words:
    wo = re.sub(' ', '', wo)
    if not wo in stop_words:
      wo = ''.join([letter for letter in wo if ord(letter) < 128]) # remove non-ascii characters
      tokens.append(wo) 
  return stemming(tokens)  # returns a list of 'cleaned' words


#==============#
#   is_vowel   #
#==============#

def is_vowel(word, index): # e.g. is_vowel('bright', 0)
  """
  We can't just give a letter as input to this
  function and decide whether or not it is a vowel
  because vowel and consonant depend on the phonetic
  context of the phoneme. The word provides this context.
  """
  return not is_consonant(word, index)


#==================#
#   is_consonant   #
#==================#

def is_consonant(word, index): # index must be an integer
  char = word[index]
  if char == 'a' or char == 'e' or char =='i' or char =='o' or char =='u':
    return False  # i.e. character is not a consonant
  if char != 'y' or index == 0: 
    return True     
  else:
    return not is_consonant(word, index-1)


#===================#
#   remove_plural   #
#===================#

def remove_plural(word):
  if word[-1] == 's':
    if word[-3:] == 'ies':  # e.g. parties, calories
      word = word[:-1]  # remove the final s
      if word in all_words:  # e.g. calorie
        return word
      word = word[:-2] + 'y' # e.g. party
      return word
    elif word[-2:] == 'es':  # e.g. buses, faces, crosses
      word = word[:-1] # remove final s
      if len(word) >= 4 and word in all_words and not word[-3:-1] == 'ss':  # we prevent 'crosse' for crosses. Although it is found in WN, it is very rare.
        return word  # e.g. face
      word = word[:-1]  # remove e
      if word in all_words:
        return word
      word += 'e'  # undo 'remove e'
      return word  # the default is to retain e
    else:  # e.g. dogs, pompous, dress
      if len(word) > 3 and word[-3:] != 'ous' and word[-2] != 's':
        word = word[:-1] # remove final s, unless the word ends in 'ous' or a double 's'
      return word
  else:
    return word


#=======================#
#   remove_past_tense   #
#=======================#

def remove_past_tense(word):
  if len(word) <= 4 and not word[-3:] == 'ied':  # This prevents things like fled --> fl  # We'll deal with small verbs (e.g. 'die') later
    return word
  if word[-3:] == 'ied': # e.g. 'replied'
    if len(word) == 4:
      word = word[:-1]
      return word
    word = word[:-1]  # remove final d
    if word in all_words:
      return word
    word = word[:-2] + 'y'
    return word
  elif word[-2:] == 'ed': # e.g. grabbed, played, filled
    word = word[:-1]  # remove final d
    if word in all_words:
      return word
    word = word[:-1]  # remove 'ed'
    if word in all_words:
      return word
    if word[-2] == word[-1]:  # e.g. grabbed
      word = word[:-1]
      if word in all_words:
        return word
    else:
      word += 'e' # undo 'remove e'
      if word in all_words:
        return word
    return word
  else:
    return word


#===================#
#   remove_aspect   #
#===================#

def remove_aspect(word): # i.e. -ing
  if len(word) <= 5 and not 'ying' in word:
    return word
  if word[-3:] == 'ing':   # e.g. replying, playing, grabbing, stripping, striping, striking, dying
    if word[1] == 'y':
      word = word[0] + 'ie'
      return word
    word = word[:-3] + 'e'  # remove 'ing' and add 'e', e.g. strike
    if word in all_words:
      return word
    word = word[:-1]  # remove the added 'e'
    if word in all_words:
      return word
    if word[-2] == word[-1]:  # if there is a double character before 'ing' (e.g. 'bb' in 'grabbing')
      word = word[:-1]  # remove the second character, so we have sth like 'grab'
      if word in all_words:
        return word
      word += word[-1]  # restore the second character
      return word
    else:
      if is_consonant(word, -2) and is_consonant(word, -1):
         return word
      word += 'e'
      return word
  return word


#==============#
#   stemming   #
#==============#

def stemming(list_of_words):  # a list of strings, e.g. ['fins', 'breathing', 'gills']
  """
  This is a modified version of the Krovetz stemmer.
  Krovetz, R. (1993) "Viewing morphology as an inference process" 
  in R. Korfhage et al., Proceedings of 16th ACM SIGIR Conference, 
  Pittsburgh, June 27-July 1 1993, pp. 191-202.
  """  
  new_list = []
  for original in list_of_words: # original means original word, i.e. before the stemming
    if original in all_words:
      new_list += [original]
      continue
    if stemmed.has_key(original):
      new_list += stemmed[original] # list concatenation
      continue
    if irregular.has_key(original):
      new_list += irregular[original]
      continue
    loop = 1
    for i in range(len(original)):
      if not original[i].isalpha():
        stemmed[original] = [original]
        new_list += [original]
        loop = 0
        break
    if loop == 0:
      continue
    current_word = original # default response
    current_word = remove_plural(current_word) # both 'current_word' and 'original' are strings
    if current_word in all_words:
      stemmed[original] = [current_word]
      new_list += [current_word]
      continue
    current_word = remove_past_tense(current_word)
    if current_word in all_words:
      stemmed[original] = [current_word]
      new_list += [current_word]
      continue
    current_word = remove_aspect(current_word)
    if current_word in all_words:
      stemmed[original] = [current_word]
      new_list += [current_word]
      continue

    stemmed[original] = [current_word]
    new_list += [original]
  return new_list # returns a list of stemmed words


#====================#
#   get_candidates   #
#====================#

def get_candidates(ontology):
  """
  includes a recursive function which traverses the
  trees (nested structures) in the ontology to decide
  which terms ('concepts') are 'candidates' for
  semantic matching. For instance, the following are
  unlikely to change from one version to the other
  so, they are not 'candidates':
  -- built-in SUO-KIF operators such as 'and', 'not', 
     'or', '=>' and '<=>', as well as quantifiers such
     as 'exists' and 'forall' because they are part of 
     the ontology language and not the ontology itself.
     Pease, A. (2009) "Standard Upper Ontology Knowledge 
     Interchange Format", online.
  -- fundamental SUMO concepts which we can assume as
     known to ORS and very unlikely to change. Examples
     are 'subclass', 'instance', 'documentation', 'domain',
     'attribute', 'disjoint' etc.
  """
  try:
    sys.path.append(path_to_pa+'updated')
    import nested_lists
    tup = nested_lists.nlists
  except:
    tup = parse_ontologies.nested_lists(ontology)
    f = open(path_to_pa+'updated/nested_lists.py', 'w')
    f.write('nlists = '+str(tup))
    f.close()
  """
  The above tuple contains two dictionaries, one with nested
  lists and one with comments. See the nested_lists function
  in the data/parse_ontologies.py module.
  """
  trees = tup[0]
  comments = tup[1] # will use it later
  excluded = set(['and', 'not', 'or', '=>', '<=>', 'forall', 'exists', 'subclass', 
                  'instance', 'documentation', 'domain', 'attribute', 'disjoint',
                  'equal'])
  candidates = []
  def recursive_backtracking(item): # item can be a list or a string
    if type(item) == str:
      # exclude uninstantiated variables and phrases
      if not item in excluded and not item[0] == '?' and not item[0] == '"' and not item in candidates and not is_number(item): 
        candidates.append(item)
    else:
      for thing in item:
        recursive_backtracking(thing)
  for tu in trees:
    recursive_backtracking(trees[tu])
  candidates.sort()
  # To print 'candidates' in a file,
  # comment out the following code:
  """
  f = open(path_here+'candidates.txt', 'w')
  f.write(str(candidates))
  f.close()
  """
  # candidates is a list of concepts in the input ontology
  # which are 'candidates' for semantic matching
  # e.g. ['SmokingPipe', 'Smuggling', 'Snake', 'Sober'...etc]
  return candidates 


#==================#
#   bag_of_words   #
#==================#

def bag_of_words(ontology, pa_path):
  global path_to_pa
  path_to_pa = pa_path
  #pa_path = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/'+scenario+'/PA-onts/')
  def synset_info(syns): # syns is a synset offset (e.g. '14818238')
    try:
      return synsets[syns][-1]
    except:
      return ''   
  def synset_hypernym_info(syns):
    text = ''
    try:
      for ss in synsets[syns][:-1]:  # ss is also a synset offset
        text += synset_info(ss)
    except:
      pass
    return text   
  # a list of 'candidates' for semantic matching. See get_candidates function.
  cand = get_candidates(ontology)
  # bag of words
  bow = {}
  for i in cand:
    if not bow.has_key(i):
      bow[i] = []
    if dsi.has_key(i):
      bow[i] += normalise_text(dsi[i]) # list concatenation
  """
  bow is a dictionary whose every key is a concept
  and value is a list ('bag') of words, e.g.
  'Accelerating': ['accelerate', 'increase', 'speed', 'move', 'increase', 'translocation']
  (Words can occur more than once in the list.)
  """
  for concept in cand:  
    new_conc = normalise_text(concept) # new_conc is a list
    if not bow.has_key(concept):
      bow[concept] = []
      """
      add the tokenised concept to the original concept's bag of 
      words. If it can't be tokenised, just add the word itself.
      """
    bow[concept] += new_conc
    if sumo_wordnet_mappings.has_key(concept):
      for synset in sumo_wordnet_mappings[concept]:
        bow[concept] += normalise_text(synset_info(synset))  # list concatenation  
  return bow


#=================#
#   bow_to_file   #
#=================#

def bow_to_file(bow):
  """
  writes a dictionary such as the one returned by the bag_of_words 
  function into a file in an easily readable format
  """
  f = open(path_here+'bags', 'w')
  for i in bow:
    f.write(i+'  ')
    for word in bow[i]:
      f.write(' '+word)
    f.write('\n')
  f.close()  



if __name__ == '__main__':

  end = time.time()
  running_time = end - start
  print '\nRunning time: %.3f seconds\n' % running_time



