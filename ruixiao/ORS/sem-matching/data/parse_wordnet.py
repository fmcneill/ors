#!/usr/bin/env python


import time
import re
import os.path


start = time.time() # this is a float

# the absolute path of the directory where the parse_wordnet module is
path_here = os.path.expandvars('$ORS_HOME/ORS/sem-matching/data/')  


#==========================#
#   extract_synsets_info   #
#==========================#

def extract_synsets_info():
  st1 = open(path_here+'WordNet/WordNet-3.0/data/data.noun').read()
  st2 = open(path_here+'WordNet/WordNet-3.0/data/data.verb').read()
  st3 = open(path_here+'WordNet/WordNet-3.0/data/data.adj').read()
  st4 = open(path_here+'WordNet/WordNet-3.0/data/data.adv').read()
  pat = re.compile(r'\n([^\s]+)\s[0-9]+\s[a-z]\s[0-9]+\s(([^\s]+\s[0-9]+\s)+)[^@\|]*((@\s[0-9]+\s[a-z]\s[0-9]+\s)*)[^\|]*\|([^\"\n]+)[\"|\n]')
  pat2 = re.compile(r'[0-9]+')
  pat3 = re.compile(r'[a-z]\s[0-9]+')
  l = re.findall(pat, st1)
  l += re.findall(pat, st2)
  l += re.findall(pat, st3)
  l += re.findall(pat, st4)
  l.sort()
  fl = open(path_here+'synsets_info', 'w')
  for tup in l:
    fl.write(str(tup[0])+' ')
    s = re.sub(pat3, '', tup[3])
    li = re.sub(r'@', '', s).split()
    li += re.sub(pat2, '', tup[1]).split() 
    for i in li:
      fl.write(str(i)+' ')
    last = re.sub(r'\;', '', tup[5])
    fl.write('|'+last+'\n')
  fl.close()


#=============================#
#   extract_words_n_synsets   #
#=============================#

def extract_words_n_synsets():
  st1 = open(path_here+'WordNet/WordNet-3.0/index/index.noun').read()
  st2 = open(path_here+'WordNet/WordNet-3.0/index/index.verb').read()
  st3 = open(path_here+'WordNet/WordNet-3.0/index/index.adj').read()
  st4 = open(path_here+'WordNet/WordNet-3.0/index/index.adv').read()
  pat = re.compile(r'\n([^\s]+)\s([a-z])(.*?)(([0-9]{8}\s)+)')
  l = re.findall(pat, st1)
  l += re.findall(pat, st2)
  l += re.findall(pat, st3)
  l += re.findall(pat, st4)
  l.sort()
  fl = open(path_here+'words_n_synsets', 'w')
  words = []
  for tup in l:
    words.append(tup[0])
    line = tup[0] + ' ' + tup[3] + '\n'
    #  If we also wanted to print part-of-speech information, we could write:   
    #  line = tup[0] + ' ' + tup[3] + ' ' + tup[1] + '\n'
    fl.write(line)
  fl.close()
  return words


#=======================#
#   extract_all_words   #
#=======================#

def extract_all_words():
  all_words = extract_words_n_synsets()
  all_words = list(set(all_words))  # remove duplicates
  all_words.sort()
  fl = open(path_here+'all_words', 'w')
  for word in all_words:
    fl.write(word+'\n')
  fl.close()
  return all_words # returns a list of all words excluding exceptions


#===========================#
#   extract_SUMO_mappings   #
#===========================#

def extract_SUMO_mappings():
  d = {}
  st1 = open(path_here+'WordNet/SUMO-mappings/WordNetMappings30-noun.txt').read()
  st2 = open(path_here+'WordNet/SUMO-mappings/WordNetMappings30-verb.txt').read()
  st3 = open(path_here+'WordNet/SUMO-mappings/WordNetMappings30-adj.txt').read()
  st4 = open(path_here+'WordNet/SUMO-mappings/WordNetMappings30-adv.txt').read()  
  pat = re.compile(r'\n([0-9]+)\s(.+?)((\&%[a-zA-Z0-9_]+=)+)[^\n]*\n')
  l = re.findall(pat, st1)
  l += re.findall(pat, st2)
  l += re.findall(pat, st3)
  l += re.findall(pat, st4)
  for tup in l:
    for word in re.sub(r'[\&|=|%]', '', tup[2]).split():
      if not d.has_key(word):
        d[word] = []
      d[word].append(tup[0])
  li = d.keys()
  li.sort()
  fl = open(path_here+'sumo_wordnet', 'w')
  for i in li:
    fl.write(str(i)+' ')
    for j in d[i]:
      fl.write(j+' ')
    fl.write('\n')
  fl.close()
  return d




if __name__ == '__main__':

  print path_here
  extract_synsets_info()
  '''
  The extract_all_words() function also calls extract_words_n_synsets,
  therefore, the latter does not have to be explicitly called here.
  The file that it is supposed to generate will be generated anyway.
  '''
  extract_all_words()
  extract_SUMO_mappings()
  end = time.time()
  running_time = end - start
  print '\nRunning time: %.3f seconds\n' % running_time















