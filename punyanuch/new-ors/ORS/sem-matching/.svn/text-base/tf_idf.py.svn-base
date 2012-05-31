#!/usr/bin/env python

"""
This module precomputes tf-idf (i.e. 'term frequency' - 
'inverse document frequency') for every word in every 
'bag'. A bag of words associated with a SUMO concept 
in the Planning Agent's ontology is treated as a 'document' 
in Information Retrieval (IR) terms, while the 'bag' 
associated  with the Service Providing Agent's concept to 
be matched is treated as a 'query'.
"""

import time
import math
import re
import os.path
import bag_of_words
import optparse



start = time.time() # this is a float



#==================#
#   inverted_bow   #
#==================#

def inverted_bow(doc_lists): # expects a dictionary
  """
  It takes as input a dictionary such as the one returned by
  the bag_of_words function in the bag_of_words module. This
  is like the 'document list' in Information Retrieval (IR), 
  where key is a document and value is a list of words in it. 
  See the 'bow' variable in the precompute_tf_idf function for
  details. The inverted_bow function returns a tuple with two
  dictionaries and an integer:
  1) --- inverted_lists ---
     (terminology borrowed from IR). Key to this dictionary is
     a word and the value is a set of 'documents' (i.e. SUMO
     concepts) where the word appears. e.g.
    'graphite': set(['Carbon', 'Pencil'])

  2) --- idfs ---
    (terminology borrowed from IR). Key to this dictionary is
     a word and the value is its 'inverse document frequency'
    i.e. the logarithm of the quotient obtained by dividing
    the number of words in the colletion (i.e. all words in
    all bags) by the number of documents which contain the
    in question. e.g.
    'graphite': 4.41512365466
    To see how idf is computer, refer to the precompute_tf_idf
    function.

  3) --- words_in_collection ---
     the number of words appearing in all 'documents', i.e. 
     the sum of the lengths of all bags for all 'candidate'
     concepts in the ontology, e.g. 52018
  """
  inverted_lists = {}
  idfs = {}
  docs_in_collection = len(doc_lists) 
  words_in_collection = 0 # all words in all bags in the ontology (initialised to 0)
  """
  in practice this means how many 'candidate' 
  concepts we have in the ontology
  """
  for doc in doc_lists: # This is like saying 'for concept in bow'
    words_in_collection += len(doc_lists[doc]) # i.e. length of the bag of words
    for word in doc_lists[doc]:
      if not inverted_lists.has_key(word):
        inverted_lists[word] = set([])
      inverted_lists[word].add(doc)
  for word in inverted_lists:
     quotient = float(docs_in_collection)/float(len(inverted_lists[word]))
     idfs[word] = math.log(quotient, 10)
  return (inverted_lists, idfs, words_in_collection)


#=======================#
#   precompute_tf_idf   #
#=======================#

def precompute_tf_idf(ontology, pa_path): # ontology should be a path
  bow = bag_of_words.bag_of_words(ontology, pa_path)
  """
  bow is a dictionary whose key-value pairs look like this:
  'Coach': ['skilled_occupation', 'occupation', 'training' etc.] 
  'Immigrating': ['human', 'nation', 'citizen' etc.]
  The key is a SUMO concept and value is a list ('bag') of 
  words associated with it.
  """
  bow_2 = {'uppercase':{}, 'lowercase':{}}
  """
  a key-value pair in bow_2['uppercase'] looks like this:
 'WindInstrument': {'case': 0.39961827947232353, 'flute': 0.79656449707700638, 
                    'played': 0.8879046248596234, 'musical_instrument': 1.2122618506057778 etc}

  'WindInstrument' is a 'candidate' concept and its value is a 
  dictionary with words in the bag as keys and a 'tf_idf' float
  (term frequency - inverse document frequency) that will later
  be multiplied by the frequency of the word in the Service Providing
  Agent's bag, for every word (This will have to be done 'online', i.e.
  during agent communication). These product of these two numbers will 
  be summed to help us compute the similarity of the concept's 'bag' 
  to the SPA's bag (The latter is seen as a 'query' in IR terms).

  The tf_idf float is the product of the multiplication of tf with idf.

  The tf for every word in every document/SUMO concept is equal to:
  wf/(wf + k*bl/abl)

  wf  --> frequency of word in the bag
  k   --> a constant, usually set to very low values
  bl  --> bag length
  abl --> average bag length in the ontology

  The idf is the same for one word no matter what the document is.
  The idf float (which is obtained from the inverted_bow function)
  is equal to:

  log(C/df)

  C  --> number of documents in the collection
  df --> number of documents which contain this word
  """
  k = 0.01  # This is a constant which we can tune to achieve better results
  tup = inverted_bow(bow)
  words_n_idfs = tup[1]
  words_in_collection = tup[2]
  average_bag_length = float(words_in_collection)/float(len(bow))
  for concept in bow: # This is like saying 'for document in document list' in IR
    if concept[0].isupper():
      index = 'uppercase'
    else:
      index = 'lowercase'
    bag = bow[concept]
    bg = bow_2[index][concept] = {}
    for word in bag:
      word_freq = bag.count(word)  
      tf_quotient = float(word_freq)/float(word_freq + k*len(bag)*average_bag_length)
      if not word in bg:
        bg[word] = tf_quotient*(words_n_idfs[word])
  return bow_2


#===================#
#   tfidf_to_file   #
#===================#

def tfidf_to_file(bow_2):
  """
  writes a dictionary such as the one returned by the 
  precompute_tf_idf function into a file in an easily 
  readable format
  """
  f = open(path_to_pa+'/updated/bags_tfidf.py', 'w')
  f.write('bow_2 = '+str(bow_2))
  f.close()


if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--ont')
  p.add_option('--sce')
  (options, arguments) = p.parse_args()
  ontology_file = options.ont # This might be something like 'Mid-level-ontology_v34.kif'
  global path_to_pa
  path_to_pa = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/'+options.sce+'/PA-onts/')
  ont_path = path_to_pa+'updated/'+ontology_file

  tfidf_to_file(precompute_tf_idf(ont_path, path_to_pa))
  """
  One command that we could give is:
  python tf_idf.py --ont 'Mid-level-ontology_v34.kif'
  """
  end = time.time()
  running_time = end - start
  #print '\nRunning time: %.3f seconds\n' % running_time



