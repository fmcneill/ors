#!usr/bin/env python


import urllib
import re
import time
import os.path
import bag_of_words
import tf_idf
import optparse
import sys
path_to_pa = os.path.expandvars('$ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/')

start = time.time()



#=================#
#   tag_plateau   #
#=================#

def tag_plateau(url): # expects http url or local path to html file
  """
  The Tag plateau optiomisation algorithm extracts
  the content from a webpage, discarding advertisements etc.
  """
  try:
    htmlfile = urllib.urlopen(url)
  except:
    path = os.path.expandvars(url)
    htmlfile = open(path)
  text = htmlfile.read()
  text = ''.join([i for i in text if ord(i) < 128]) # remove non-ASCII characters
  pat1 = re.compile(r'<script[^>]*>.*?</script>', re.DOTALL)# Get rid of javascripts.
  pat2 = re.compile(r'<[^>]+>')
  pat3 = re.compile(r'\&nbsp;')
  pat4 = re.compile(r'\&quot;')
  pat5 = re.compile(r'-?[0-9]+-?')   # remove dates etc.
  pat6 = re.compile(r'[;|,|:|/|\.|\[|\]|\(|\)|#|\\|"|&|%|\?|\']')
  """
  Find tags, add spaces before and after them, just in case they are adjacent, 
  e.g. </body></html> (which would give one token when tokenized) and replace 
  what's inside with 't' (meaning 'tag')to get rid of spaces, e.g. <a = href.....> 
  (which will give more than one token when tokenized)
  """
  text = re.sub(pat1, r' <t> <t> ', text)
  text = re.sub(pat2, ' <t> ', text)
  text = re.sub(pat3, ' ', text)
  text = re.sub(pat4, ' ', text)
  text = re.sub(pat5, '', text)
  text = re.sub(pat6, '', text)
  tokenslist = text.split()
  ones_n_zeros = []
  Left = 0               # Number of tags on the left of 'a' (See plateau algorithm)
  Total_tags = 0         # Number of tags in the html text.
  best = 0               # maximum of Sigma + Sigma + Sigma
  A = 0                  # (best position for a)
  B = 0                  # (best position for b)
  for item in tokenslist:
    if item == '<t>':
      ones_n_zeros.append(1)
      Total_tags += 1
    else:
      ones_n_zeros.append(0)
  number_of_tokens = len(tokenslist)
  number_of_nontags = number_of_tokens - Total_tags
  c = float(Total_tags)/float(number_of_nontags)
  for a in range(number_of_tokens):
    Left += ones_n_zeros[a]  # ones_n_zeros in position [a] is 1 or 0 depending on whether it represents a tag or a word.
    Middle = 0
    Right = Total_tags - Left 
    for b in range(a + 1, number_of_tokens):
      Right -= ones_n_zeros[b]
      Middle += 1 - ones_n_zeros[b]  
      if Left + c*Middle + Right > best:
        best = Left + c*Middle + Right
        A = a
        B = b
  tokenslist = tokenslist[A:B+1]
  tokens = []
  for token in tokenslist:
    if token != '<t>':
      tokens += bag_of_words.normalise_text(token) # Remember that the normalise_text function returns a list.
  return tokens                           



#========================#
#   compute_similarity   #
#========================#

def compute_similarity(query, name, documents, kind, integer=0):
  """
  I use Information Retrieval terminology. 'query' is 
  in fact the Service Providing Agent's bag of words 
  and 'documents' will be bags of words from the 
  Planning Agent's 'candidate' concepts. The three 
  arguments of the compute_similarity function can
  be:
  > query --> either a *url*, or a local *path* or a *list*
              of words, such as the one returned by 
              the tag_plateau function
  > name --> the name of the concept whose bag of words
             is the 'query'. This is equivalent to the
             document id in Information Retrieval. It is
             not needed for the matching if we have the
             bag of words, but here we need it to create
             a file named after 'name'.
  > documents --> either the *path* to the file where tf-idf has 
                  been precomputed or a nested *dictionary* such 
                  as the one returned by the precompute_tf_idf
                  function in the tf_idf module.
  > kind --> either of the strings 'relation' and 'class-indiv'
             This tells the matcher to consider only lower-case
             concept if the concept to be matched (i.e. the name)
             is a relation/predicate. If not, only concepts starting
             with an upper-case letter will be considered.
  > integer --> this is an integer which tells the function
                how many top-ranked relevant concepts to return
                in a sorted list. This is an optional argument.
                If omitted or if set to 'ALL', the function
                returns the whole sorted list of rankings.
  """
  if kind == 'class-indiv':
    case = 'uppercase'
    name = name[0].upper()+name[1:] 
    """
    In case we want the file to-be-created to
    have a lowercase name (e.g. because in the
    prolog ontologies, everything is lowercase)
    we can comment out the above line.
    """
  elif kind == 'relation':       
    case = 'lowercase'   
  if type(query) == str:   
    query = tag_plateau(query)
  if type(documents) == str:
    sys.path.append(path_to_pa+'updated')
    import bags_tfidf
    """
    f = open(path_to_pa+'original/'+documents)
    documents = {'uppercase': {}, 'lowercase':{}}
    for line in f:
      l = line.split()
      concept = l[0]
      if concept[0].isupper():
        index = 'uppercase'
      else:
        index = 'lowercase'
      di = documents[index][concept] = {}
      for i in range(1, len(l), 2): # we iterate through the list using the even indices
        di[l[i]] = l[i+1]
    """
  """
  Now *queries* looks like this:
  ['ship', 'simulator', 'dozens', 'computer'...etc]
  *documents['uppercase']* looks like this:
  {
   'MillenniumDuration': {'millennium_duration': '1.74960866756', 'years': '1.16877329185'}
   'profit': {'business': '0.638787379144', 'profit': '0.908889203525', 'gain': '0.864829351385'}
  }
  """
  query_freqs = {} # words in the 'query' (SPA's bag of words) and their frequencies
  for word in query:
    if not query_freqs.has_key(word):
      query_freqs[word] = 1
    else:
      query_freqs[word] += 1
  ranking = []
  try:
    dictionary = bags_tfidf.bow_2[case]
  except:
    dictionary = documents[case]
  for doc in dictionary:
    words_n_tf_idf = dictionary[doc]
    similarity = 0
    intersection = set(query).intersection(set(words_n_tf_idf.keys()))
    if not intersection: # i.e. if intersection is an empty set
      continue
    for word in intersection:
      similarity += query_freqs[word]*float(words_n_tf_idf[word])
    ranking.append((-similarity, doc)) 
    """
    similarity is negated to bring higher similarities 
    up the list when the list is sorted
    """
  ranking.sort()
  if integer == 'ALL' or not integer:
    integer = len(ranking)
  ranking = ranking[:integer]
  path = path_to_pa+'original/matches/candidateLexemes.pl'
  f = open(path, 'w')
  f.write("candidates(['"+name+"', [")
  for tup in ranking[:-1]:
    f.write("'"+tup[1]+"', ")
  f.write("'"+ranking[-1][1]+"']]).")
  f.close()
  return ranking




if __name__ == '__main__':

  p = optparse.OptionParser()
  p.add_option('--url')
  p.add_option('--name')
  p.add_option('--kind')
  (options, arguments) = p.parse_args()
  url = options.url # e.g. '$ORS_HOME/semantic_matching/ServiceProvidingAgents/html/DrinkingCup.html'
  name = options.name # e.g. 'DrinkingCup'
  kind = options.kind # e.g. 'class-indiv'
  compute_similarity(url, name, 'bags_tfidf', kind, 5)
  """
  Some command that we could give are:
  python semantic_matcher.py --url $ORS_HOME/semantic_matching/ServiceProvidingAgents/html/DrinkingCup.html --name drinkingCup --kind class-indiv

  python semantic_matcher.py --url $ORS_HOME/semantic_matching/ServiceProvidingAgents/html/familyName.html --name familyName --kind relation
  """

  end = time.time()
  duration = end - start
  #print '\n\nRunning time: %.3f secs\n\n' % duration



