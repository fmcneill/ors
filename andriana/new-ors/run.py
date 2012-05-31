import os


def create_run_command(scenario_name, spas, goal, onto_type): 
  # spas is a list of strings and the rest of the args are strings // onto_type can be either 'sumo' or 'onto' (i.e. ontolingua)
  geometry = ['40x12+300+200', '40x12+20+200', '40x12+300+400', '40x12+20+400', '40x12+300+600', '40x12+20+600', '40x12+300+800', '40x12+20+800', '40x12+580+800', '40x12+580+600', '40x12+860+800', '40x12+860+600']

  def write_spas():
    number_of_spas = 0
    part_of_command = ''
    for spa in spas:
      spa_line = 'xterm -bg "#52657F" -fg "#FFFFFF" -geometry "' + geometry[number_of_spas] + '" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/' + scenario_name + '/SPA/' + spa + '/Agent.pl &\n\n'
      number_of_spas += 1
      part_of_command += spa_line
    return part_of_command 

  run_command = '\nkillall sicstus\n\nxterm -geometry "0x0" -e sicstus -Xrs -l $ORS_HOME/agent_environment/server_create.pl &\nsleep 1\n\nxterm -bg "#FFFFFF" -geometry "60x5" -e sicstus --noinfo -l $ORS_HOME/agent_environment/server.pl &\nsleep 1\n\n' + write_spas() + 'sleep 1\n\nxterm -bg "#AFCCF4" -fg "#000000" -geometry "85x27+580+200" -e sicstus --noinfo --nologo -l $ORS_HOME/agent_environment/PA/planningAgent.pl --goal "assert(scenario_name(%s)),assert(goal(%s)),assert(ontoType(%s))." &\n\nsleep 2\n\n' % (scenario_name, goal, onto_type)

  f = open('run.sh', 'w')
  f.write(run_command)
  f.close()
  os.system('chmod +x run.sh')
  return run_command # We will only need the returned object if we want to launch ORS from Python, e.g. with os.system(create_run_command(ontology_type))




#create_run_command('shopping', ['buyAgent', 'joinGroupAgentOne', 'joinGroupAgentTwo', 'putInBasketAgent', 'putItemInBasketAgent'], 'has(shoppingAgent,ourMutualFriend)', 'onto')

create_run_command('sem_matching', ['tomRecruiterAgent'], 'employs(scottishNationalGalleryOfModernArt,jerryTheBot)', 'sumo')


