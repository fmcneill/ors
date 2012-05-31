
killall sicstus

xterm -bg "#FFFFFF" -geometry "60x5" -e sicstus --noinfo -l $ORS_HOME/agent_environment/server.pl &
sleep 1

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+300+200" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/water2/SPA/plantOne/Agent.pl &

sleep 1

xterm -bg lightblue -geometry "85x27+580+200" -e sicstus --noinfo --nologo -l $ORS_HOME/agent_environment/PA/planningAgent.pl  --goal "assert(scenario_name(water2)),assert(goal(at(plantZero,alpha))),assert(ontoType(onto)),assert(experience_sharing(off))." &
