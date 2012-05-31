
killall sicstus

xterm -bg "#FFFFFF" -geometry "60x5" -e sicstus --noinfo -l $ORS_HOME/agent_environment/server.pl &
sleep 1

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+300+200" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/shopping/SPA/buyAgent/Agent.pl &




xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+20+200" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/shopping/SPA/joinGroupAgentOne/Agent.pl &

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+300+400" -e sicstus --noinfo -l $ORS_HOME/ORS/refinement/Ontolingua_refinement/works.pl &

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+20+400" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/shopping/SPA/putInBasketAgent/Agent.pl &

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+300+600" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/shopping/SPA/putItemInBasketAgent/Agent.pl &

sleep 1

xterm -sb -bg lightblue -geometry "85x27+580+200" -e sicstus --noinfo --nologo -l $ORS_HOME/agent_environment/PA/planningAgent.pl  --goal "assert(scenario_name(shopping)),assert(goal(has(shoppingAgent,ourMutualFriend))),assert(ontoType(onto)),assert(experience_sharing(off))." &
