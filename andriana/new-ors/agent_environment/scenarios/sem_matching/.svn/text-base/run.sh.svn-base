
killall sicstus

xterm -bg "#FFFFFF" -geometry "60x3" -e sicstus --noinfo -l $ORS_HOME/agent_environment/server.pl  &
sleep 1

xterm -bg "#52657F" -fg "#FFFFFF" -geometry "40x12+300+200" -e sicstus --noinfo -l $ORS_HOME/agent_environment/scenarios/sem_matching/SPA/tomRecruiterAgent/Agent.pl &

sleep 1

xterm -bg "#AFCCF4" -fg "#000000" -geometry "85x27+580+200" -e sicstus --noinfo --nologo -l $ORS_HOME/agent_environment/PA/planningAgent.pl --goal "assert(scenario_name(sem_matching)),assert(semantics(not_processed)),assert(goal(employs(scottishNationalGalleryOfModernArt,jerryTheBot))),assert(ontoType(sumo)),assert(experience_sharing(off))." &
