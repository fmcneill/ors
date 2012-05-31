cp ontologies/ken-shopping/ont.in2 ont.in
cp ontologies/ken-shopping/metaOnt.in .
cp agents/shopping/putInBasketAgentSig.pl2 agents/shopping/putInBasketAgentSig.pl

killall sicstus
xterm -e sicstus -l agents/shopping/server.pl &
sleep 2
xterm -bg "#3cb371" -fg "#8b1c62" -geometry "40x12+0+0" -e sicstus -l agents/shopping/joinGroupAgentOne.pl &
xterm -bg "#912cee" -fg "#ffdab9" -geometry "40x12+300+0" -e sicstus -l agents/shopping/joinGroupAgentTwo.pl &
xterm -bg "#ffd700" -fg "#000080" -geometry "40x12+0+200" -e sicstus -l agents/shopping/buyAgent.pl &
xterm -bg "#ff6347" -fg "#2f4f4f" -geometry "40x12+300+200" -e sicstus -l agents/shopping/putInBasketAgent.pl &
xterm -bg "#ff6347" -fg "#2f4f4f" -geometry "40x12+0+400" -e sicstus -l agents/shopping/putItemInBasketAgent.pl &

sleep 1

xterm -bg "#3cb371" -geometry "+800+0" -e sicstus -l agents/expServerAgent.pl &

sleep 1

xterm -bg lightblue -geometry "+300+400" -e sicstus -l agents/planningAgent.pl &
