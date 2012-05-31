killall sicstus
xterm -e sicstus -l agents/shopping/server.pl &
sleep 2
xterm -bg "#3cb371" -fg "#8b1c62" -geometry "40x12+0+0" -e sicstus -l agents/shopping/joinGroupAgentOne.pl &
xterm -bg "#912cee" -fg "#ffdab9" -geometry "40x12+300+0" -e sicstus -l agents/shopping/joinGroupAgentTwo.pl &
xterm -bg "#ffd700" -fg "#000080" -geometry "40x12+0+200" -e sicstus -l agents/shopping/buyAgent.pl &
xterm -bg "#ff6347" -fg "#2f4f4f" -geometry "40x12+300+200" -e sicstus -l agents/shopping/putInBasketAgent.pl &

sleep 1
# xterm -bg lightblue -geometry "50x20+300+200" -fn "-*-lucidatypewriter-*-r-*-*-*-240-*-*-*-*-*-*" -e sicstus -l agents/shopping/lucas.pl &

xterm -bg lightblue -geometry "+300+400" -e sicstus -l agents/planningAgent.pl &