killall sicstus
xterm -e sicstus -l agents/morike-shopping/server.pl &
sleep 2
xterm -bg "#3cb371" -fg "#8b1c62" -geometry "40x15+0+0" -e sicstus -l agents/morike-shopping/addToBagAgent.pl &
xterm -bg "#912cee" -fg "#ffdab9" -geometry "40x15+400+0" -e sicstus -l agents/morike-shopping/beanProductAgent.pl &
xterm -bg "#ffd700" -fg "#000080" -geometry "40x15+0+270" -e sicstus -l agents/morike-shopping/dairyProductAgent.pl &
xterm -bg "#ff6347" -fg "#2f4f4f" -geometry "40x15+400+270" -e sicstus -l agents/morike-shopping/payForAgent.pl &
sleep 1
xterm -bg lightblue -geometry "+400+540" -e sicstus -l agents/planningAgent.pl &
