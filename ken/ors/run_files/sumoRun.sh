killall sicstus
xterm -e sicstus -l agents/sumo/server.pl &
sleep 2
xterm -bg "#3cb371" -fg "#8b1c62" -geometry "40x15+0+0" -e sicstus -l agents/sumo/tellAgent.pl &
xterm -bg "#912cee" -fg "#ffdab9" -geometry "40x15+400+0" -e sicstus -l agents/sumo/doAAgent.pl &
xterm -bg "#ffd700" -fg "#000080" -geometry "40x15+0+270" -e sicstus -l agents/sumo/doBAgent.pl &
sleep 1
xterm -bg lightblue -geometry "+400+540" -e sicstus -l agents/planningAgent.pl &
