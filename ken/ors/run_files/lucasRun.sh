killall sicstus
xterm -e sicstus -l agents/lucas-conf/server.pl &
sleep 2
xterm -bg "#3cb371" -fg "#8b1c62" -geometry "40x15+0+0" -e sicstus -l agents/lucas-conf/colin.pl &
xterm -bg "#912cee" -fg "#ffdab9" -geometry "40x15+400+0" -e sicstus -l agents/lucas-conf/joe.pl &
xterm -bg "#ffd700" -fg "#000080" -geometry "40x15+0+270" -e sicstus -l agents/lucas-conf/alison.pl &
xterm -bg "#ff6347" -fg "#2f4f4f" -geometry "40x15+400+270" -e sicstus -l agents/lucas-conf/jarred.pl &
xterm -bg "#8b1a1a" -fg "#fdf5e6" -geometry "40x15+800+0" -e sicstus -l agents/lucas-conf/graham.pl &
xterm -bg "#00cdcd" -fg "#8b4726" -geometry "40x15+800+270" -e sicstus -l agents/lucas-conf/dan.pl &
xterm -bg "#ff1493" -fg "#fffacd" -geometry "40x15+0+540" -e sicstus -l agents/lucas-conf/sophie.pl &
sleep 1
xterm -bg lightblue -geometry "+400+540" -e sicstus -l agents/planningAgent.pl &