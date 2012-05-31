:- dynamic fact/1,class/2.

agentNeeded(colin,convertPaper).
agentNeeded(joe,submitPaper).
agentNeeded(alison,register).
agentNeeded(ewen,findConfAccomInfo).
agentNeeded(graham,bookAccom).
agentNeeded(dan,bookFlight).
agentNeeded(sophie,reimburse).


fact(hasPaper(lucas,isabellePaperDvi)).
fact(location(lucas,edinburgh)).
fact(location(cade,miami)).
fact(flight(edinburgh,miami,300)).
fact(regFee(cade,200)).
fact(money(lucas,1000)).
fact(accomInfo(cade,room(single,50))).




class(isabellePaperDvi,paperDvi).
class(edinburgh,city).
class(lucas,agent).
class(cade,conference).
class(edinAirport,airport).

