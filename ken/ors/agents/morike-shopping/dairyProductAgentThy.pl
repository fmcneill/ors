:-dynamic fact/1.


class(bean,beanProduct).
class(butter,dairyProduct).
class(ten,price).
class(abcke,thing).

fact(onSale(butter,abcke)).
fact(cost(butter,ten)).




%onSale(butter, 77).
%cost(butter, 77, 20).
%class(butter, dairyProduct).
pickingAgency(dairyProductAgent, dairyProduct).


fact(pickedUp(_,_)).


