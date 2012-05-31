:-dynamic fact/1.

class(bean, beanProduct).
class(butter, dairyProduct).
class(ten,price).
class(abcke,thing).

fact(onSale(butter,abcke)).
fact(cost(butter,ten)).

pickingAgency(beanProductAgent, beanProduct).


fact(pickedUp(_,_)).

