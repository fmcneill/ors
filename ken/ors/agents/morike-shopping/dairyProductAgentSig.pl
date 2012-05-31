
rule(pickUpDairy(Butter,Price,TagNumber),
    [rule1,
      [onSale(Item,TagNumber),class(Item,dairyProduct),cost(Item,Price)],
      [pickedUp(Item,TagNumber)]
    ]).

subclass(agent,thing).
subclass(dairyProductAgent,agent).
subclass(dairyProduct,item).
subclass(agency,thing).
subclass(pickingAgency,agency).
subclass(money,thing).
subclass(price,money).
subclass(amount,money).
subclass(newAmount,money).
subclass(tagNumber,number).
subclass(sitVar,thing).



nonFacts([member(_,_),assert(_),class(_,_)]).


ask([onSale(_,_), instance(_,_), cost(_,_,_)]).

wait([calculation(_)]).

tasksIPerform([pickUpDairy]).

