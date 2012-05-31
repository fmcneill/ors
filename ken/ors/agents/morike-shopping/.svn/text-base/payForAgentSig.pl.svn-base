
rule(payFor(Agent,Price,Item,TagNumber),
    [rule3,
      [hasMoney(Agent,Amount), pickedUp(Item,TagNumber)],
      [bought(Item,ReceiptNumber), calculation(NewAmount is Amount-Price),
       hasMoney(Agent,NewAmount), not(hasMoney(Agent,Amount))]
     ]).
     
     
subclass(agent,thing).
subclass(currency,thing).
subclass(francs,currency).
subclass(money,thing).
subclass(price,money).
subclass(amount,money).
subclass(newAmount,money).
subclass(number,thing).
subclass(tagNumber,number).
subclass(receiptNumber,number).
subclass(sitVar,thing).



nonFacts([member(_,_),assert(_),class(_,_)]).


ask([hasMoney(_,_), pickedUp(_,_), cost(_,_,_)]).

wait([calculation(_)]).

tasksIPerform([payFor]).


     
     
     
     
     