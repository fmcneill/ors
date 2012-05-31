
rule(addToBag(Agent,Item),
     [rule4,
       [paidFor(Agent,Item), class(Agent, agent)],
       [buy(ourAgent, butter)]
      ]).

subclass(agent, thing).
subclass(ourAgent, agent).
subclass(number, thing).
subclass(receiptNumber, number).
subclass(sitVar, thing).



nonFacts([member(_,_),assert(_),class(_,_)]).


ask([paidFor(_,_), class(_,_)]).

wait([calculation(_)]).

tasksIPerform([addToBag]).
