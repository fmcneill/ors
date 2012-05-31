rule(postIrelandLetter(Letter,LocationAddress,Person,Region),
     [rule1,
      [class(LocationAddress,irelandAddress),class(Region,place),class(Letter,letter),class(Region,place),class(Person,person),addressed(Person,Region,LocationAddress,Letter)],
      [class(Letter,letter),class(Person,person),sent(Person,Letter)]
    ]).



subclass(agent,thing).
subclass(place,thing).
subclass(country,place).
subclass(region,country).
subclass(town,region).
subclass(office,thing).
subclass(person,thing).
subclass(letter,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).
subclass(locationAddress,thing).
subclass(irelandAddress,locationAddress).
subclass(unitedKingdomAddress,locationAddress).
subclass(scotlandAdress,unitedKingdomAddress).




nonFacts([member(_,_),assert(_),class(_,_)]).


ask([addressed(_,_,_,_), class(_,_)]).

wait([calculation(_)]).

tasksIPerform([postIrelandLetter]).
