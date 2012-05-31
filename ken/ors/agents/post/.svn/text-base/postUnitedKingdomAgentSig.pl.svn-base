rule(postUnitedKingdomLetter(Country,Letter,Person,Region),
     [rule1,
      [class(unitedKingdom,place),class(Region,place),class(Letter,letter),class(Country,place),class(Region,place),class(Person,person),addressed(Person,Region,Country,Letter),located(Region,unitedKingdom)],
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




nonFacts([member(_,_),assert(_),class(_,_)]).


ask([addressed(_,_,_,_), class(_,_)]).

wait([calculation(_)]).

tasksIPerform([postUnitedKingdomLetter]).
