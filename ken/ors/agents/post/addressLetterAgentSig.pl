rule(address(Letter,LocationAddress,Person,Region),
     [rule3,
      [class(Region,place),class(Person,person),class(Country,country),class(Letter,letter)],
      [class(Letter,letter),class(Region,place),class(Person,person),inform(addressed(Person,Region,Country,Letter))]
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



nonFacts([member,assert,class]).


ask([located,class]).

wait([calculation]).

tasksIPerform([address]).
