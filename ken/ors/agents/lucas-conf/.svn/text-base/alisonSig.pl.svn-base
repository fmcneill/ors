rule(register(Agent,Conference,Currency,Paper),
     [rule1,
      [acceptedPaper(Agent,Paper,Conference,ConfirmationNo),money(Agent,dollars,Amount),registrationFee(Conference,Cost),calculation(Cost  < Amount),confirmConfNo(Agent,Paper,ConfirmationNo)],
      [registered(Agent,Conference,PseudoVar),calculation(Newamount is Amount - Cost),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
    ]).

rule(register(Agent,Conference,Paper),
     [rule1,
      [acceptedPaper(Agent,Paper,Conference,ConfirmationNo),money(Agent,dollars,Amount),registrationFee(Conference,Cost),calculation(Cost  < Amount),confirmConfNo(Agent,Paper,ConfirmationNo)],
      [registered(Agent,Conference,PseudoVar),calculation(Newamount is Amount - Cost),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
    ]).



subclass(agent,thing).
subclass(attendee,thing).
subclass(location,thing).
subclass(object,thing).
subclass(event,thing).
subclass(conference,event).
subclass(paper,thing).
subclass(paperDvi,paper).
subclass(paperPs,paper).
subclass(paperPdf,paper).
subclass(institution,place).


nonFacts([class(_,_),member(_,_),assert(_)]).

ask([location(_,_),money(_,_,_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),acceptedPaper(_,_,_,_)]).

wait([calculation(_),confirmConfNo(_,_,_)]).

tasksIPerform([register]).
