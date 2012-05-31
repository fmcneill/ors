
rule(bookAccom(Conference,Attendee),
     [rule5,
     [accomInfo(Conference,Price),money(Attendee,Amount),calculation(Amount>Price),class(Attendee,Agent)],
     [inform(hasAccom(Attendee,Conference,AccomRef)),calculation(NewAmount is Amount-Price),money(Attendee,NewAmount),not(money(Attendee,Amount))]
    ]).

rule(bookAccom(Attendee,Conference,Currency),
     [rule5,
     [accomInfo(Conference,Price),money(Attendee,dollars,Amount),calculation(Amount>Price),class(Attendee,Agent)],
     [inform(hasAccom(Attendee,Conference,AccomRef)),calculation(NewAmount is Amount-Price),money(Attendee,dollars,NewAmount),not(money(Attendee,dollars,Amount))]
    ]).

subclass(agent,thing).
subclass(confAgent,agent).
subclass(attendee,thing).
subclass(location,thing).
subclass(object,thing).
subclass(event,thing).
subclass(conference,event).
subclass(paper,thing).
subclass(paperDvi,paper).
subclass(paperPs,paper).
subclass(paperPdf,paper).


%nonFacts([class(_,_),member(_,_),assert(_)]).

nonFacts([member(_,_),assert(_)]).


ask([location(_,_),money(_,_,_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),acceptedPaper(_,_,_,_),class(_,_),registered(_,_,_)]).

wait([calculation(_),confirmConfNo(_,_,_)]).

tasksIPerform([bookAccom]).
