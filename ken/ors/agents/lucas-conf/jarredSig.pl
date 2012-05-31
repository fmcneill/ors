
rule(findConfAccomInfo(Attendee,Conference),
     [rule4,
     [registered(Attendee,Conference,RegistrationNo),confirmRegNo(Attendee,RegistrationNo)],
     [inform(accomInfo(Conference,AccomodationInfo))]
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


nonFacts([class(_,_),member(_,_),assert(_)]).

ask([location(_,_),money(_,_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),acceptedPaper(_,_,_,_)]).

wait([calculation(_),confirmRegNo(_,_,_)]).

tasksIPerform([findConfAccomInfo]).
