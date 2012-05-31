
rule(submitPaper(Agent,Conference,PaperPs),
     [rule2,
      [hasPaper(Agent,PaperPs),class(PaperPs,psPaper)],
      [acceptedPaper(PaperPs,Conference,ConfirmationNo)]
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

ask([location(_,_),money(_,_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),class(_,_)]).

wait([calculation(_)]).

tasksIPerform([submitPaper]).
