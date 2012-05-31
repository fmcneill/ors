rule(bookFlight(ConfLoc,AgentLoc,Agent,Conference),
     [rule2,
      [location(Agent,AgentLoc),location(Conference,ConfLoc),flight(AgentLoc,ConfLoc,Price),money(Agent,Amount),calculation(Price  < Amount)],
      [inform(hasTicket(Agent,PseudoVar)),calculation(Newamount is Amount - Price),money(Agent,Newamount),not(money(Agent,Amount))]
    ]).

rule(bookFlight(Agent,AgentLoc,ConfLoc,Conference,Currency),
     [rule2,
      [location(Agent,AgentLoc),location(Conference,ConfLoc),flight(AgentLoc,ConfLoc,Price),money(Agent,dollars,Amount),calculation(Price  < Amount)],
      [inform(hasTicket(Agent,PseudoVar)),calculation(Newamount is Amount - Price),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
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

ask([location(_,_),money(_,_,_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),acceptedPaper(_,_,_,_)]).


wait([calculation(_),confirmConfNo(_,_,_)]).

tasksIPerform([bookFlight]).
