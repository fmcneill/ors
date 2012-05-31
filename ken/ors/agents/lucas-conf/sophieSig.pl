rule(reimburse(ConfLoc,AgentLoc,Conference,Agent),
     [rule5,
      [registered(Agent,Conference,RegNo),registrationFee(Conference,ConfCost),hasAccom(Agent,Conference,AccomRef),accomodationInfo(Conference,RoomCost),hasTicket(Agent,FlightRef),location(Agent,AgentLoc),location(Conference,ConfLoc),flight(AgentLoc,ConfLoc,FlightCost),money(Agent,Amount)],
      [attendConference(Agent,Conference),calculation(Total is FlightCost + RoomCost  + ConfCost ),calculation(Newamount is Total + Amount ),money(Agent,Newamount),not(money(Agent,Amount))]
    ]).

rule(reimburse(Agent,AgentLoc,ConfLoc,Conference,Currency),
     [rule5,
      [registered(Agent,Conference,RegNo),registrationFee(Conference,ConfCost),hasAccom(Agent,Conference,AccomRef),accomodationInfo(Conference,RoomCost),hasTicket(Agent,FlightRef),location(Agent,AgentLoc),location(Conference,ConfLoc),flight(AgentLoc,ConfLoc,FlightCost),money(Agent,dollars,Amount)],
      [attendConference(Agent,Conference),calculation(Total is FlightCost + RoomCost  + ConfCost ),calculation(Newamount is Total + Amount ),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
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
subclass(dollars,money).


nonFacts([class(_,_),member(_,_),assert(_)]).

ask([location(_,_),money(_,_,_),hasTicket(_,_),hasAccom(_,_,_),hasPaper(_,_),acceptedPaper(_,_,_,_),registered(_,_,_),registrationFee(_,_),accomodationInfo(_,_),dollars(_,_)]).

wait([calculation(_),confirmConfNo(_,_,_)]).

tasksIPerform([reimburse]).
