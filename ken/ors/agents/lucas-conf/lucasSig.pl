
rule(convertPaper(Attendee,PaperDvi,PaperPs),
     [rule1,
      [hasPaper(Attendee,PaperDvi),class(PaperDvi,paperDvi)],
      [hasPaper(Attendee,PaperPs),class(PaperPs,paperPs)]
     ]).


%rule(submitPaper(Attendee,PaperPs,Conference),
%     [rule2,
%      [hasPaper(Attendee,PaperPs),class(PaperPs,paperPs)],
%      [inform(acceptedPaper(Attendee,PaperPs,Conference,ConfirmationNo))]
%     ]).

rule(submitPaper(Attendee,PaperPs,Conference),
     [rule2,
      [hasPaper(Attendee,PaperPs),class(Paper,paperPs)],
      [inform(acceptedPaper(Attendee,PaperPs,Conference,ConfirmationNo))]
     ]).



rule(register(Attendee,Conference),
      [rule3,
       [acceptedPaper(Attendee,PaperPs,Conference,ConfirmationNo),money(Attendee,Amount),regFee(Conference,Cost),calculation(Amount>Cost)],
       [inform(registered(Attendee,Conference,RegistrationNo)),calculation(NewAmount is Amount - Cost),money(Attendee,NewAmount),not(money(Attendee,Amount))]
      ]).



rule(findConfAccomInfo(Attendee,Conference),
     [rule4,
     [registered(Attendee,Conference,RegistrationNo)],
     [inform(accomInfo(Conference,AccomodationInfo))]
    ]).



rule(bookAccom(Attendee,Conference),
     [rule5,
     [accomInfo(cade,room(single,Price)),money(Attendee,Amount),calculation(Amount>Price),class(Attendee,agent)],
     [inform(hasAccom(Attendee,AccomRef)),calculation(NewAmount is Amount-Price),money(Attendee,NewAmount),not(money(Attendee,Amount))]
    ]).



rule(bookFlight(Attendee,Conference),
     [rule6,
     [location(Attendee,AttLocation),location(Conference,ConfLocation),flight(AttLocation,ConfLocation,Price),money(Attendee,Amount),calculation(Amount>Price)],
     [inform(hasTicket(Attendee,FlightRef)),calculation(NewAmount is Amount-Price),money(Attendee,NewAmount),not(money(Attendee,Amount))]
    ]).


rule(reimburse(Attendee,Conference),
     [rule7,
      [registered(Attendee,Conference,RegNo),regFee(Conference,Fee),hasAccom(Attendee,AccomRef),accomInfo(Conference,room(single,RoomPrice)),hasTicket(Attendee,FlightRef),location(Attendee,AttLocation),location(Conference,ConfLocation),flight(AttLocation,ConfLocation,FlightPrice),calculation(TotalPrice is Fee + RoomPrice + FlightPrice),money(Attendee,Amount)],
      [calculation(NewAmount is Amount + TotalPrice),money(Attendee,NewAmount),not(money(Attendee,Amount))]
     ]).
     
nonFacts([class(_,_),member(_,_),assert(_)]).

myFacts([money(_,_),calculation(_),hasTicket(_,_),hasAccom(_,_),hasPaper(_,_),acceptedPaper(_,_,_,_)]).

transitivePreds([location(_,_)]).


subclass(agent,thing).
subclass(place,thing).
subclass(city,place).
subclass(object,thing).
subclass(event,thing).
subclass(conference,event).
subclass(paper,object).
subclass(paperDvi,paper).
subclass(paperPs,paper).
subclass(paperPdf,paper).
subclass(predicate,thing).
subclass(acceptedPaper,predicate).
subclass(accomodationInfo,predicate).
subclass(flight,predicate).
subclass(hasAccom,predicate).
subclass(hasPaper,predicate).
subclass(hasTicket,predicate).
subclass(location,predicate).
subclass(money,predicate).
subclass(dollars,money).
subclass(dollarsUS,dollars).
subclass(dollarsMalaysian,dollars).
subclass(sterling,money).
subclass(registered,predicate).
subclass(registrationFee,predicate).
subclass(airport,city).
subclass(institution,place).
