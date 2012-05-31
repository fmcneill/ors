
rule(tell(Communication,Agent,Physical,Entity),
     [rule2,
     [represents(Physical,Entity),patient(Communication,Physical),destination(Agent,Communication)],
     [class(Communication,communication),knows(Communication,Agent)]
    ]).


subclass(action,thing).
subclass(agent,thing).
subclass(cognitiveAgent,agent).
subclass(pseudoRel,agent).
subclass(location,thing).
subclass(communication,thing).
subclass(entity,thing).
subclass(physical,thing).
subclass(transfer,paper).
subclass(refers,represents).


%nonFacts([class(_,_),member(_,_),assert(_)]).


nonFacts([member(_,_),assert(_)]).


ask([represents(_,_),destination(_,_),patient(_,_)]).

wait([calculation(_)]).

tasksIPerform([tell]).
