
rule(doA(Entity,Physical,Communication,Action,Agent),
     [rule1,
     [resultRel(Action,Entity),performs(Agent,Action),patient(Communication,Physical)],
     [represents(Physical,Entity)]
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



%nonFacts([class(_,_),member(_,_),assert(_)]).


nonFacts([member(_,_),assert(_)]).


ask([resultRel(_,_),performs(_,_),patient(_,_)]).

wait([calculation(_)]).

tasksIPerform([doA]).
