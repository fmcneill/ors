
rule(doB(Entity,Physical,Domain,Action,Agent),
     [rule3,
     [resultRel(Action,Object),class(Object,object),performs(Agent,Action),domainRel(Action,Domain)],
     [refers(Physical,Entity)]
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
subclass(domains,thing).
subclass(object,entity).



%nonFacts([class(_,_),member(_,_),assert(_)]).


nonFacts([member(_,_),assert(_)]).


ask([resultRel(_,_),performs(_,_),domainRel(_,_)]).

wait([calculation(_)]).

tasksIPerform([doB]).
