myFacts([]).

transitivePreds([]).

inform([]).

agentNeeded(plantOne,sendWater).
agentNeeded(plantZero,sendWater).


predSubclass(action,thing).
predSubclass(inform,predicate).
predSubclass(functionOption,thing).
predSubclass(argumentOption,thing).
predSubclass(protectionLevel,thing).


protectFunction(['(treats,functionAll,highProtection)']).

protectArgument(['(treats,3,argumentAll,lowProtection)']).

tasksIPerform([]).

ask([]).

wait([]).

nonFacts([]).