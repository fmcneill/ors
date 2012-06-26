myFacts([]).

transitivePreds([]).

inform([inBasket]).

agentNeeded(joinGroupAgentTwo,joinGroup).
agentNeeded(putInBasketAgent,putInBasket).
agentNeeded(putItemInBasketAgent,putItemInBasket).
agentNeeded(buyAgent,buy).


predSubclass(chooseItem,predicate).
predSubclass(chooseThing,chooseItem).
predSubclass(waitFact,predicate).
predSubclass(predicate,thing).
predSubclass(action,thing).
predSubclass(inform,predicate).
predSubclass(agent,thing).
predSubclass(askFact,predicate).
predSubclass(functionOption,thing).
predSubclass(argumentOption,thing).
predSubclass(protectionLevel,thing).


protectFunction([]).

protectArgument([]).

tasksIPerform([]).

ask([]).

wait([]).

nonFacts([]).