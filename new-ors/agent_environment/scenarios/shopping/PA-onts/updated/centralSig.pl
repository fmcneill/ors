rule(buy(Agent,Item,MetaVar),
     [rule3,
      [class(Item,item),class(MetaVar,currency),class(Agent,agent),class(Item,item),class(Agent,agent),inBasket(Agent,Item,PseudoVar0),money(Agent,MetaVar,Amount),cost(Item,Price),calculation(Price  < Amount)],
      [class(MetaVar,currency),class(Agent,agent),class(MetaVar,currency),class(Agent,agent),class(Item,item),class(Agent,agent),has(Agent,Item),calculation(Newamount is Amount - Price),money(Agent,MetaVar,Newamount),not(money(Agent,MetaVar,Amount))]
    ]).

rule(putInBasket(Agent,Group,Item),
     [rule2,
      [class(Group,group),class(Agent,agent),class(Item,item),class(Agent,agent),chooseThing(Agent,Item),registeredMember(Agent,Group),class(Group,group),class(Group,shoppingGroup)],
      [class(Item,item),class(Agent,agent),inform(inBasket(Agent,Item,PseudoVar0))]
    ]).

rule(joinGroup(Agent,Group),
     [rule1,
      [class(Group,group)],
      [class(Group,group),class(Agent,agent),registeredMember(Agent,Group)]
    ]).

predicate(has(agent,item)).
predicate(inBasket(agent,item,confirmationNumber)).
predicate(chooseItem(agent,item)).
predicate(chooseThing(agent,item)).
predicate(registeredMember(agent,group)).
predicate(cost(item,number)).
predicate(money(agent,currency,number)).
predicate(sterling(agent,number)).

subclass(item,thing).
subclass(group,thing).
subclass(shoppingGroup,group).
subclass(academicGroup,group).
subclass(agent,thing).
subclass(book,item).
subclass(currency,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).
