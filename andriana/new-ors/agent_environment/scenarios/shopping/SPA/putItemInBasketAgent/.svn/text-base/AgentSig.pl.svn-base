
rule(putItemInBasket(Agent,Group,Item),
     [rule2,
      [class(Item,item),chooseItem(Agent,Item),registeredMember(Agent,Group),class(Group,shoppingGroup)],
      [inform(inBasket(Agent,Item,PseudoVar0))]
    ]).


subclass(item,thing).
subclass(group,thing).
subclass(shoppingGroup,group).
subclass(academicGroup,group).
subclass(agent,thing).
subclass(book,item).
subclass(food,item).
subclass(currency,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).



nonFacts([member,assert,class]).


ask([chooseItem,registeredMember,class]).

wait([calculation]).

tasksIPerform([putItemInBasket]).
