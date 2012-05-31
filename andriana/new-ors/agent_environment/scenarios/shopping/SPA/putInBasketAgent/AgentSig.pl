
% rule(putInBasket(Agent,Group,Item),
%       [rule2,
%        [chooseThing(Agent,titchmarshMemoirs),registeredMember(Agent,Group),class(Group,shoppingGroup)],
%        [inform(inBasket(Agent,Item,PseudoVar0))]
%      ]).

rule(putInBasket(Agent,Group,Item),
     [rule2,
      [chooseItem(Agent,Item),registeredMember(Agent,Group),class(Group,shoppingGroup)],
      [inform(inBasket(Agent,Item,PseudoVar0))]
    ]).


subclass(item,thing).
subclass(group,thing).
subclass(shoppingGroup,group).
subclass(academicGroup,group).
subclass(agent,thing).
subclass(book,item).
subclass(gardeningBook,book).
subclass(currency,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).



nonFacts([member,assert,class]).


ask([chooseItem,chooseThing,registeredMember,class]).

wait([calculation]).

tasksIPerform([putInBasket]).
