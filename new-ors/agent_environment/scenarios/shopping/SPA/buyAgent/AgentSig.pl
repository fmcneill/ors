

rule(buy(Agent,Item),
     [rule3,
      [inBasket(Agent,Item,PseudoVar0),money(Agent,Currency,Amount),cost(Item,Price),calculation(Price  < Amount)],
      [inform(has(Agent,Item)),calculation(Newamount is Amount - Price),money(Agent,Currency,Newamount),not(money(Agent,Currency,Amount))]
    ]).


rule(buy(Agent,Item,Currency),
     [rule3,
      [inBasket(Agent,Item,PseudoVar0),money(Agent,Currency,Amount),cost(Item,Price),calculation(Price  < Amount)],
      [inform(has(Agent,Item)),calculation(Newamount is Amount - Price),money(Agent,Currency,Newamount),not(money(Agent,Currency,Amount))]
    ]).


%rule(register(Conference,Currency,Agent,Paper),
%     [rule1,
%      [acceptedPaper(Agent,Paper,Conference,ConfirmationNo),money(Agent,dollars,Amount),registrationFee(Conference,Cost),calculation(Cost  < Amount),confirmConfNo(Agent,Paper,ConfirmationNo)],
%      [registered(Agent,Conference,PseudoVar),calculation(Newamount is Amount - Cost),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
%    ]).

%rule(register(Conference,Agent,Paper),
%     [rule1,
%      [acceptedPaper(Agent,Paper,Conference,ConfirmationNo),money(Agent,dollars,Amount),registrationFee(Conference,Cost),calculation(Cost  < Amount),confirmConfNo(Agent,Paper,ConfirmationNo)],
%      [registered(Agent,Conference,PseudoVar),calculation(Newamount is Amount - Cost),money(Agent,dollars,Newamount),not(money(Agent,dollars,Amount))]
%    ]).


subclass(item,thing).
subclass(group,thing).
subclass(shoppingGroup,group).
subclass(academicGroup,group).
subclass(agent,thing).
subclass(book,item).
subclass(currency,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).

predicate(money(agent,currency,amount)).



nonFacts([member,assert,class]).


ask([money,inBasket]).

wait([calculation]).

tasksIPerform([buy]).
