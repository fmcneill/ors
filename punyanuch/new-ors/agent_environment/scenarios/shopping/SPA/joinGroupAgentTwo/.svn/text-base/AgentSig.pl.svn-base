
% rule(joinGroup(Agent,Group),
%      [rule1,
%       [interest(Group,Agent)],
%        [registeredMember(Agent,Group)]
%      ]).

rule(joinGroup(Agent,Group),
      [rule1,
       [],
       [registeredMember(Agent,Group)]
     ]).


subclass(item,thing).
subclass(group,thing).
subclass(shoppingGroup,group).
subclass(academicGroup,group).
subclass(agent,thing).
subclass(book,item).
subclass(currency,thing).
subclass(confirmationNumber,thing).
subclass(sitVar,thing).




nonFacts([member,assert,class]).


ask([interest]).

wait([calculation]).

tasksIPerform([joinGroup]).
