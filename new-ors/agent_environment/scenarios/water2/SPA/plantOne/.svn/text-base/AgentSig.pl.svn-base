% rule(sendWater(Agent1,Agent2,Contaminant),
%      [rule1,
%       [class(Contaminant,contaminant),class(Agent2,agent),class(Contaminant,contaminant),class(Agent2,agent),class(Contaminant,contaminant),class(Agent2,agent),class(Contaminant,contaminant),class(Agent2,agent),at(Agent1,Contaminant),not(at(Agent2,Contaminant)),not(treats(Agent1,Contaminant)),treats(Agent2,Contaminant)],
%       [class(Contaminant,contaminant),class(Agent1,agent),class(Contaminant,contaminant),class(Agent1,agent),at(Agent2,Contaminant),not(at(Agent1,Contaminant))]
%     ]).


rule(sendWater(Agent1,Agent2,Contaminant,Contaminant2),
      [rule1,
       [class(Contaminant,contaminant),class(Contaminant2,contaminant),class(Agent2,agent),at(Agent1,Contaminant),treats(Agent2,Contaminant)],
       [class(Contaminant,contaminant),class(Agent1,agent),class(Contaminant,contaminant),class(Agent1,agent),at(Agent2,Contaminant),at(Agent2,Contaminant2)]
     ]).

rule(sendWater(Agent1,Agent2,Contaminant),
      [rule1,
       [class(Contaminant,contaminant),class(Agent2,agent),at(Agent1,Contaminant),treats(Agent2,Contaminant)],
       [class(Contaminant,contaminant),class(Agent1,agent),class(Contaminant,contaminant),class(Agent1,agent),at(Agent2,Contaminant)]
     ]).


predicate(treats(agent,contaminant)).
predicate(at(agent,contaminant)).

subclass(agent,thing).
subclass(water,thing).
subclass(contaminant,thing).

tasksIPerform([sendWater]).


ask([treats]).

wait([calculation]).

nonFacts([]).
