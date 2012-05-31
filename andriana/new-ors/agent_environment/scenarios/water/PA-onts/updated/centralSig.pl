rule(sendContaminant(Agent1,Agent2,Contaminant),
     [rule2,
      [class(Contaminant,contaminant),class(Agent1,agent),at(Agent1,Contaminant)],
      [class(Contaminant,contaminant),class(Agent2,agent),at(Agent2,Contaminant)]
    ]).

rule(sendVolume(Agent1,Agent2,Sent),
     [rule1,
      [class(Agent2,agent),class(Agent1,agent),class(Agent2,agent),class(Agent1,agent),class(Agent2,agent),number(Sent),used(Agent1,Used1),max(Agent1,Max1),used(Agent2,Used2),calculation(Used1  > Sent)],
      [class(Agent2,agent),class(Agent2,agent),calculation(Newused1 is Used1 - Sent),used(Agent1,Newused1),calculation(Newused2 is Sent + Used2 ),used(Agent2,Newused2)]
    ]).

predicate(treats(agent,contaminant)).
predicate(at(agent,contaminant)).
predicate(used(agent,number)).
predicate(max(agent,number)).

subclass(agent,thing).
subclass(contaminant,thing).
subclass(sitVar,thing).
