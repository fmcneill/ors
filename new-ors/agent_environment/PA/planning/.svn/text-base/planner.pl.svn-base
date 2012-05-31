%findPlan(+Goals,-Plan)
% sets up findPlan in the required format and instantiates StateChanges to the empty list.
% Note that the current state consists of a combination of the facts, classes and class hierarchy asserted in the ontology, and the list of statechanges which is carried around.  A fact is true if: 1) it is a member of StateChanges or 2) it is a fact asserted in the original ontology, and its negation is not a member of StateChanges.

findPlan(Goals,Plan) :-
	findPlan(goals(start,Goals),[],Plan,_FinalState).

%findPlan(+Goals,+StateChanges,-Plan,-FinalStateChanges)

findPlan(goals(_,Goals),StateChanges,[],StateChanges) :-
	satisfied(Goals,StateChanges).


% The plan is decomposed into stages by conc, the precondtion plan (PrePlan) is found in breadth-first fashion.  However, the length of the rest of the plan is not restricted and goals are achieved in depth-first style.

% note that the slightly weird passing around of the action name with the list of goals is for the following reason: when 'choose' finds a goal, it binds all possible variables in the rest of the goals, according to the state at the time.  But we do not want to carry around this list of largely bound goals, as ultimately we want them to be bound according to whatever the current state is, and not the state at the time the 'choose' predicate was called.  So by carrying round the action name, we can retrieve these goals unsullied from the action definition when necessary.  This is possible because the list of goals is always either the list of preconds for an action, or the list of original goals (in which case we invent a false action 'start' with no preconditions).  The process will terminate without this, and discover the correct bindings through backtracking, but it is *very* slow.

findPlan(goals(PrevAction,Goals),StateChanges,Plan,FinalState) :-
	conc(PrePlan,[Action|PostPlan],Plan),
	choose(StateChanges,Goals,Goal),
	findPreconditions(PrevAction,UninstGoals),
	achieves(Action,Goal),
	findPreconditions(Action,Condition),
	findPlan(goals(Action,Condition),StateChanges,PrePlan,MidStateChanges),
	apply(MidStateChanges,Action,NewStateChanges),
	findPlan(goals(PrevAction,UninstGoals),NewStateChanges,PostPlan,FinalState).


%findPreconditions(+Action,-Preconds)

findPreconditions(start,[]).

findPreconditions(Action,Condition) :-
	rule(Action,[_ruleNo,Condition,_Effects]).



%satisfied(+Goals): succeeds if all goals are true 

satisfied([],_StateChanges).


satisfied([class(Object,Class)|Goals],StateChanges) :-  
	checkClass(class(Object,Class)),
	satisfied(Goals,StateChanges).

satisfied([calculation(Sum)|Goals],StateChanges) :-  
	Sum,
	satisfied(Goals,StateChanges).

% a negative fact is satisfied if it is either not true originally and has not been made true, or if it was true originally and has been made false.
	
satisfied([not(Fact)|Goals],StateChanges) :-
	(   \+ fact(Fact),
	    \+ member(Fact,StateChanges)
	;
	    fact(Fact),
	    member(not(Fact),StateChanges)
	),
	satisfied(Goals,StateChanges).

% a fact is satisfied if it is either stated and not negated in statechanges, or if it is made true in statechanges.

satisfied([Fact|Goals],StateChanges) :-
	(   member(Fact,StateChanges)
	;
	    \+ member(not(Fact),StateChanges),
	    fact(Fact)
	),
	satisfied(Goals,StateChanges).




% choose(+StateChanges,+Goals,-Goal)
% chooses a goal from the list which is not satisfied in the current state.  There must be at least one.  Goes through all the preconditions to check that the bindings of variables are valid with respect to all the preconditions.

choose(StateChanges,Goals,Goal) :-
	choose(StateChanges,Goals,[Goal|_Unsatisfied],[]).

choose(_StateChanges,[],Unsatisfied,Unsatisfied).

choose(StateChanges,[class(Object,Class)|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	checkClass(class(Object,Class)),
	choose(StateChanges,Rest,Unsatisfied,UnsatisfiedSoFar).

choose(StateChanges,[calculation(_Sum)|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	choose(StateChanges,Rest,Unsatisfied,UnsatisfiedSoFar).

choose(StateChanges,[not(FirstGoal)|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	(   fact(not(FirstGoal))
	;
	    \+ fact(FirstGoal)
	),
	\+ member(FirstGoal,StateChanges),
	choose(StateChanges,Rest,Unsatisfied,UnsatisfiedSoFar).

choose(StateChanges,[FirstGoal|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	member(FirstGoal,StateChanges),
	choose(StateChanges,Rest,Unsatisfied,UnsatisfiedSoFar).

choose(StateChanges,[FirstGoal|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	fact(FirstGoal),
	\+ member(not(FirstGoal),StateChanges),
	choose(StateChanges,Rest,Unsatisfied,UnsatisfiedSoFar).

choose(StateChanges,[FirstGoal|Rest],Unsatisfied,UnsatisfiedSoFar) :-
	\+ FirstGoal = class(_Object,_Class),
	\+ FirstGoal = calculation(_Sum),
	\+ fact(FirstGoal),
	\+ member(FirstGoal,StateChanges),
	choose(StateChanges,Rest,Unsatisfied,[FirstGoal|UnsatisfiedSoFar]).



%achieves(-Action,+Goal): finds an action which makes Goal true (for which Goal is in its effects)

achieves(_Action,Goal) :-
	fact(Goal).

achieves(Action,Goal) :-
	rule(Action,[_ruleNo,_Preconds,Effects]),
	(   member(Goal,Effects)
	;
	    member(inform(Goal),Effects)
	).


%apply(+StateChanges,+Action,-NewStateChanges): updates the state according to the effects of the action which has just been performed.

apply(StateChanges,Action,NewStateChanges) :-
	rule(Action,[_ruleNo,Preconds,Effects]),
	instantiateVars(Preconds,StateChanges,Effects),
	processEffects(StateChanges,Effects,NewStateChanges).


%instantiateVars(+Preconds,+StateChanges,+Effects): binds all the variables in Effects according to the bindings in Preconds.  If the fact is negative, it is not necessary to instantiate the vars as, unless they are instantiated elsewhere, these should remain free.

instantiateVars([],_StateChanges,_Effects).

instantiateVars([not(_Fact)|Rest],StateChanges,Effects) :-
	instantiateVars(Rest,StateChanges,Effects).

instantiateVars([class(Object,Class)|Rest],StateChanges,Effects) :-
	checkClass(class(Object,Class)),
	instantiateVars(Rest,StateChanges,Effects).

instantiateVars([calculation(Sum)|Rest],StateChanges,Effects) :-
	Sum,
	instantiateVars(Rest,StateChanges,Effects).

instantiateVars([Fact|Rest],StateChanges,Effects) :-
	(   member(Fact,StateChanges)
	;
	    fact(Fact),
	    \+ member(not(Fact),StateChanges)
	),
	instantiateVars(Rest,StateChanges,Effects).


%processEffects(+StateChanges,+Effects,-NewStateChanges): updates the state appropriately.

processEffects(StateChanges,[],StateChanges).

processEffects(StateChanges,[class(Object,Class)|RestEffects],NewStateChanges) :-
	checkClass(class(Object,Class)),
	processEffects(StateChanges,RestEffects,NewStateChanges).

processEffects(StateChanges,[FirstEffect|RestEffects],NewStateChanges) :-
	member(FirstEffect,StateChanges),
	processEffects(StateChanges,RestEffects,NewStateChanges).

processEffects(StateChanges,[not(FirstEffect)|RestEffects],NewStateChanges) :-
	member(FirstEffect,StateChanges),
	removeEl(FirstEffect,StateChanges,CurrentStateChanges),    
	processEffects(CurrentStateChanges,RestEffects,NewStateChanges).

processEffects(StateChanges,[not(FirstEffect)|RestEffects],NewStateChanges) :-
	\+ member(not(FirstEffect),StateChanges),
	processEffects([not(FirstEffect)|StateChanges],RestEffects,NewStateChanges).

processEffects(StateChanges,[calculation(Sum)|RestEffects],NewStateChanges) :-
	Sum,
	processEffects(StateChanges,RestEffects,NewStateChanges).

processEffects(StateChanges,[inform(FirstEffect)|RestEffects],NewStateChanges) :-
	\+ member(FirstEffect,StateChanges),
	processEffects([FirstEffect|StateChanges],RestEffects,NewStateChanges).

processEffects(StateChanges,[FirstEffect|RestEffects],NewStateChanges) :-
	\+ FirstEffect = calculation(_Sum),
	\+ FirstEffect = class(_Object,_Class),
	\+ FirstEffect = inform(_Fact),
	\+ member(FirstEffect,StateChanges),
	\+ fact(FirstEffect),
	processEffects([FirstEffect|StateChanges],RestEffects,NewStateChanges).
	
	
	

%conc(?List1,?List2,?List3) concatenates List1 and List2 into List3.

conc([],L,L).

conc([X|L1],L2,[X|L3]) :-
	conc(L1,L2,L3).


%checkClass(class(+Object,+Class)): succeeds if Object is a direct or indirect subclass of Class.

checkClass(class(Object,Class)) :-
	class(Object,Class).

checkClass(class(Object,Class)) :-
	subclass(SubClass,Class),
	checkClass(class(Object,SubClass)).


%performCalculcation(calculation(+Sum)): evaluates Sum.

performCalculation(calculation(Sum)) :-
	Sum.


%removeEl(+Element,+OldList,-NewList): NewList is OldList with the first occurrence of Element removed.

removeEl(Element,OldList,NewList) :-
	removeEl(Element,OldList,[],NewList).
	
removeEl(Element,[Element|EndList],ListSoFar,NewList) :-
	append(ListSoFar,EndList,NewList).

removeEl(Element,[FirstEl|EndList],ListSoFar,NewList) :-
	append(ListSoFar,[FirstEl],NextList),
	removeEl(Element,EndList,NextList,NewList).