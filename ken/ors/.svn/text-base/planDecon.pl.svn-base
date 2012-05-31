:- use_module(library(lists)),use_module(library(charsio)). 



% top level function to deconstruct a plan.

% Initially, deconstruct is given a plan and an uninstantiated variable to build up as the answer.  This is turned into a 5-arity predicate, where the initial Ans has started to be built up - here, it consists of a statelist (initial values stated in specific file), and the initial situation (start):
% deconstruct(Plan,InitialJust,InitialState,InitialSit,Ans).

% state is of the form [State1,...,StateN], where StateX is of the form [at(X,sit),[haslist],sit].

deconstruct(Plan,Ans) :- 
	setUp(initial(InitialTrue,InitialSit)),
	deconstruct(Plan,[[InitialTrue,InitialSit]],[start],[[InitialTrue,[InitialSit]]],Ans).



% Once the plan list is empty, instantiate Ans to the justification
%that has been built up and print out in pretty format.

deconstruct([],Just,_,_,Just).
%	makePretty(Just). 




% This is where the justificaiton is actually built up.  The plan is
%divided into the first step and the rest of the plan, with the rest of
%the plan only being used to pass on to the next recursion.  First the
%correct rule is found, and the list of preconds and postconds from
%this is labelled FirstJust.



deconstruct([FirstStep|Rest],Just,Sit,StateList,Ans) :-

	checkRule(FirstStep,FirstJust,StateList,Sit,NewState,NewSit),
	append([FirstStep|FirstJust],[NewState],PlusStateList),
	append(PlusStateList,[NewSit],NewJust),
        deconstruct(Rest,[NewJust|Just],NewSit,NewState,Ans).

  

setUp(initial(InitialTrue,start)) :-
	setof(Facts,fact(Facts),InitialTrue).


% CheckRule will make sure that each of the rules is executable in the
%situation in which it is to be executed and updates the situation and
%the state according to the action and its postconditions.

%checkRule(ActionRule,Just,State,Sit,NewState,NewSit) :-

%	rule(ActionRule,[RuleName,Preconds,Postconds]),
%	Just = [RuleName,Preconds,Postconds],
%	checkPreconds([Preconds],State),
%	append([ActionRule],Sit,NewSit),
%	updateState(State,[Postconds],NewSit,NewState).

checkRule(ActionRule,Just,State,Sit,NewState,NewSit) :-

	rule(ActionRule,[RuleName,Preconds,Postconds]),
	Just = [RuleName,Preconds,Postconds],
	checkPreconds([Preconds],State),
	append([ActionRule],Sit,NewSit),
	updateState(State,[Postconds],NewSit,NewState).



% CheckPreconds ensures that each of the preconditions of a rule holds
%in the current situation.  A precondition can be justified in one of
%three ways: first, it is a fact in the database - this is checked by
%creating a list of all the facts and then checking that this
%particular instantion is valid; second, it is a calculation - here it
%is checked to see that the calculation is performable; third, it is a
%fluent - here the State is checked to see that the precondition
%complies with its most recent mention in the state.


checkPreconds([[]],_).


checkPreconds([[class(Thing,Class)|Rest]],State) :-
	 % if the precond is a class statement then we can check the class hierarchy, or else we mustsee if this class statement has been made true by a previous action
	checkClass(Thing,Class),
	checkPreconds([Rest],State).

checkPreconds([FirstPrecond|Rest],State) :-
	checkState([FirstPrecond],State),
	checkPreconds([Rest],State).

checkPreconds([[FirstPrecond|Rest]],State) :-
	nonFacts(PredicateList),
	member(FirstPrecond,PredicateList),
	FirstPrecond,
	checkPreconds([Rest],State).

checkPreconds([[calculation(Sum)|Rest]],State) :-
	Sum,
	checkPreconds([Rest],State).

checkPreconds([[agentNeeded(_,_)|Rest]],State) :-
	checkPreconds([Rest],State).

checkPreconds([[not(FirstPrecond)|Rest]],State) :-
	\+ checkState([FirstPrecond],State),
	checkPreconds([Rest],State).

checkPreconds([[FirstPrecond|Rest]],State) :-
	checkState([FirstPrecond],State),
	checkPreconds([Rest],State).




%	    ( member(FirstPrecond,PredicateList) ->
%		FirstPrecond
%	    ;	
%		( FirstPrecond = calculation(Sum) ->
%		    Sum
%		;   
%		    ( FirstPrecond = agentNeeded(_,_) ->
%			FirstPrecond
%		    ;	
%			checkState([FirstPrecond],State)
%		    )
%		)
%	    )
%	), 
%	checkPreconds([Rest],State).
	     

% If a precondition or a postcondition is a calculation, it must
%checked that this calculation is performable.  Since all the values
%will be instantiated, no situation or state variables are necessary.



% this is to check whether a certain fluent is true in a given situation.


checkState([],_).

checkState([FirstPrecond|Rest],State) :-

%	removeSit(FirstPrecond,PrecondNoSit),
	checkTrue(FirstPrecond,State),!,
	checkState(Rest,State).
 


checkTrue(Precond,[[CurrentState|_]|OtherStates]) :-

	member(Precond,CurrentState);
	member(not(Precond),CurrentState),
	!,fail;
	checkTrue(Precond,OtherStates).
 



removeSit(Precond,PrecondNoSit) :-

	Precond =.. PrecondList,
	lastEl(PrecondList,Last),
	deleteEl(PrecondList,Last,NewPrecondList),
	PrecondNoSit =.. NewPrecondList.
	  


% so it's readable:

makePretty([Start|[]]) :-
 
	write('Initial: '), nl,
        makePrettyList(Start),
        nl, nl. 


makePretty([FirstUgly|RestUgly]) :-
 
	write('Action:'), nl, 
        makePrettyList(FirstUgly), 
        nl, nl, nl, 
        makePretty(RestUgly). 


makePrettyList([]). 


makePrettyList([FirstItem|RestItems]) :-
 
        nl, write(FirstItem), nl, 
        makePrettyList(RestItems). 



% to update the state with all the information that has been made true
%or false by a rule - a list of all the postconditions, together with
%a description of the situation, is added to the statelist.  If the
%postcondition is a calcluation, this calculation must be performed
%(but not added to the state list).

updateState(State,Postconds,Sit,NewState) :-

	updateState(State,Postconds,Sit,NewState,[]).



updateState(CurrentState,[[]],Sit,NewState,Changes) :-

%	removeSits(Changes,ChangesNoSit), 
	append([Changes],[Sit],NewStuff),
	append([NewStuff],CurrentState,NewState). 



updateState(CurrentState,[[FirstPostcond|Rest]],Sit,NewState,List) :-

	(FirstPostcond = calculation(Sum) ->
	    Sum,
	    updateState(CurrentState,[Rest],Sit,NewState,[FirstPostcond|List])
	;
	    ( FirstPostcond = inform(Fact) ->
		% ** see below for details of what is going on here.
		( checkTrue(Fact,CurrentState) ->
		    updateState(CurrentState,[Rest],Sit,NewState,[Fact|List])
		;
		    updateState(CurrentState,[Rest],Sit,NewState,[Fact|List])
		)
	    ;
		updateState(CurrentState,[Rest],Sit,NewState,[FirstPostcond|List])
	    )
	).


% sometimes these predicates about which other agents inform us contain information about things that we must have assumptions about to form plans - e.g. money.  sometimes they just have, e.g. reg nos, which don't affect plan formation.  So we must check whether we have assumptions about this, and if so, we must use these assumptions, because to use the predicate uninstantiated would lead to error (i.e. how can we perform calculations to work out if we can afford something if the price is uninstantiated - we must use an assumption about the price).  if we have no assumptions then we just assume the fact uninstantiated.


% Remove the situation from a fluent so that it can be compared to or
%added to the statelist.

removeSits(List,Ans) :-

	removeSits(List,[],Ans). 



removeSits([],Ans,[Ans]). 
 


removeSits([FirstChange|Rest],SoFar,Ans) :-

	removeSit(FirstChange,FirstChangeNoSit), 
	removeSits(Rest,[FirstChangeNoSit|SoFar],Ans). 


%checkClass(metaVar,_).

checkClass(Thing,Class) :-
        class(Thing,Class)
        ;
	setof(SubClass,subclass(SubClass,Class),SubClassList),
	checkSubClasses(Thing,SubClassList).

checkSubClasses(_,[]) :-
	fail.
checkSubClasses(Thing,[FirstSubClass|RestSubClasses]) :-
	(   class(Thing,FirstSubClass)
	;   
	    checkClass(Thing,FirstSubClass)
	)
	;
	checkSubClasses(Thing,RestSubClasses).




% Manipulations of lists

lastEl([H],H). 

lastEl([_|T],Last) :- 

	lastEl(T,Last). 



deleteEl(List,El,Ans) :- 

	deleteEl(List,El,[],Ans). 



deleteEl([H|T],El,SoFar,Ans) :-

	H == El,
	append(SoFar,T,Ans).



deleteEl([H|T],El,Ans,X) :- 

	append(Ans,[H],SoFar), 
	deleteEl(T,El,SoFar,X). 




firstEl([H|_],H). 



%findRule(ActionAtomName,[RuleName,Preconds,Postconds]) :-
%	findInfo(ActionAtomName,Name,Arity),
%	rule(RealActionName,[RuleName,Preconds,Postconds]),
%	RealActionName =.. [RealName|RealArgs],
%	length(RealArgs,Arity),
%	write_to_chars(RealName,Name).


%findInfo(Action,Name,Arity) :-
%	name(Action,ActionChars),
%	name('(',RightBracket),
%	matchExpression(Name,RightBracket,Args,ActionChars),
%	getArity(Args,Arity,1).

%getArity(Args,Arity,AritySoFar) :-
%	name(',',Comma),
%	matchExpression(_,Comma,RestArgs,Args),
%	NewArity is AritySoFar + 1,
%	getArity(RestArgs,Arity,NewArity).

%getArity(_,Arity,Arity).