% This file contains the code that performs a diagnosis of the problem
% after plan execution failure occurs.  The top level function,
% diagnoseFailure/8 is called by the central agent when necessary.


:- dynamic planSoFar/1.

% diagnoseFailure(+PlanningAgent,+FailedAction,+RelevantServiceProvidingAgent,+ListOfQueries,+Justification,+ListOfSurprisingQuestions,-Outcome,-RefinementPerfomed)
% This predicate is called by the planning agent when plan execution occurs, and passes all the relevant information to the diagnostic system.  The appropriate refinement is automatially performed within the diagnostic system; the Refinement variable is merely to report this to the planning agent, which is compiling a list of refinements performed.  The Outcome variable indicates whether refinement has been successful or not.

diagnoseFailure(Me,Action,Agent,[request],Just,_,Outcome,Refinement) :-
	write('this plan failed immediately after a request was made to perform '),write(Action),nl,
	out(query(Me,Agent,question,performTask(Action,Agent))),
	sleep(1),
	in_noblock(reply(Agent,Me,Reply)),
	interpretReply_noQuestions(Me,Action,Agent,Reply,Just,Outcome,Refinement).


diagnoseFailure(_,Action,Agent,[request],_,_,_,_) :-
	write('this plan failed immediately after a request was made to perform '),write(Action),nl,
	write(Agent),write(' wont bloody talk to me now. '),nl.


diagnoseFailure(_Me,Action,_Agent,[MostRecentQuery|_],_Just,[],_Outcome,negated_precondition) :-
	write('There seems to be a problem with '),write(MostRecentQuery),write('although I knew that I would be asked about it.'),nl,nl,
	checkFullyInstantiated(MostRecentQuery),
	write('this is fully instantiated: therefore, I must negate my expectations about '),write(MostRecentQuery),nl,
	rule(Action,[_,PrecondList,_]),
	Action =.. [ActionName|_ActionArgs],
	MostRecentQuery =.. [QueryName|_QueryArgs],
	findRightNegation(MostRecentQuery,PrecondList,NegationOutcome),
	translatePred(ActionName,TransAction),
	translatePred(QueryName,TransQuery),
	attemptRefine(negatePrecond,[TransAction,TransQuery,NegationOutcome]).


diagnoseFailure(_Me,_Action,_Agent,[MostRecentQuery|_],_Just,[],_Outcome,incorrect_instantiation) :-
	write('There seems to be a problem with '),write(MostRecentQuery),write('although I knew that I would be asked about it.'),nl,nl,
	write('I must have instantiated it incorrectly.  Im not sure what to do about this!'),nl.


diagnoseFailure(Me,Action,Agent,[_|_],Just,[RelevantSurprisingQuestion|_],Outcome,Refinement) :-
	write('I received a query about '),write(RelevantSurprisingQuestion),write(', which I was not expecting to be asked about.'),nl,
	matchesPrecond(RelevantSurprisingQuestion,NameRSQ,ArityRSQ,Action,Precond,ArityPrecond),
	instantiatePreconds([Precond]),!,
	write(RelevantSurprisingQuestion),write(' has the same name as the precondition '),write(Precond),nl,
	compareArity(Me,Agent,Just,RelevantSurprisingQuestion,NameRSQ,ArityRSQ,Action,Precond,ArityPrecond,Outcome,Refinement).


diagnoseFailure(Me,Action,Agent,[_|_],Just,[RelevantSurprisingQuestion|_],Outcome,Refinement) :-
	predicateRefinement(Me,Agent,Action,Just,RelevantSurprisingQuestion,Outcome,Refinement).


diagnoseFailure(Me,Action,Agent,[_|_],_,[RelevantSurprisingQuestion|_],Outcome,Refinement) :-
	nl,write('diagnosis: precondition anti-abstraction '),nl,
	write(Action),write(' requires an extra precondition: '),write(RelevantSurprisingQuestion),nl,
	write('WARNING: this is a guess'),nl,nl,
	% ** add in link to new refinement here (morike's).
	precondTranslate(Me,Agent,Action,RelevantSurprisingQuestion,TransInfo),
	attemptRefine(precondAA,TransInfo,Outcome),
	Refinement = [none,RelevantSurprisingQuestion,[precondAA,_]].





% interpretReply_noQuestions(+PlanningAgent,+FailedAction,+ServiceProvdingAgent,-Response,+Justification,-Outcome,-Refinement)
% This predicates is called in the situation where no questions have been asked prior to failure, and it interprets the service providing agent's response as to whether it can perform the action or not.  If it should be able to, the preconditions are examined.

interpretReply_noQuestions(Me,Action,Agent,yes,Just,Outcome,Refinement) :-
	write(Agent),write(' says that he can perform this task'),nl,
	problemPreconds(Me,Agent,Just,Action,ProblemPrecond),
	interpretProblemPreconds(Me,Agent,Just,Action,ProblemPrecond,Outcome,Refinement).


interpretReply_noQuestions(_,Action,Agent,no,_,Outcome,Refinement) :-
	write('diagnosis: '),write(Agent),write(' cant perform '),write(Action),nl,
	translatePred(Agent,TransAgent),
	attemptRefineMeta(agent,TransAgent,Outcome),
	Refinement = [none,meta,[incorrectAgent,_]].


interpretReply_noQuestions(_,_,_,Reply,_,failure,[]) :-
	write('reply is '),write(Reply),nl.




% interpretProblemPreconds(+PlanningAgent,+ServiceProvidingAgent,+Justification,+FailedAction,+ProblemPrecond,-Outcome,-Refinement)
% Diagnoses problem based on whether there is a problem precondition or not.

interpretProblemPreconds(Me,Agent,_,_,class(Item,_WrongClass),Outcome,change_instance_definition) :-
	write('diagnosis: change instance definition: '),
	class(Item,WrongClass),
	out(query(Me,Agent,question,class(Item,Class))),
	in_noblock(reply(Agent,Me,class(Item,Class))),
	WrongClass \= Class,
	write('According to '),write(Agent),
	write(' the class of '),write(Item),
	write(' should be '), write(Class), nl,
	write('I thought that '),write(Item),
	write(' was of class '),write(WrongClass),
	write('I will update my ontology to align with the '),
	write(Agent),write(' ontology '), nl,
	translatePred(Item,TransItem),
	translatePred(WrongClass,TransWrongClass),
	translatePred(Class,TransClass),
	attemptRefine(changeInstDef,[TransItem,TransWrongClass,TransClass],Outcome).

interpretProblemPreconds(Me,Agent,_,_,subclass(Class,_WrongSuperClass),Outcome,change_class_definition) :-
	write('diagnosis: change class definition: '),
	class(Class,WrongSuperClass),
	out(query(Me,Agent,question,subclass(Class,SuperClass))),
	in_noblock(reply(Agent,Me,subclass(Class,SuperClass))),
	WrongSuperClass \= SuperClass,
	write('According to '),write(Agent),
	write(' the superclass of '),write(Class),
	write(' should be '),write(SuperClass), nl,
	write('I thought that '),write(Class),
	write(' was of superclass '),write(SuperClass),
	write(' I will update my ontology to align with the '), write(Agent),write(' ontology '), nl.

interpretProblemPreconds(Me,Agent,Just,_,ProblemPrecond,Outcome,Refinement) :-
	write('diagnosis: problem precond: '),
	write(ProblemPrecond),nl,
	shapiroCheck(Me,Agent,ProblemPrecond,Just,Outcome),
	Refinement = [ProblemPrecond,shapiro,[problemPrecondition,Just]].


% this is the pre-Morike stuff:

% interpretProblemPreconds(_,Agent,_,_,none,failure,missing_precondition) :-
% 	write(Agent),write(' agrees that all my preconditions are correct '),nl,
% 	write('diagnosis: missing precond'),nl,
% 	write('FAILURE - no refinement is possible'),nl.


% %interpretProblemPreconds(Me,Agent,_,_,ProblemPrecond,_,problem_precondition) :-
% %	write('diagnosis: problem precond: '),write(ProblemPrecond),nl,
% %	checkTransitive_noQueries(Me,Agent,ProblemPrecond,TransClosure),
% %	write('transclosure is possible.  transclosure fact is '),write(TransClosure),nl.


% interpretProblemPreconds(Me,Agent,Just,_,ProblemPrecond,Outcome,problem_precondition) :-
% 	write('diagnosis: problem precond: '),write(ProblemPrecond),nl,
% 	shapiroCheck(Me,Agent,ProblemPrecond,Just,Outcome).




% compareArity(+PlanningAgent,+ServiceProvidingAgent,+RelevantSurprisingQuestion,+NameRelevantSurprisingQuestion,+ArityRSQ,+FailedAction,+Precondition,+ArityPrecond,-Outcome,-Refinement)
% calls either domainRefinement or propositionalRefinement, depending on whether the received question and the expected question have the same arity or not.

compareArity(Me,Agent,Just,RelevantSurprisingQuestion,_,ArityRSQ,Action,Precond,ArityRSQ,Outcome,Refinement) :-
	write('They also have the same arity ('),write(ArityRSQ),write(')'),nl,
	domainRefinement(Me,Action,Agent,Just,RelevantSurprisingQuestion,Precond,Outcome,Refinement).


compareArity(Me,Agent,_,RelevantSurprisingQuestion,NameRSQ,ArityRSQ,_,Precond,ArityPrecond,Outcome,Refinement) :-
	write('They have different arity ('),write(ArityRSQ),write(' and '),write(ArityPrecond),write(')'),nl,
	propositionalRefinement(Me,Agent,RelevantSurprisingQuestion,Precond,ArityRSQ,ArityPrecond,NameRSQ,Outcome,Refinement).



% attemptRefine(+RefinementType,+RefinementInformation,-Outcome)
% once a diagnosis has been made, the diagnostic algorithm calls the refinement system to implement it.  AttemptRefine monitors whether this has been successful, and returns the outcome.

attemptRefine(RefineType,RefineInfo,success) :-
	refine(RefineType,RefineInfo),
	nl,write('the appropriate refinement has been performed'),nl.

attemptRefine(_,_,failure) :-
	write('WARNING: refinement has failed'),nl.



% attemptRefineMeta(+RefinementType,+RefinementInformation,-Outcome)
% as for attemptRefine, only refines the meta ontology.

attemptRefineMeta(agent,TransAgent,success) :-
	refineMeta(agent,TransAgent),
	nl,write('the appropriate refinement has been performed'),nl.

attemptRefineMeta(agent,_,failure) :-
	write('WARNING: refinement has failed'),nl.
	
		    




% propositionalRefinement(+PlanningAgent,+ServiceProvidingAgent,+RelevantSurprisingQuestion,+ExpectedPrecondition,+ArityRSQ,+ArityExP,+NameRSQ,-Outcome)
% diagnoses exactly how propositional refinement should be performed, finds the necessary information for refining, and attemps to perform it.

propositionalRefinement(Me,Agent,RSQ,ExPrecond,ArityRSQ,ArityExP,NameRSQ,Outcome,propositional_anti_abstraction) :-
	1 is ArityRSQ - ArityExP,
	nl,write('diagnosis: propositional anti-abstraction'),nl,
	findArgsClassMine(ExPrecond,ExPArgsClassList),
	write('my arguments are of these types : '),write(ExPArgsClassList),nl,
	findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
	compareClass(ExPArgsClassList,RSQArgsClassList,[],HisUnmatchedClasses,MyUnmatchedClasses),
	findHisUnmatchedClass(MyUnmatchedClasses,HisUnmatchedClasses,[UnmatchedClass|[Position]]),
	checkUnmatchedClass(Me,Agent,NameRSQ,UnmatchedClass,Position,ArityExP,RSQ,Outcome).
	

propositionalRefinement(Me,Agent,RSQ,ExPrecond,ArityRSQ,ArityExP,NameRSQ,Outcome,Refinement) :-
	-1 is ArityRSQ - ArityExP,
	nl,write('diagnosis: propositional abstraction'),nl,
	findArgsClassMine(ExPrecond,ExPArgsClassList),
	write('my arguments are of these types : '),write(ExPArgsClassList),nl,
	findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
	findClassAndPosition(ExPArgsClassList,RSQArgsClassList,_UnmatchedClass,Position),
	translatePred(NameRSQ,TransName),
	attemptRefine(propositionalA,[TransName,Position],Outcome),
	Refinement = [ExPrecond,RSQ,[propositionalA,[NameRSQ,Position]]].


propositionalRefinement(_,_,_,_,_,_,_,failure,[]) :-
	write('The difference in arity is greater than 1.  The system cannot currently deal with this.'),nl.



% checkUnmatchedClass(+PlanningAgent,+ServiceProvidingAgent,+NameRSQ,+UnmatchedClass,+Position,+ArityExP,+RSQ,-Outcome)
% in proprositional anti-abstraction, there is always an unmatched class.  checkUnmatchedClasses performs propositional anti-abstraction on the basis of what this unmatched class is and whether it is already known.  If it is not, it must be added to the class hierarchy.

checkUnmatchedClass(_,_,NameRSQ,uninstantiated,_,_,_,failure) :-
	write(NameRSQ),write(' requires an extra argument of undetermined type'),nl,
	write('Refinement impossible: we do not know the type of the argument to add').

checkUnmatchedClass(_,_,NameRSQ,thing,Position,ArityExP,_,Outcome) :-
	write(NameRSQ),write(' requires an extra argument of type '),write(thing),nl,nl,
	write('I already know that '),write(thing),write(' is the highest superclass '),
	write('position is '),write(Position),nl,
	propAATranslate(NameRSQ,thing,Position,ArityExP,TransInfo),
	attemptRefine(propositionalAA,TransInfo,Outcome).
	
checkUnmatchedClass(_,_,NameRSQ,UnmatchedClass,Position,ArityExP,_,Outcome) :-
	subclass(UnmatchedClass,Class),
	write(NameRSQ),write(' requires an extra argument of type '),write(UnmatchedClass),nl,nl,
	write('I already know that '),write(UnmatchedClass),write(' is a subclass of '),write(Class),
	propAATranslate(NameRSQ,UnmatchedClass,Position,ArityExP,TransInfo),
	attemptRefine(propositionalAA,TransInfo,Outcome).
    
checkUnmatchedClass(Me,Agent,NameRSQ,UnmatchedClass,Position,ArityExP,RSQ,Outcome) :-
	out(query(Me,Agent,question,subclass(UnmatchedClass,Class))),
	in_noblock(reply(Agent,Me,subclass(UnmatchedClass,Class))),
	write(Agent),write(' tells me that '),write(UnmatchedClass),write(' is of class '),write(Class),nl,
	classTranslate(UnmatchedClass,Class,TransClass,TransSuper),
	addClass(TransClass,TransSuper),
	propAATranslate(NameRSQ,UnmatchedClass,Position,ArityExP,TransInfo),
	findIndividual(Position,RSQ,InstArg),
	translatePred(InstArg,TransInst),
	addIndividual(TransInst,TransClass),
	attemptRefine(propositionalAA,TransInfo,Outcome).



% domainRefinement(+PlanningAgent,+FailedAction,+ServiceProvidingAgent,+Justification,+RSQ,+ExpectedPrecond,-Outcome,-Refinement)
% if the expected preconditions have the same name and the same arity, domain refinement determines how they are mismatched by looking at the classes of the arguments.

domainRefinement(Me,Action,Agent,_,class(Object,RightClass),class(Object,WrongClass),Outcome,Refinement) :-
	write('my object is of the wrong class'),nl,
	checkSubClass(Me,Agent,RightClass,WrongClass),
	write(Agent),write(' expected an object of class '),write(RightClass),write(' whereas I thought that an object of class '),write(WrongClass),write(' would be acceptable.  '),nl,
	write(WrongClass),write(' is a subclass of '),write(RightClass),write(' so I need to be more specific about this object'),
	precondDomTranslate(Me,Agent,Action,class,RightClass,WrongClass,TransInfo),
	attemptRefine(precondAA,TransInfo,Outcome),
	Refinement = [WrongClass,RightClass,[precondAA,class]].


domainRefinement(Me,Action,Agent,Just,RSQ,ExPrecond,Outcome,Refinement) :-
	findArgsClassMine(ExPrecond,ExPArgsClassList),
	write('args class list is '),write(ExPArgsClassList),nl,nl,
	findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
	compareClass(RSQArgsClassList,ExPArgsClassList,[],_,MyDiffList),
	compareClass(ExPArgsClassList,RSQArgsClassList,[],_,HisDiffList),
	write('my unmatched classes: '),write(MyDiffList),nl,
	write('his unmatched classes: '),write(HisDiffList),nl,
	examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,RSQArgsClassList,Outcome,Refinement).


domainRefinement(_,_,_,_,_,_,failure,[]) :-
	write('this is more complex: need to sort this out later'),nl.




% examineDiffs(+PlanningAgent,+ServiceProvidingAgent,+FailedActoin,+RSQ,+ExpectedPrecondition,+Justification,+MyUnmatchedClassList,+HisUnmatchedClassList,+ExpectedPrecondArgsClassList,+RSQArgsClassList,-Outcome,-Refinement)
% examines how the classes differ between the class list of the expected precondition and the class list of the actual precondition.

examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,_,Outcome,Refinement) :-
	length(MyDiffList,1),
	length(HisDiffList,1),
	firstEl(MyDiffList,MyDiff),
	firstEl(HisDiffList,HisDiff),
	compareDiffs(Me,Agent,Action,RSQ,ExPrecond,ExPArgsClassList,Just,MyDiff,HisDiff,Outcome,Refinement).

examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,RSQArgsClassList,Outcome,Refinement) :-
	MyDiffList = [],
	HisDiffList = [],
	compareClassOrder(RSQArgsClassList,ExPArgsClassList,[],SwitchList),
	checkSwitchList(Me,Agent,Action,RSQ,ExPrecond,Just,SwitchList,Outcome,Refinement).



% checkSwitchList(+PlanningAgent,+ServiceProvdingAgent,+RSQ,+ExpectedPrecondition,+Justification,+SwitchList,-Outcome,-Refinement)
% if the classes of the arguments of the two predicates are the same, checkSwitchList examines whether the order is the same and attempts to refine accordingly.

checkSwitchList(Me,Agent,_,RSQ,ExPrecond,_,[],success,[]) :-
	checkTransitive(Me,Agent,RSQ,ExPrecond),
	write('diagnosis: transitive closure.  New Pred is '),write(RSQ),nl.


checkSwitchList(Me,Agent,_,_,ExPrecond,Just,[],Outcome,Refinement) :-
	write('class argreement between the args.  incorrect instantiation'),nl,
	shapiroCheck(Me,Agent,ExPrecond,Just,Outcome),
	Refinement = [ExPrecond,shapiro,[incorrectInstantiation,Just]].
	

checkSwitchList(_,_,_,RSQ,ExPrecond,_,SwitchList,Outcome,Refinement) :-
	write('right classes for args of this pred but in the wrong order'),nl,
	write('switch list is '),write(SwitchList),nl,
	ExPrecond =.. [PredName|_PredArgs],
	translatePred(PredName,TransPred),
	attemptRefine(switchArgs,TransPred,Outcome),
	Refinement = [ExPrecond,RSQ,[switchArgs,PredName]].

	

% compareDiffs(+PlanningAgent,+ServiceProvidingAgent,+FailedAction,+RSQ,+ExpectedPrecondition,+ExectedPreconditionArgsClassList,+Justification,+MyDifference,+HisDifference,-Outcome,-Refinement)
% If there are arguments that are mismatched, compareDiffs explores how they are mismatched and attempts refinement on that basis.

compareDiffs(Me,Agent,_,RSQ,ExPrecond,ExPArgsClassList,_,MyDiff,HisDiff,Outcome,Refinement) :-
	checkSubClass(Me,Agent,MyDiff,HisDiff) ->
	write('diagnosis: domain abstraction.  '),write(MyDiff),write(' is a subtype of '),write(HisDiff),nl,
	domTranslate(RSQ,HisDiff,MyDiff,ExPArgsClassList,TransInfo),
	attemptRefine(domainA,TransInfo,Outcome),
	Refinement = [ExPrecond,RSQ,[domainA,[ExPArgsClassList,MyDiff,HisDiff]]].

compareDiffs(Me,Agent,_,RSQ,ExPrecond,ExPArgsClassList,_,MyDiff,HisDiff,Outcome,Refinement) :-
	checkSubClass(Me,Agent,HisDiff,MyDiff) ->
	write('diagnosis: domain anti-abstraction. '),write(HisDiff),write(' is a subtype of '),write(MyDiff),nl,
	domTranslate(RSQ,MyDiff,HisDiff,ExPArgsClassList,TransInfo),
	attemptRefine(domainAA,TransInfo,Outcome),
	Refinement = [ExPrecond,RSQ,[domainAA,[ExPArgsClassList,MyDiff,HisDiff]]].

compareDiffs(Me,Agent,_,_,ExPrecond,_,Just,MyDiff,HisDiff,Outcome,Refinement) :-
	write('diagnosis: arguments of different types and are not subtypes of each other. '),nl,
	write('my argument is of type '),write(MyDiff),write(', his argument is of type '),write(HisDiff),nl,
	shapiroCheck(Me,Agent,ExPrecond,Just,Outcome),
	Refinement = [ExPrecond,shapiro,[changeOfType,Just]].

compareDiffs(_,_,_,_,_,_,_,_,_,failure,[]) :-
	write('the system cannot diagnose the appropriate refinement.'),nl.
	




% predicateRefinement(+PlanningAgent,+ServiceProvidingAgent,+FailedAction,+Justification,+RelevantSurprisingQuestion,-Outcome,-Refinement)
% finds the relevant preconditions and determines how the predicates are related.

predicateRefinement(Me,Agent,Action,Just,RelevantSurprisingQuestion,Outcome,Refinement) :-
	locateRule(Action,Just,Preconds,_),
	checkPredicateSubTypes(Me,Agent,Action,RelevantSurprisingQuestion,Preconds,Outcome,Refinement).



% problemPreconds(+PlanningAgent,+ServiceProvidingAgent,+Justification,+FailedAction,-Problem)
% checks with the other agent to find a problem precond if one exists, returning [none] if none can be found.

problemPreconds(Me,Agent,Just,Action,Problem) :-
	locateRule(Action,Just,Preconds,_),
	instantiatePreconds(Preconds),!,
	problemPreconds(Me,Agent,Preconds,Problem).

problemPreconds(_,_,[],none).

problemPreconds(Me,Agent,[FirstPrecond|RestPreconds],Problem) :-
	myFactsList(MyFacts),
	FirstPrecond =.. [FirstPrecondName|_],
	member(FirstPrecondName,MyFacts),
	problemPreconds(Me,Agent,RestPreconds,Problem).

problemPreconds(Me,Agent,[FirstPrecond|RestPreconds],Problem) :-
	out(query(Me,Agent,truth,FirstPrecond)),
	sleep(1),
	in_noblock(reply(Agent,Me,Reply)),
	problemPrecondReply(Me,Agent,FirstPrecond,RestPreconds,Reply,Problem).

% problemPrecondReply(+PlanningAgent+ServiceProvidingAgent,+FirstPrecond,+RestPreconds,+Agent'sReply,-Problem)
% interprets agent's reply to find the problem precond, recursing on the rest of the preconds if necessary.

problemPrecondReply(Me,Agent,class(Object,_),_,no,class(Object,NewClass)) :-
	out(query(Me,Agent,question,class(Object,NewClass))),
	in_noblock(reply(Agent,Me,class(Object,NewClass))).
	
problemPrecondReply(_,_,FirstPrecond,_,no,FirstPrecond).

problemPrecondReply(Me,Agent,_,RestPreconds,yes,Problem) :-
	problemPreconds(Me,Agent,RestPreconds,Problem).


% matchesPrecond(+SurprisingQuestion,-RSQName,-RSQArity,+FailedAction,-Precondition,-ArityPrecond)
% returns the first precondition from the precondition list who's name matches that of the surprising question.  fails if none match.  incidentally instantiates the name of the RSQ and its arity and the arity of the matching precondition, since these need to be calculated in this predicate and are useful later.

matchesPrecond(Pred,Name,ArityPred,Action,Precond,ArityPrecond) :-
	rule(Action,[_,PrecondList,_]),
	findPredInfo(Pred,Name,ArityPred),
	matchName(Name,PrecondList,Precond,ArityPrecond).

% matchName(+Name,+Preconditions,-Precond,-ArityPrecond)
% returns the name and arity of hte matching precondition; fails if none match.

matchName(Name,[],_,_) :-
	write('no preconds have the same name as '),write(Name),nl,
	fail.

matchName(Name,[FirstPrecond|_],FirstPrecond,ArityPrecond) :-
	findPredInfo(FirstPrecond,Name,ArityPrecond).

matchName(Name,[_|Rest],Precond,ArityPrecond) :-
	matchName(Name,Rest,Precond,ArityPrecond).


% findPredInfo(+Predicate,-Name,-Arity)
% returns the name and arity of a predicate.

findPredInfo(Predicate,Name,Arity) :-
	Predicate =.. [Name|ArgsList],
	length(ArgsList,Arity).


% findArgsClassMine(+ExpectedPrecondition,-ArgsClassList)
% returns the classes of all the arguments of expected precondition.

findArgsClassMine(ExPrecond,ArgsClassList) :-
	instantiatePreconds([ExPrecond]),!,
	ExPrecond =.. [_|ArgsList],
	findArgsClassMine(ArgsList,[],ArgsClassList).

findArgsClassMine([],ArgsClassList,ArgsClassList).

findArgsClassMine([H|T],ClassesSoFar,ArgsClassList) :-
	number(H),
	findArgsClassMine(T,[number|ClassesSoFar],ArgsClassList).

findArgsClassMine([H|T],ClassesSoFar,ArgsClassList) :-
	class(H,HClass),
	findArgsClassMine(T,[HClass|ClassesSoFar],ArgsClassList).


% findArgsClassHis(+PlanningAgent,+ServiceProvidingAgent,+ExectedPrecondition,-ArgsClassList)
% returns the classes of all the arguments of expected precondition.  this is very similar to findArgsClassMine, but the serviceproviding agent can be contacted for information if necessary.

findArgsClassHis(Me,Agent,ExPrecond,ArgsClassList) :-
	ExPrecond =.. [_|ArgsList],
	findArgsClassHis(Me,Agent,ArgsList,[],ArgsClassList).

findArgsClassHis(_,_,[],ArgsClassList,ArgsClassList).

findArgsClassHis(Me,Agent,[uninstantiated|T],ClassesSoFar,ArgsClassList) :-
	findArgsClassHis(Me,Agent,T,[uninstantiated|ClassesSoFar],ArgsClassList).
	
findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
	number(H),
	findArgsClassHis(Me,Agent,T,[number|ClassesSoFar],ArgsClassList).

findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
	class(H,HClass),
	findArgsClassHis(Me,Agent,T,[HClass|ClassesSoFar],ArgsClassList).

findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
	out(query(Me,Agent,question,class(H,HClass))),
	in_noblock(reply(Agent,Me,class(H,HClass))),
	write(Agent),write(' tells me that '),write(H),write(' is of class '),write(HClass),nl,
	findArgsClassHis(Me,Agent,T,[HClass|ClassesSoFar],ArgsClassList).


% compareClass(+ExpectedPrecondArgsClassList,+RSQArgsClassList,+HisUnmatchedClassesSoFar,-HisUnmatchedClass,-MyUnmatchedClass)
% returns two lists: one of all the classes of the arguments of the surprising question that are not matched by the classes of arguments of the expected precondition, and one vice versa.

compareClass(ExPArgsClassList,RSQArgsClassList,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass) :-
	reverse(ExPArgsClassList,RevExPArgsClassList),
	reverse(RSQArgsClassList,RevRSQArgsClassList),
	compareClass(RevExPArgsClassList,RevRSQArgsClassList,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,1).

compareClass(MyUnmatchedClass,[],UnmatchedClass,UnmatchedClass,MyUnmatchedClass,_).

compareClass(ExPArgsClassList,[FirstClass|OtherClass],HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,Counter) :-
	NewCounter is Counter + 1,
	member(FirstClass,ExPArgsClassList),
	deleteEl(ExPArgsClassList,FirstClass,NewExPArgsClassList),
	compareClass(NewExPArgsClassList,OtherClass,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,NewCounter).

compareClass(ExPArgsClassList,[FirstClass|OtherClass],HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,Counter) :-
	NewCounter is Counter + 1,
	compareClass(ExPArgsClassList,OtherClass,[[FirstClass,Counter]|HisUnmatchedClassSoFar],HisUnmatchedClass,MyUnmatchedClass,NewCounter).



% compareClassOrder(+RSQClassList,+ExpectedPreconditionClassList,+SwitchListSoFar,-SwitchList)
% if the classes all match but the order is not the same, this returns a list of the arguments that need to be switched.

compareClassOrder([],[],SwitchList,SwitchList).

compareClassOrder([FirstRSQClass|RestRSQ],[FirstRSQClass|RestExP],SwitchListSoFar,SwitchList) :-
	compareClassOrder(RestRSQ,RestExP,SwitchListSoFar,SwitchList).

compareClassOrder([FirstRSQClass|RestRSQ],[FirstExPClass|RestExP],SwitchListSoFar,SwitchList) :-
	compareClassOrder(RestRSQ,RestExP,[[FirstRSQClass,FirstExPClass]|SwitchListSoFar],SwitchList).


% checkSubClass(+PlanningAgent,+ServiceProvidingAgent,+Class1,+Class2)
% succeeds if Class1 is a subclass of Class2, fails otherwise.  this may require interaction with the serviceProvidingAgent if sufficient class information is not already available.

checkSubClass(_,_,Class1,Class2) :-
        subclass(Class1,Class2).

checkSubClass(Me,Agent,Class1,Class2) :-
	out(query(Me,Agent,truth,subclass(Class1,Class2))),
	in_noblock(reply(Agent,Me,yes)),!.

checkSubClass(Me,Agent,Class1,Class2) :-
	setof(SubClass,subclass(SubClass,Class2),SubClassList),
	checkSubClasses1(Me,Agent,Class1,SubClassList).


% checkSubClasses1(+PlanningAgent,+ServiceProvidingAgent,+Class,+SubclassList)
% if class1 is not an immediate subclass of class2, checks through all the subclasses of class2 to see if there is a more distant subclass relation.

checkSubClasses1(_,_,_,[]) :-
	fail.

checkSubClasses1(_,_,Class1,[FirstSubClass|_]) :-
	subclass(Class1,FirstSubClass).

checkSubClasses1(Me,Agent,Class1,[FirstSubClass|_]) :-
	checkSubClass(Me,Agent,Class1,FirstSubClass).

checkSubClasses1(Me,Agent,Class1,[_|RestSubClasses]) :-
	checkSubClasses1(Me,Agent,Class1,RestSubClasses).



% checkPredicateSubTypes(+PlanningAent,+ServiceProvidingAgent,+FailedAction,+Preconditions,-Outcome,-Refinement)
% determines whether predicate abstraction or anti-abstraction is required.

checkPredicateSubTypes(Me,Agent,Action,RSQ,[FirstPrecond|RestPreconds],Outcome,Refinement) :-
	findPredInfo(RSQ,NameRSQ,_),
	findPredInfo(FirstPrecond,NameFP,_),
	checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],NameRSQ,NameFP,Outcome,Refinement).

checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],_NameRSQ,NameFP,Outcome,Refinement) :-
	NameFP = class,!,
	checkPredicateSubTypes(Me,Agent,Action,RSQ,RestPreconds,Outcome,Refinement).

checkPredicateSubTypes(_Me,_Agent,Action,RSQ,[_|_],NameRSQ,NameFP,Outcome,Refinement) :-
	predSubclass(NameRSQ,NameFP),
	write('diagnosis: predicate anti-abstraction'),nl,
	write(NameRSQ),write(' is a subtype of '),write(NameFP),nl,
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	% is this a subtype we know ourselves?  or one we have to add?
	attemptRefine(predicateAA,TransInfo,Outcome),
	Refinement = [none,RSQ,[predicateAA,[NameRSQ,NameFP]]],
	write('i knew the type'),nl,
	write('the appropriate refinement has been performed'),nl.

checkPredicateSubTypes(Me,Agent,Action,_RSQ,[_|_],NameRSQ,NameFP,Outcome,[NameFP,NameRSQ,[predicateAA,TransInfo]]) :-
	checkPredSubClass(Me,Agent,NameRSQ,NameFP),!,
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	classTranslate(NameRSQ,NameFP,NewPredClass,NewPredSuperClass),
	addPredClass(NewPredClass,NewPredSuperClass),
	attemptRefine(predicateAA,TransInfo,Outcome).

checkPredicateSubTypes(_,_,Action,_RSQ,[_|_],NameRSQ,NameFP,Outcome,[NameFP,NameRSQ,[predicateAA,TransInfo]]) :-
	predSubclass(NameFP,NameRSQ),
	write('diagnosis: predicate abstraction'),nl,
	write(NameFP),write(' is a subtype of '),write(NameRSQ),nl,
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	attemptRefine(predicateAA,TransInfo,Outcome).

checkPredicateSubTypes(Me,Agent,Action,_RSQ,[_|_],NameRSQ,NameFP,Outcome,[NameFP,NameRSQ,[predicateAA,TransInfo]]) :-
	checkPredSubClass(Me,Agent,NameFP,NameRSQ),!,
	write('diagnosis: predicate abstraction'),nl,
	write(NameFP),write(' is a subtype of '),write(NameRSQ),nl,
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	classTranslate(NameRSQ,NameFP,NewPredClass,NewPredSuperClass),
	addPredClass(NewPredClass,NewPredSuperClass),
	attemptRefine(predicateAA,TransInfo,Outcome).

checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],_,_,Outcome) :-	
	checkPredicateSubTypes(Me,Agent,Action,RSQ,RestPreconds,Outcome).



% checkPredSubClass(+PlanningAgent,+ServiceProvidingAgent,+Class1,+Class2)
% succeeds if there is Class1 is a subclass of Class2, where class1 and class2 are predicates; fails otherwise.

checkPredSubClass(_,_,Class1,Class2) :-
        predSubclass(Class1,Class2).

checkPredSubClass(Me,Agent,Class1,Class2) :-
	out(query(Me,Agent,truth,subclass(Class1,Class2))),
	in_noblock(reply(Agent,Me,Answer)),!,
	Answer = yes.

checkPredSubClass(Me,Agent,Class1,Class2) :-	
	setof(PredSubClass,subclass(PredSubClass,Class2),PredSubClassList),
	checkPredSubClasses1(Me,Agent,Class1,PredSubClassList).


% checkPredSubClasses1(+PlanningAgent,+ServiceProvidingAgent,+Class,+SubClassList)
% succeeds if there is an indirect subclass relation between Class and any of the classes in the subclass list.

checkPredSubClasses1(_,_,_,[]) :-
	fail.
checkPredSubClasses1(_,_,Class1,[FirstPredSubClass|_]) :-
	predSubclass(Class1,FirstPredSubClass).

checkPredSubClasses1(Me,Agent,Class1,[FirstPredSubClass|_]) :-
	checkPredSubClass(Me,Agent,Class1,FirstPredSubClass).

checkPredSubClasses1(Me,Agent,Class1,[_|RestPredSubClasses]) :-	
	checkPredSubClasses1(Me,Agent,Class1,RestPredSubClasses).



% shapiroCheck(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,+Justification,-Outcome)
% this is used to determine how a problem precondition came to be believed.  it locates the rule which last changed the value of the precondition and then analyses why this happened

shapiroCheck(Me,Agent,ProblemPrecond,Just,Outcome) :-
	    locateProblemRule(ProblemPrecond,Just,ProblemRule,ProbPostconds,ProbAction),
	    analyseProblemRule(Me,Agent,ProblemPrecond,ProblemRule,ProbPostconds,ProbAction,Outcome).



% locateProblemRule(+ProblemPrecond,+Justification,-ProblemRule,-ProblemPostconds,-ProblemAction)
% locates the rule which last changed the value of the problem precondition and returns the instantiated features.

locateProblemRule(ProbPrecond,[[FirstFluents,start]],originalFact,none,none) :-
	member(ProbPrecond,FirstFluents).


locateProblemRule(ProbPrecond,[[Action,RuleNo,_,Postconds,[[LastFluents|_]|_],_]|_],RuleNo,Postconds,Action) :-
	member(ProbPrecond,LastFluents).

locateProblemRule(ProbPrecond,[[_,_,_,_,_,_]|RestJust],ProblemRule,ProbPostconds,ProbAction) :-
	locateProblemRule(ProbPrecond,RestJust,ProblemRule,ProbPostconds,ProbAction).



% analyseProblemRule(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,+ProblemRule,+ProblemPostconds,+ProbAction,-Outcome)
% determines whether the precondition had always been believed to be true (original fact) or whether it is a postcondition of a previous rule

analyseProblemRule(_,_,ProblemPrecond,originalFact,_,_,Outcome) :-
	write(ProblemPrecond),write(' is an original fact in my ontology '),nl,
	translatePrecond(ProblemPrecond,TransInfo),
	attemptRefine(problemPrecond,TransInfo,Outcome).

analyseProblemRule(Me,Agent,ProblemPrecond,_,ProbPostconds,ProbAction,Outcome) :-
	member(ProblemPrecond,ProbPostconds),
	checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,Outcome).

analyseProblemRule(Me,Agent,ProblemPrecond,_,ProbPostconds,ProbAction,Outcome) :-
	member(inform(ProblemPrecond),ProbPostconds),
	checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,Outcome).


% checkProblemPredicates(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecondition,+ProbAction,-Outcome)
% finds the agent that performed the old action and determines what it thinks the value of the precondition should be

checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,Outcome) :-
	queryProblemPrecond(Me,Agent,ProblemPrecond,InstPrecondNew),
	findPredInfo(ProbAction,Name,_),
	agentNeeded(OldAgent,Name),
	queryProblemPrecondOldAgent(Me,OldAgent,ProblemPrecond,InstPrecondOld),
	write('i believe that '),write(ProblemPrecond),write(' is the case because i think it is a postcond of '),write(ProbAction),write(' which was performed for me by '),write(OldAgent),write('.'),nl,nl,
	findAgreement(Agent,OldAgent,ProblemPrecond,InstPrecondNew,InstPrecondOld,ProbAction,Outcome).


% findAgreement(+PlanningAgent,+OldServiceProvidingAgent,+ProblemPrecond,+NewInstantiationofPrecond,+OldInstantiationofPrecond,+ProblemAction,-Outcome)
% refines according to what agreement there is between the old and new instantiations of the preconditions.

findAgreement(Agent,OldAgent,ProblemPrecond,InstPrecondNew,InstPrecondNew,ProbAction,Outcome) :-
	write(Agent),write(' and '),write(OldAgent),write(' agree with each other, so I must modify my ontology '),nl,
	refineRule(ProblemPrecond,ProbAction,Outcome).

findAgreement(Agent,OldAgent,ProbPrecond,_,ProbPrecond,_,failure) :-
	write(OldAgent),write(' agrees with me here.  there seems to be disagreement between '),write(Agent),write(' and '),write(OldAgent),write('.  this is a problem for me.'),nl.

findAgreement(_,_,_,_,_,_,_) :-
	write('shit.  noone agrees with anyone.  this is too crazy for me.'),nl.



% refineRule(+ProblemPrecondition,+ProblemAction,-Outcome)
% refines a rule according to the problem postcondition determined in findAgreement

refineRule(ProblemPrecond,ProbAction,Outcome) :-
	translatePrecond(ProblemPrecond,TransInfo),
	ProbAction =.. [ProbActionName|_Args],
	translatePred(ProbActionName,TransProbAction),
	ProblemPrecond =.. [ProbPrecondName|_OtherArgs],
	translatePred(ProbPrecondName,TransPrecond),
	attemptRefine(problemPrecond,TransInfo,_),
	attemptRefine(removePostcond,[TransPrecond,TransProbAction],Outcome).
	
	
% queryProblemPrecond(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,-NewInstantiation)
% returns the instantiation that the service providing agent believes the problem precondition ought to have and interprets the results
	
queryProblemPrecond(Me,Agent,class(Object,Class),class(Object,NewClass)) :-
	out(query(Me,Agent,question,class(Object,NewClass))),
	in_noblock(reply(Agent,Me,class(Object,class(Object,NewClass)))),
	Class \= NewClass.

queryProblemPrecond(_,_,class(_,_),failure) :-	
	write('postcondition seems to be correct.  it is not clear why it is causing failure (shouldnt really be here)'),nl.

queryProblemPrecond(Me,Agent,Precond,Answer) :-	    
	findRelevantInstantiation(Me,Agent,Precond,Answer).

queryProblemPrecond(_,_,_,_) :-
	write('i dont know why this has failed'),nl.


% findRelevantInstantiation(+PlanningAgent,+ServiceProvidingAgent,+Precondition,-InstantiatedPrecond)
% returns the instantiation that the service providing agent believes the problem precondition ought to have

findRelevantInstantiation(Me,Agent,Precond,InstPrecond) :-
	findPredInfo(Precond,Name,Arity),
	createList(Arity,List),
	UnInstPrecond =.. [Name|List],
	out(query(Me,Agent,question,UnInstPrecond)),
	in_noblock(reply(Agent,Me,InstPrecond)).


% createList(+Arity,-List)
% returns a list of arity Arity with all arguments uninstantiated

createList(Arity,List) :-
	createList(Arity,[],List).

createList(0,List,List).

createList(Arity,ListSoFar,List) :-
	NewArity is Arity - 1,
	createList(NewArity,[_|ListSoFar],List).



% queryProblemPrecondOldAgent(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,-NewInstantiation)
% queries the original action performing agent to return the correct instantiation of the precondtion according to that agent

queryProblemPrecondOldAgent(Me,OldAgent,class(Object,_),class(Object,NewClass)) :-
	out(query(Me,OldAgent,question,class(Object,NewClass))),
	in_noblock(reply(OldAgent,Me,class(Object,NewClass))).

queryProblemPrecondOldAgent(Me,OldAgent,ProblemPrecond,InstPrecond) :-	    
	findRelevantInstantiation(Me,OldAgent,ProblemPrecond,InstPrecond).




% translation predicates for preparing the relevant information for insertion into the KIF ontology:

% classTranslate(+Class,+SuperClass,-TransClass,-TransSuperClass)
% returns the class and superclass translated into KIF notation

classTranslate(Class,SuperClass,TransClass,TransSuperClass) :-
	translatePred(Class,TransClass),
	translatePred(SuperClass,TransSuperClass).


% propAATranslate(+PredName,+ClassofNewArg,+Position,+ArityPred,[-TransPredName,-TransNewType,-Position,-ArityPred)
% returns the translated information necessary for propositional translation
% the arity of the expected precond is increased to account for the fact that it has a situational argument in KIF but not in Prolog

propAATranslate(PredName,Type,Position,ArityExP,[TransPredName,NewType,NewType,Position,NewArityExP]) :-
	translatePred(PredName,TransPredName),
	translatePred(Type,NewType),
	NewArityExP is ArityExP + 1.


% propTranslateNewType(+OldType,+PredName,+Class,+Position,+ArityPred,[-TransOldType,-TransPredName,-TransType,-Position,-ArityExP])
% returns the translated information necessary for propositional translation when the class of the additional argument is not currently in the ontology

propTranslateNewType(OldType,PredName,Type,Position,ArityExP,[TransOldType,TransPredName,NewType,NewType,Position,NewArityExP]) :-
	% we need to make an appropriate sting of the predicate name.
	translatePred(OldType,TransOldType),
	translatePred(PredName,TransPredName),
	translateType(Type,NewType),
	NewArityExP is ArityExP + 1.


% translatePred(+PredName,-TransPredName)
% translates a name (for example, of a predicate) from Prolog to KIF notation

translatePred(PredName,TransPredName) :-
	name(PredName,[FirstPredNameChar|RestPredNameChars]),
	NewFirstChar is FirstPredNameChar - 32,
	translatePredRec(RestPredNameChars,TransPredChars),
	append([NewFirstChar],TransPredChars,FullTransPredChars),
	name(TransPredName,FullTransPredChars).


% translatePredRec(+Chars,-TransChars)
% recurses through all the characters of a name and translates them as appropriate

translatePredRec(Chars,TransChars) :-
	translatePredRec(Chars,[],TransChars).

translatePredRec([],TransSoFar,TransChars) :-
	reverse(TransSoFar,TransChars).

translatePredRec([FirstChar|RestChars],TransSoFar,TransChars) :-
	FirstChar < 91,
	FirstChar > 64,
	name('-',Dash),
	append([FirstChar],Dash,NewChar),
	append(NewChar,TransSoFar,NewTransSoFar),
	translatePredRec(RestChars,NewTransSoFar,TransChars).

translatePredRec([FirstChar|RestChars],TransSoFar,TransChars) :-	    
	translatePredRec(RestChars,[FirstChar|TransSoFar],TransChars).


% translateType(+Class,-NewClass)
% translates a class

translateType(Type,NewType) :-
	name(Type,[FirstTypeChar|RestTypeChars]),
	NewTypeChar is FirstTypeChar - 32,
	NewTypeChars = [NewTypeChar|RestTypeChars],
	name(NewType,NewTypeChars).

% translatePrecond(+Fact,[-TransFact,-TransSingleFact,-TransFirstIndiv])
% translates a precondition and returns its first argument.

translatePrecond(Fact,[TransFact,TransSingleFact,TransFirstIndiv]) :-
	Fact =.. [FactName|[FirstIndiv]],
	translatePred(FactName,TransName),
	translatePred(FirstIndiv,TransFirstIndiv),
	name('(',RightBracket),
	name(')',LeftBracket),
	name(' ',Space),
	name(TransName,NameChars),
	name(TransFirstIndiv,FirstIndivChars),
	append(RightBracket,NameChars,Fact1),
	append(Fact1,Space,Fact2),
	append(Fact2,FirstIndivChars,Fact3),
	append(Fact3,LeftBracket,TransFactChars),
	name(TransFact,TransFactChars),
	append(Fact2,LeftBracket,TransSingleChars),
	name(TransSingleFact,TransSingleChars).

translatePrecond(Fact,[TransFact,TransSingleFact,TransFirstIndiv]) :-
	Fact =.. [FactName|[FirstIndiv|RestArgs]],
	translatePred(FactName,TransName),
	translatePred(FirstIndiv,TransFirstIndiv),
	translatePrecondArgs(RestArgs,[],TransArgs),
	name('(',RightBracket),
	name(')',LeftBracket),
	name(' ',Space),
	name(TransName,NameChars),
	name(TransFirstIndiv,FirstIndivChars),
	append(RightBracket,NameChars,Fact1),
	append(Fact1,Space,Fact2),
	append(Fact2,FirstIndivChars,Fact3),
	append(Fact3,Space,Fact4),
	append(Fact4,TransArgs,Fact5),
	append(Fact5,LeftBracket,TransFactChars),
	name(TransFact,TransFactChars),
	append(Fact2,TransArgs,Fact6),
	append(Fact6,LeftBracket,TransSingleChars),
	name(TransSingleFact,TransSingleChars).


% translatePrecondArgs(+LastArg,+TransSoFar,-TransArgs)
% recursive section of translatePrecond, translating each argument

translatePrecondArgs([LastArg],TransSoFar,TransArgs) :-
	translatePred(LastArg,TransLast),
	name(TransLast,LastName),
	reverse(LastName,RevLast),
	append(RevLast,TransSoFar,NewTrans),
	reverse(NewTrans,TransArgs).

translatePrecondArgs([FirstArg|Rest],TransSoFar,TransArgs) :-
	translatePred(FirstArg,FirstTrans),
	name(FirstTrans,FirstName),
	name(' ',Space),
	append(FirstName,Space,NewFirstArg),
	reverse(NewFirstArg,RevFirst),
	append(RevFirst,TransSoFar,NewTrans),
	translatePrecondArgs(Rest,NewTrans,TransArgs).
	

% domTranslate(+SurprisingQuestion,+PlanningAgentDifferentArgument,+ServiceProvidingAgentDiffArg,+ArgumentClassList,[-TransPredName,-TransOldType,-TransNewType,-TransOldType,-Position])
% translates the information necessary for domain refinement

domTranslate(RSQ,MyDiff,HisDiff,ArgsClassList,[TransPredName,TransOldType,TransNewType,TransOldType,Position]) :-
	RSQ =.. [PredName|_],
	translatePred(PredName,TransPredName),
	findPos(ArgsClassList,MyDiff,RevPos,1),
	length(ArgsClassList,Length),
	FullLength is Length + 1,
	Position is FullLength - RevPos,
	translatePred(MyDiff,TransOldType),
	translatePred(HisDiff,TransNewType).


% findPos(+ArgsClassList,+Argument,-Position,+PositionSoFar)
% returns the position in a list of class arguments of a given argument

findPos([Arg|_],Arg,Position,Position).

findPos([_|Rest],Arg,Position,PosSoFar) :-
	NextPos is PosSoFar + 1,
	findPos(Rest,Arg,Position,NextPos).


% predTranslate(+OldName,+NewName,+Action,[-TransOldName,-TransNewName,-TransAction])
% translates the information necessary for predicate translation

predTranslate(OldName,NewName,Action,[TransOld,TransNew,TransAction]) :-
	translatePred(OldName,TransOld),
	translatePred(NewName,TransNew),
	Action =.. [ActionName|_],
	translatePred(ActionName,TransAction).


% precondDomTranslate(+PlanningAgent,+ServiceProvidingAgent,+Precondition,+RightClass,+WrongClass,[-TransRule,-TransRightClass,-TransWrongClass])
% translates the information necessary for domain translation

precondDomTranslate(_Me,_Agent,Action,class,RightClass,WrongClass,[TransRule,TransRightClass,TransWrongClass]) :-
	Action =.. [ActionName|_],
	translatePred(ActionName,TransRule),
	translatePred(RightClass,TransRightClass),
	translatePred(WrongClass,TransWrongClass).


% precondTranslate(+PlanningAgent,+ServiceProvidingAgent,+Action,+Precondition,[-TransAction,-TransPrecond])
% translates the information necessary for precondition translation
	
precondTranslate(Me,Agent,Action,Precond,[TransRule,TransPrecond]) :-
	Action =.. [ActionName|_],
	translatePred(ActionName,TransRule),
	Precond =.. [PrecondName|PrecondArgs],
	translatePred(PrecondName,TransName),
	translateArgs(Me,Agent,PrecondArgs,TransPrecondArgs,[]),
	name('(',RightBracket),
	name(TransName,PrecondChars),
	append(RightBracket,PrecondChars,Start),
	append(Start,TransPrecondArgs,MostPrecond),
	name(' ?Sit1',SitArg),
	name(')',LeftBracket),
	append(MostPrecond,SitArg,SitPrecond),
	append(SitPrecond,LeftBracket,FullPrecond),
	name(TransPrecond,FullPrecond),
	write('this is the precond I need to add: '),write(TransPrecond),nl.


% translateArgs(+PlanningAgent,+ServiceProvidingAgent,+ArgumentList,-TranslatedPrecond,+TransSoFar)
% translates each of the arguments of the new precondition, which may involve discussion with the service providing agent over classes.

translateArgs(_,_,[],TransPrecond,TransArgs) :-
	reverse(TransArgs,TransPrecond).

translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-
	FirstArg = uninstantiated,
	name(' Pseudo-Var',PV),
	reverse(PV,RevFirst),
	append(RevFirst,TransSoFar,NewTransSoFar),
	translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).
	
translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-
	% first, we need to find out the class of the first arg:
	class(FirstArg,FirstClass),
	write('I know that '),write(FirstArg),write(' is of class '),write(FirstClass),nl,
	name(' ?',ArgBegin),
	translatePred(FirstClass,TransFirst),
	name(TransFirst,TransFirstChars),
	append(ArgBegin,TransFirstChars,NewFirst),
	reverse(NewFirst,RevFirst),
	append(RevFirst,TransSoFar,NewTransSoFar),
	translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).

translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-	    
	% or ask the other agent:
	out(query(Me,Agent,question,class(FirstArg,FirstClass))),
	in_noblock(reply(Agent,Me,class(FirstArg,FirstClass))),
	write(Agent),write(' tells me that '),write(FirstArg),write(' is of class '),write(FirstClass),nl,
	name(' ?',ArgBegin),
	translatePred(FirstClass,TransFirst),
	name(TransFirst,TransFirstChars),
	append(ArgBegin,TransFirstChars,NewFirst),
	reverse(NewFirst,RevFirst),
	append(RevFirst,TransSoFar,NewTransSoFar),
	translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).


% findHisUnmatchedClass(+MyUnmatchedClasses,+HisUnmatchedClass,-UnmatchClass)
% calls emptyMyClasses

findHisUnmatchedClass(MyUnmatchedClasses,HisUnmatchedClasses,UnmatchedClass) :-
	emptyMyClasses(MyUnmatchedClasses,HisUnmatchedClasses,[UnmatchedClass]).


% emptyMyClasses(+MyUnmatchedClasses,+HisUnmatchedClasses,-[UnmatchedClass]).
% determines whether there is a class in HisUnmatchedClasses that is not also in MyUnmatchedClasses

emptyMyClasses([],Remainder,Remainder).

emptyMyClasses([_|MyRest],HisUnmatched,Remainder) :-
	member(['uninstantiated',Counter],HisUnmatched),
	deleteEl(HisUnmatched,['uninstantiated',Counter],NewUnmatched),
	emptyMyClasses(MyRest,NewUnmatched,Remainder).

% findClassAndPosition(+MyClasses,+HisClasses,-UnmatchedClass,-Position)
% returns the UnmatchedClass and its position

findClassAndPosition(MyClasses,HisClasses,UnmatchedClass,Position) :-
	reverse(MyClasses,RevMine),
	reverse(HisClasses,RevHis),
	findClassAndPosition(RevMine,RevHis,1,UnmatchedClass,Position).

findClassAndPosition([MyFirst|MyRest],[MyFirst|HisRest],PosSoFar,UnmatchedClass,Position) :-
	NewPos is PosSoFar + 1,
	findClassAndPosition(MyRest,HisRest,NewPos,UnmatchedClass,Position).

findClassAndPosition([MyFirst|_],_,Position,MyFirst,Position).



% checkFullyInstantiated(+Predicate)
% succeeds if predicate is fully instantiated; fails if any of the arguments are uninstantiated

checkFullyInstantiated(Predicate) :-
	Predicate =.. [_PredName|Arguments],
	checkArgsInstantiated(Arguments).

checkArgsInstantiated([]).

checkArgsInstantiated([FirstArg|Rest]) :-
	\+ FirstArg = fullInstantiationCheck,
	checkArgsInstantiated(Rest).


% findRightNegation(+MostRecentQuery,+PrecondList,-RightNegation)
% when a precondition is negated, returns the correct way in which this should be done (negate if already positive, make positive if already negated

findRightNegation(MostRecentQuery,PrecondList,posToNeg) :-
	member(MostRecentQuery,PrecondList),
	write('have to negate this '),nl.

findRightNegation(MostRecentQuery,PrecondList,negToPos) :-
	member(not(MostRecentQuery),PrecondList),
	write('have to unnegate this '),nl.


% findIndividual(+Position,+Query,-InstantiatedArgument)
% returns the argument in a given position of a query

findIndividual(Position,Query,InstArg) :-
	Query =.. [_|QueryArgs],
	findIndivArg(Position,QueryArgs,InstArg).

findIndivArg(1,[InstArg|_],InstArg).

findIndivArg(Position,[_FirstArg|RestArgs],InstArg) :-
	NewPosition is Position - 1,
	findIndivArg(NewPosition,RestArgs,InstArg).
