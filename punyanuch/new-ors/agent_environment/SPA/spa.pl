%:- use_module(library('linda/client')).




% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).



spa(MyName) :-
    nl,write('I am '),write(MyName),nl,
    listen(MyName).

% note: spa1 is for spa's which have kif onts and therefore need to translate.  ultimately this will replace spa.
spa1(MyName) :-
	nl,write('I am '),write(MyName),nl,
	scenario_name(Scenario),
	ontoType(OntType),
	copyOntFiles(OntType,MyName,Scenario),
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translation', Ontol_transAbsolutePath),
	consult(Ontol_transAbsolutePath),nl,nl,
	find_ontology_path(Scenario,MyName,ScenarioPath),
	translation(Scenario,ScenarioPath),
        atom_concat(ScenarioPath, '/centralThy', ThyPath),
	atom_concat(ScenarioPath, '/centralSig', SigPath),
	reconsult(ThyPath),
	reconsult(SigPath),
	listen(MyName).

listen(MyName) :-
    in(query(QAgent,MyName,QueryType,Query)),!,
    write(MyName),write(' has received a query ..'),nl,
    write(Query),write(' '),write(QueryType),nl,nl,
    resolve(MyName,QueryType,Query,QAgent),
    listen(MyName).

% resolve(+QueryType,+Query,+QAgent): solves the query and sends the answer to the querying agent.


resolve(MyName,request,Query,QAgent) :-
	solve(MyName,request,Query,Answer,QAgent),
	write('I am saying '),write(Answer),write(' to '),write(Query),nl,
	out(reply(MyName,QAgent,Query,Answer)).

resolve(MyName,QueryType,Query,QAgent) :-
	solve(MyName,QueryType,Query,Answer,QAgent),
	write('I am saying '),write(Answer),write(' to '),write(Query),nl,
	out(reply(MyName,QAgent,QueryType,Answer)).

resolve(MyName,_,Query,QAgent) :-
	out(reply(MyName,QAgent,Query,problem)).


% solve(+QueryType,+Action,-Outcome,+QueryAgent): tries to perform Action, which is of QueryType, for the QueryingAgent, and returns the Outcome.

solve(_MyName,request,Action,problem,_Agent) :-
	tasksIPerform(TasksList),	
	Action =.. [ActionName|_],
	\+ member(ActionName,TasksList),
	write('don\'t perform '),write(Action),nl,nl.	

solve(MyName,request,Action,ok,Agent) :-
	write('I want to perform '),nl,write(Action),write('for '),write(Agent),nl,nl,
	tasksIPerform(TasksList),	
	Action =.. [ActionName|_],
	write('TASKS I PERFORM '), write(TasksList),nl,write('ACTION'),write(Action),nl,write('ACTION NAME'),write(ActionName),nl,nl,
	member(ActionName,TasksList),
	rule(Action,[_,Preconds,_]),
	ask(AskList),
	wait(WaitList),
	write('I\'m checking preconds...'),nl,nl,
	checkPreconds(MyName,Preconds,AskList,WaitList,Agent,[],[]),
	write(Action),write(' has been completed satisfactorily.'),nl,nl.

solve(_MyName,request,_,problem,_) :-
	write('Sorry, I cannot perform the task you request!'),nl,nl.

solve(_MyName,question,performTask(Action,_),Answer,_Agent) :-
	write('question is '),write('performTask('),write(Action),write(')'),nl,
	write('checking tasks ...'),
	tasksIPerform(TasksList),
	write(TasksList),nl,
	Action =.. [Pred|_Args],
	( member(Pred,TasksList) ->
	    Answer = yes
	;   
	    Answer = no
	),
	write('answer is '),write(Answer).

solve(_MyName,question,Question,Question,_Agent) :-
	nonFacts(PredList),
        Question =.. [QuePred|_Args],
	member(QuePred,PredList),
	Question.

solve(_MyName,question,Question,Question,_Agent) :-
	fact(Question).

solve(_MyName,truth,class(Thing,Class),yes,_Agent) :-
	checkClass(class(Thing,Class)).

solve(_MyName,truth,class(_Thing,_Class),no,_Agent).

solve(_MyName,truth,subclass(Class1,Class2),yes,_Agent) :-
	checkSubClass(Class1,Class2).

solve(_MyName,truth,subclass(_Class1,_Class2),no,_Agent).

solve(_MyName,truth,Query,yes,_Agent) :-
	nonFacts(PredList),
	member(Query,PredList),
	Query.

solve(_MyName,truth,Query,yes,_Agent) :-
	fact(Query).

solve(_MyName,truth,_Query,no,_Agent).


solve(MyName,repair,no,Agent).


%checkPreconds(+Spa,+PrecondList,+AskList,+WaitList,+QueryAgent,+ThingsToAsk,+WaitingThings): in order to perform an action, the agent must check that all the Preconds are valid.  Some of these the agent can check itself; others must be asked of the QueryAgent (these types of things are listed in the AskList and built up in ThingsToAsk); others - such as calculations - can only be evaluated once all variables have been instantiated (these types of things are lists in WaitList and built up in WaitingThings). When checking classes: either the agent knows that Thing is of type Class (great), or that thing is of a different class and this different class (which leads to failure: we can assume this wrongclass is not a subclass of Class, or else the first checkClass would have succeeded), or it knows nothing and asks the PA.

checkPreconds(MyName,[],_,_,Agent,ThingsToAsk,WaitingThings) :-
	checkWithAgent(MyName,ThingsToAsk,Agent),!,
	checkWaitingThings(WaitingThings).


% checkPreconds(MyName,[class(Thing,Class)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
% 	checkClass(class(Thing,Class)),!,
% 	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

% checkPreconds(_MyName,[class(Thing,Class)|_T],_AskList,_WaitList,_Agent,_ThingsToAsk,_WaitingThings) :-
% 	class(Thing,WrongClass),!,
%         Class \= WrongClass,       
%         fail.

checkPreconds(MyName,[class(Thing,Class)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :- !,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[class(Thing,Class)|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	member(Pred,AskList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[not(H)|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	\+ Pred = not,
	member(Pred,AskList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[H|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	\+ Pred = not,
	member(Pred,WaitList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,[H|WaitingThings]).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	nonFacts(PredList),
	member(H,PredList),
	\+ H,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	nonFacts(PredList),
	member(H,PredList),
	H,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	\+ fact(H),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	fact(H),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[calculation(Sum)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	Sum,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).



%checkWithAgent(+Spa,+ListOfQuestions,+Agent): all those preconditions which require an answer are put to Agent.
%Note: the variable binding Question = Answer is inelegant: better to use the same variable name and if the binding is impossible, we fail.  However, this means that the spa will only look for messages such that Answer can match Question: if Answer is something unexpected (such as 'no'), then the spa will wait indefinitely for message that will match, rather than this predicate failing.

checkWithAgent(_MyName,[],_).

checkWithAgent(MyName,[Question|T],Agent) :- 
	write('have asked '),write(Agent),write(' about '),write(Question),nl,
	out(query(MyName,Agent,question,Question)),
	in(reply(Agent,MyName,question,Answer)),
	write(Agent),write(' has replied '),write(Answer),nl,
	Question = Answer,
	checkWithAgent(MyName,T,Agent).

	    
%checkWaitingThings(+ListOfWaitingThings): once all the other preconditions have been evaluated, all possible variables have been bound, so the list of waiting things can be evaluated.

checkWaitingThings([]).

checkWaitingThings([calculation(Sum)|T]) :-
	Sum,!,
	checkWaitingThings(T).

checkWaitingThings([H|T]) :-
	fact(H),
	checkWaitingThings(T).


%checkClass(class(?Object,?Class)): if given, evaluates whether Objects is of class Class (or a subclass of Class); otherwise, instantiates these variables appropriately.

checkClass(class(Object,Class)) :-
	class(Object,Class).

checkClass(class(Object,Class)) :-
	subclass(SubClass,Class),
	checkClass(class(Object,SubClass)).

%checkSubClass(?SubClass,?Class): succeeds if Subclass is a subclass of Class, or, if these are variables, there is an instantiation which makes this possible.

checkSubClass(SubClass,Class) :-
	subclass(SubClass,Class).

checkSubClass(SubClass,Class) :-
	subclass(MidClass,Class),
	checkSubclass(SubClass,MidClass).




copyOntFiles(sumo,_MyName,_Scenario).

copyOntFiles(onto,MyName,Scenario) :-
	find_original_ontology_path(Scenario,MyName,OrigScenarioPath),
	find_ontology_path(Scenario,MyName,ScenarioPath),
	atom_concat(OrigScenarioPath,'/ont.in',OrigOnt),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	see(OrigOnt),tell(OntIn),
	copyOntFile,
	told,seen,
	atom_concat(OrigScenarioPath,'/metaOnt.in',OrigMetaOnt),
	atom_concat(ScenarioPath,'/metaOnt.in',MetaOntIn),
	see(OrigMetaOnt),tell(MetaOntIn),
	copyOntFile,
	told,seen.


copyOntFile :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyThisLine(Rest)
	;
	    copyThisLine(Line),
	    copyOntFile
	).


% readLine(-Text)
% reads the current stream until it reaches the end of the line

readLine(Text):-
   get_code(C),
   addtoline(C, Text).

% addtoline(+character,-text)
% checks if this is a new line or end of file character.  if not builds adds it to the text of the line and returns to readLine.
 
addtoline(C, []):-
    newline(C).

addtoline(C,[end]) :-
	endoffile(C).
 
addtoline(C, [C|More]):-
    readLine(More).

readLine(Stream,Text):-
	get_code(Stream, C),
	addtoline(Stream, C, Text).

addtoline(_, C, []):-
	newline(C).

addtoline(_, C, [end]):-
	endoffile(C).

addtoline(Stream, C, [C|More]):-
	readLine(Stream, More).

newline(10).
 
endoffile(-1).

% extractLast(+List,?Last,-Rest)
% returns the last element of a list (which may or may not be specified: if specified, it will fail if the last is not the specified element), together with the rest of the list (with the last element removed).

extractLast(List,Last,Rest) :-
	extractLast(List,Last,[],RevRest),
	reverse(RevRest,Rest).
	
extractLast([H],H,Rest,Rest).

extractLast([H|T],Last,RestSoFar,Rest) :-
	extractLast(T,Last,[H|RestSoFar],Rest).

% copyThisLine(+Line)
% write Line as is to the out stream

copyThisLine(Line) :-
	name(LineName,Line),
	write(LineName),nl.




find_ontology_path(Scenario,MyName,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/SPA/',Scenario3),
	atom_concat(Scenario3,MyName,Scenario4),
	atom_concat(Scenario4,'/updated',ScenarioPath).


find_original_ontology_path(Scenario,MyName,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/SPA/',Scenario3),
	atom_concat(Scenario3,MyName,Scenario4),
	atom_concat(Scenario4,'/original',ScenarioPath).




matchExpression(Before,Identifier,After,Whole) :-
	findPosition(Identifier,Whole,Pos1,Pos2),
	findBefore(Whole,Pos1,[],Before),
	findAfter(Whole,Pos2,After).

% findPosition(+Identifier,+Whole,-BeginPos,-AfterPos)
% returns the start and end positions of the identifier within whole

findPosition([FirstID|Rest],Whole,Pos1,Pos1,EndPos) :-
	nth1(Pos1,Whole,FirstID),
	findPosition([FirstID|Rest],Whole,Pos1,EndPos).

findPosition([Last],Whole,EndPos,EndPos) :-
	nth1(EndPos,Whole,Last).

findPosition([FirstID|Rest],Whole,CurrentPos,EndPos) :-
	nth1(CurrentPos,Whole,FirstID),
	NextPos is CurrentPos + 1,
	findPosition(Rest,Whole,NextPos,EndPos).


% findBefore(+Whole,+Position,+BeforeSoFar,-Before)
% returns what comes before position in whole

findBefore(_,1,RevBefore,Before) :-
	reverse(RevBefore,Before).

findBefore([FirstWhole|RestWhole],Counter,BeforeSoFar,Before) :-
	NewCounter is Counter - 1,
	findBefore(RestWhole,NewCounter,[FirstWhole|BeforeSoFar],Before).

% findAfter(+Whole,+Position,-After)
% returns what comes after position in whole

findAfter(Whole,Pos2,After) :-
	length(Whole,End),
	Start is Pos2 +1,
	findAfter(Whole,Start,End,[],After).

findAfter(_,Last,End,RevAfter,After) :-
	Last is End + 1,
	reverse(RevAfter,After).

findAfter(Whole,Counter,End,AfterSoFar,After) :-
	nth1(Counter,Whole,Element),
	NewCounter is Counter + 1,
	findAfter(Whole,NewCounter,End,[Element|AfterSoFar],After).


deleteEl(List,El,Ans) :- 

	deleteEl(List,El,[],Ans). 



deleteEl([H|T],El,SoFar,Ans) :-

	H == El,
	append(SoFar,T,Ans).



deleteEl([H|T],El,Ans,X) :- 

	append(Ans,[H],SoFar), 
	deleteEl(T,El,SoFar,X). 
