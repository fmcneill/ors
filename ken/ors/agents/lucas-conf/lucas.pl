:- use_module(library(system)),use_module(library(lists)),  
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(charsio)).


:- dynamic justification/1.


% this is the code for the planning agent, who is able to form plans, execute them through communication with other agents, and diagnose and refine mismatches when necessary, which leads on to replanning until either the goal is achieved, it becomes impossible to form a plan, or refinement cannot be done.


% plan/0 loads the necessary files and finds the goal.

plan :-
	nl,nl,nl,write('consulting the translator ...'),nl,nl,
	reconsult('translation/translation'),
	nl,nl,nl,write('consulting the plan finder ...'),nl,nl,
	reconsult('planning/planfinder'),
	nl,nl,nl,write('consulting the plan deconstructor ...'),nl,nl,
   	reconsult('planning/planDecon'),
	nl,nl,nl,write('consulting the ontology updater ...'),nl,nl,
	reconsult('update/update'),
	nl,nl,nl,write('consulting the diagnostic algorithm ...'),nl,nl,
	reconsult('diagnosis/diagnosticAlgorithm'),
	nl,nl,nl,write('consulting the refinement system ...'),nl,nl,
	reconsult('refinement/refinement'),
	reconsult('refinement/metaRefinement'),
	nl,nl,nl,write('What is the goal? '),nl,nl,
	read(GoalIn),
	defaultGoal(GoalIn,Goal),
	plan(Goal,[]).


% plan(+Goal,+RefinementsSoFar)
% performs the translation process and finds the plan.  refinementsSoFar so far will be an empty list if this is the first time of planning, but if replanning is occurring, it will not be empty.

plan(Goal,RefinementsSoFar) :-
	write('goal is: '),write(Goal),nl,
	write('translating ...'),nl,nl,
	translation(Goal),
	write('need to find a plan ...'),nl,nl,
	findPlan(Plan),
	plan(Goal,Plan,RefinementsSoFar).


% plan(+Goal,+Plan,+RefinementsSoFar)
% checks that a valid plan has been produced, reads the newly translated ontology, deconstructs the plan and begins to process it.

plan(_Goal,fail,RefinementsSoFar) :-
	write('  all is lost'),nl,nl,
	write('refinements performed so far are: '),nl,
	write(RefinementsSoFar).

plan(Goal,Plan,RefinementsSoFar) :-
	write('This is the plan:'),nl,
	write(Plan),nl,nl,nl,
	reconsult('centralSig'),
	reconsult('centralThy'),
	deconstruct(Plan,Just),!,
	write('deconstructing the plan ...'),
	processPlan(Plan,Goal,Just,RefinementsSoFar).
	
plan(_,fail,_) :-	    
	write('problem with the deconstruction'),nl.



% defaultGoal(+GivenGoal,-RealGoal)
% debugging short cut - has default goals built in so that it is not always necessary to type them out.

defaultGoal('l','attendConference(lucas,cade)').
defaultGoal('m','atGoal(lucas,goal)').
defaultGoal('s','knows(agentB,communicationA)').
defaultGoal('p','simple(occ)').
defaultGoal('a','memberOrganization(researchAgent,refinementSig)').
defaultGoal('li','and(served(personOne),served(personTwo),served(personThree))').
defaultGoal('b','correctOrder(blockX,blockY,blockZ)').
defaultGoal(Other,Other).


% processPlan(+Plan,+Goal,+Justification,+RefinementsSoFar)
% builds up the necessary lists to begin plan execution.

processPlan(Plan,Goal,Just,RefinementsSoFar) :-
	nl,nl,nl,write('executing the plan ...'),nl,nl,nl,
	processPlan(Plan,[[start]],[[start]],Goal,Just,RefinementsSoFar).


% processPlan(+RemainingPlan,-AssertedFacts,-RetractedFacts,+Goal,+Justification,+RefinementList)
% executes the first plan step, unless the plan is finished, and recurses.  If the plan is finished, updates the KIF ontology according to what has been asserted and retracted during plan execution, and prints a list of what refinements have been performed

processPlan([],Asserted,_Retracted,_,Just,RefinementList) :-
	findFinalSituation(Just,Situation),
	updateKIFOnt(Asserted,Situation),
        write('The plan is completed'),nl,nl,
	write('The following refinements have been performed: '),nl,
	write(RefinementList),nl,nl.

processPlan(failure,Asserted,_Retracted,_,_,RefinementList) :-
	updateKIFOnt(Asserted,_),
	nl,nl,write('Attempting to execute the plan resulted in failure'),nl,nl,
	write('The following refinements have already been performed: '),
	write(RefinementList),nl,nl.
 
processPlan([H|T],Asserted,Retracted,Goal,Just,RefinementList) :- 
        performAction(H,Outcome,AssertedHere,RetractedHere,Asserted,Goal,Just,RefinementList,Response),!,
	checkThisAction([H|T],Outcome,AssertedHere,RetractedHere,Asserted,Retracted,Goal,Just,RefinementList,Response).


% performAction(+Action,-Outcome,-Asserted,-Retracted,+AssertedSoFar,+Goal,+Justification,+RefinementList,-Response)
% executes action by locating the appropriate information about the action rule, contacting the appropriate agent, and interpreting the returned information. 

performAction(Action,Outcome,Asserted,Retracted,AssertedSoFar,Goal,Just,RefinementList,Response) :-
	locateRule(Action,Just,Preconds,Postconds),
	query(request,Action,Answer,CorrectAgent,QueryList,Preconds,SurprisingQuestions,Response),
	resolveAction(Action,Outcome,Asserted,Retracted,AssertedSoFar,Goal,Just,Preconds,Postconds,CorrectAgent,QueryList,Answer,SurprisingQuestions,RefinementList,Response).


% resolveAction(+Action,?ExpectedOutcome,-Asserted,-Retracted,+AssertedSoFar,+Goal,+Justification,+Preconditions,+Postconditions,+CorrectAgent,+QueryList,+Answer,+SurprisingQuestions,+RefinementList,+Response).
% interprets the results of the query, noting when communication failure has occurred, when refinement is necessary, when it is not possible to update the ontology (should never happen), and when all has progressed satisfactorily.  if refinement is necessary, one must first update the KIF ontology with all the facts that have been asserted and retracted thus far, as planning is temporarily suspended.

resolveAction(Action,_,_,_,_,_,_,_,_,CorrectAgent,_,_,_,_,fail) :-
	write('there was a problem contacting '),write(CorrectAgent),write('; therefore '),write(Action),write(' could not be performed'),nl,nl.

resolveAction(Action,ExpectedOutcome,Asserted,Retracted,_,_,_,_,Postconds,CorrectAgent,_,ok,_,_,_) :-
	summary(Action,ExpectedOutcome),!,
	processPostconds(CorrectAgent,Postconds,[],[],Asserted,Retracted).

resolveAction(Action,plan_failure,_,_,_,_,_,_,_,_,_,ok,_,_,_) :-
	summary(Action,plan_failure),!,
	write('I could not update the ontology appropriately'),nl.

resolveAction(Action,refined,[none],[none],AssertedSoFar,Goal,Just,_,_,CorrectAgent,QueryList,_,SurprisingQuestions,RefinementList,_) :-
	summary(Action,plan_failure),!,
	findSituation(Action,Just,Situation),
	updateKIFOnt(AssertedSoFar,Situation),
	write('Im diagnosing what the problem is ...'),nl,nl,
	diagnoseFailure(lucas,Action,CorrectAgent,QueryList,Just,SurprisingQuestions,success,Refinement),
	append([Refinement],RefinementList,NewRefinementList),
	plan(Goal,NewRefinementList).

resolveAction(Action,failure,[none],[none],_,_,_,_,_,_,_,_,_,_,_) :-
	summary(Action,plan_failure),!,
	write('It was impossible to diagnose the problem.'),nl,nl.
	

% checkThisAction(+Plan,+Outcome,+AssertedHere,+RetractedHere,+Asserted,+Retracted,+Goal,+Justification,+RefinementList,+Response)
% after the action has been performed, verifies what the outcome of the attempt to perform it was and, if possible, continues with the processing of the plan

checkThisAction(_,_,_,_,_,_,_,_,_,fail) :-
	write('one action was not performable due to agent communication problems.  hence the goal cannot be reached.'),nl,nl.

checkThisAction(_,failure,_,_,_,_,_,_,_,_) :-
	nl,nl,nl,
	write('** WARNING **'),nl,nl,
	write('Im afraid that it was impossible to achieve the goal.  A problem arose during executiong, and the ontology could not be refined appropriately to overcome this problem.  Details of the problem and the information that is required to refine it are given above.').

checkThisAction(_,refined,_,_,_,_,_,_,_,_).

checkThisAction([H|T],_,AssertedHere,RetractedHere,Asserted,Retracted,Goal,Just,RefinementList,_) :-
	processPlan(T,[[H,AssertedHere]|Asserted],[[H,RetractedHere]|Retracted],Goal,Just,RefinementList).


% summary(+Action,+Outcome)
% writes a summary line of the outcome of action

summary(Action,ok) :-
	write(Action),write(' completed satisfactorily'),nl,nl.

summary(Action,plan_failure) :-
	write('plan has failed at '),write(Action),nl,nl.


% query(+QueryType,+Action,-Outcome,-CorrectAgent,-QueryList,+ExpectedQuestions,-SurprisingQuestions,-Reponse)
% finds the correct agent, sends a request to perform the task and waits for the response

query(request,Task,ok,noAgent,[],[],[],ok) :-
	chooseAgent(noAgent,Task),!.

query(request,Task,Outcome,CorrectAgent,QueryList,ExpectedQuestions,SurprisingQuestions,Response) :-
	chooseAgent(CorrectAgent,Task),!,
	write('im going to ask '),write(CorrectAgent),write(' to perform '),write(Task),write(' for me'),nl,
	out(query(lucas,CorrectAgent,request,Task)),
	waitForAnswer(CorrectAgent,Outcome,ExpectedQuestions,[request],[],QueryList,SurprisingQuestions,Response).


% waitForAnswer(+CorrectAgent,-Outcome,+ExpectedQuestions,-CurrentQuery,-CurrentSurprisingQuestion,+QueryList,+LastestSurprisingQuestions,-Outcome)
% listens for a reply, which may be either an outcome to the action or a request for more information.  if a query is received, this is checked against expected queries to see if it is surprising.  if no reply is forthcoming, sleeps for 2 seconds and tries again.  if there is still no answer, a failure notice is passed returned.

waitForAnswer(SolvingAgent,Outcome,_ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,CurrentQuery,CurrentSurprisingQuestions,ok) :-
	in_noblock(reply(SolvingAgent,lucas,Outcome)).

waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions,ok) :-
	in_noblock(query(SolvingAgent,lucas,Type,Ques)),
	solve(Type,Ques,Answer,SolvingAgent),
	out(reply(lucas,SolvingAgent,Type,Answer)),
	analyseQuestion(Ques,SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions).

waitForAnswer(SolvingAgent,Outcome,_ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,CurrentQuery,CurrentSurprisingQuestions,ok) :-
	sleep(2),
	in_noblock(reply(SolvingAgent,lucas,Outcome)).

waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions,ok) :-
	sleep(2),
	in_noblock(query(SolvingAgent,lucas,Type,Ques)),
	solve(Type,Ques,Answer,SolvingAgent),
	out(reply(lucas,SolvingAgent,Type,Answer)),
	analyseQuestion(Ques,SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions).

waitForAnswer(SolvingAgent,_Outcome,_ExpectedQuestions,_CurrentQuery,_CurrentSurprisingQuestions,_QueryList,_LatestSurprisingQuestions,fail) :-
	write(SolvingAgent),write(' wont bloody talk to me '),nl.


% analyseQuestion(+Ques,+SolvingAgent,-Outcome,+ExpectedQuestions,+CurrentQuery,+CurrentSurprisingQuestions,+QueryList,+LasestSurprisingQuestions)
% assesses whether the question was surprising or not and adds it to the list of surprising questions if necessary.

analyseQuestion(Ques,SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions) :-
	member(Ques,ExpectedQuestions),
	waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,[Ques|CurrentQuery],CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions,ok).

analyseQuestion(Ques,SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions) :-
	member(not(Ques),ExpectedQuestions),
	waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,[Ques|CurrentQuery],CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions,ok).

analyseQuestion(Ques,SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions) :-
	waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,[Ques|CurrentQuery],[Ques|CurrentSurprisingQuestions],QueryList,LatestSurprisingQuestions,ok).



% solve(+type,+Question,-Answer,+SolvingAgent)
% attempts to respond to a question, which may be a request to perform an action (which planning agent cannot do) or to answer a question.
      
solve(request,_,failed,SolvingAgent) :-
	Message = 'sorry, i dont perform actions',
	out(reply(qAgent,SolvingAgent,Message,_)).

solve(question,Question,Question,_) :-
	nonFacts(NonFactList),
	Question =.. [QuestionName|_],
	member(QuestionName,NonFactList),
	Question.

solve(question,Question,no,_) :-
	nonFacts(NonFactList),
	Question =.. [QuestionName|_],
	member(QuestionName,NonFactList).

solve(question,Question,Question,_) :-
	fact(Question).

solve(question,_,no,_).


% locateRule(+Action,+Justification,-Preconditions,-Postconditions)
% finds a specific action from the justification and returns its preconditions and postconditions.

locateRule(_,[],_,_) :-
	write('this rule does not appear to be executable'),nl,
	fail.

locateRule(Action,[[Action,_,Preconds,Postconds,_,_]|_],Preconds,Postconds).

locateRule(Action,[_|RestJust],Preconds,Postconds) :-
	locateRule(Action,RestJust,Preconds,Postconds).
	

% processPostconds(+Agent,+Postconditions,+AssertedSoFar,+RetractedSoFar,+Asserted,+Retracted)
% updates the temporary Prolog ontology which the agent is using by processing the postconditions of the action.

processPostconds(_,[],Asserted,Retracted,Asserted,Retracted).

processPostconds(Agent,[inform(Condition)|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	askForInfo(Agent,Condition),!,
	test,
	processPostconds(Agent,T,[Condition|AssertedSoFar],RetractedSoFar,Asserted,Retracted).

processPostconds(Agent,[not(Condition)|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	retract(fact(Condition)),
	processPostconds(Agent,T,AssertedSoFar,[not(Condition)|RetractedSoFar],Asserted,Retracted).

processPostconds(Agent,[calculation(Calc)|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	Calc,
	processPostconds(Agent,T,AssertedSoFar,RetractedSoFar,Asserted,Retracted).

processPostconds(Agent,[class(Object,Class)|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	assert(class(Object,Class)),
	processPostconds(Agent,T,[class(Object,Class)|AssertedSoFar],RetractedSoFar,Asserted,Retracted).
	
processPostconds(Agent,[H|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	assert(fact(H)),
	processPostconds(Agent,T,[H|AssertedSoFar],RetractedSoFar,Asserted,Retracted).

test.

% instantiatePreconds(+Preconditions)
% ensures that all the arguments of the preconditions are instantiated.

instantiatePreconds([]).

instantiatePreconds([H|T]) :-
	fact(H),!,
	instantiatePreconds(T).

instantiatePreconds([not(H)|T]) :-
	\+ fact(H),!,
	instantiatePreconds(T).

instantiatePreconds([calculation(Sum)|T]) :-
	Sum,
	instantiatePreconds(T).

instantiatePreconds([not(calculation(Sum))|T]) :-
	\+ Sum,
	instantiatePreconds(T).

instantiatePreconds([class(Thing,Class)|T]) :-
	checkClass(Thing,Class),
	instantiatePreconds(T).

instantiatePreconds([not(class(Thing,Class))|T]) :-
	\+ checkClass(Thing,Class),
	instantiatePreconds(T).

instantiatePreconds([H|T]) :-
	H,
	instantiatePreconds(T).

instantiatePreconds([not(H)|T]) :-
	\+ H,
	instantiatePreconds(T).


% askForInfo(+Agent,+Postcond)
% asks the appropriate agent how postconditions should be instantiated where necessary

askForInfo(Agent,Postcond) :-
	out(query(lucas,Agent,question,Postcond)),
	sleep(1),
	(   in_noblock(reply(Agent,lucas,Postcond))
	;
	    sleep(2),
	    in_noblock(reply(Agent,lucas,Postcond))
	),
	assert(fact(Postcond)).


% chooseAgent(-Agent,+Task)
% returns the appropriate agent to perform task

chooseAgent(Agent,Task) :-
	write_to_chars(Task,TaskChar),
	name('(',RightBracket),
	matchExpression(ActionName,RightBracket,_,TaskChar),
	agentNeeded(Agent,Action),
	write_to_chars(Action,ActionName).


findSituation(Action,[[Action,_,_,_,_,Situation]|_],Situation).

findSituation(Action,[_|RestJust],Situation) :-
	findSituation(Action,RestJust,Situation).

findFinalSituation([[_,_,_,_,_,Situation]|_],Situation).
	





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client
initialiseClient(PId):-
    see('server.addr'),
    read(Host:Port-PId),
    seen,
    linda_client(Host:Port).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%

% Main start server predicate
startServer(PId):-
        file_exists('server.addr'),    % if file "server.addr" (handle) exists
        see('server.addr'),            % open it,
        read(_:_-PId),                 % get PId,
        seen,                          % close it, and
        up(PId),                       % (1) check that process is up
        !.
startServer(PId):-                         % otherwise...
        createServerProgram,               % create server program "server.pl"
        startServerAux,                    % start it up
        sleep(2),                          % wait a bit... (to update file)
        see('server.addr'),                % open file with Linda handle
        read(_:_-PId),                     % get PId
        seen.                              % close it

% Check if process is running
up(PId):-
        concat(['ps -p ',PId,' | wc'],Command), % type this command in UNIX
        exec(Command,[null,pipe(Out),null],_),  % and PId will be running
        get(Out,50).                            % if first returned Char is 2

% String Concatenation
concat(ListStrings,Concat):-
    ListStrings = [Str1|Strings],
    concatList(Strings,Str1,Concat).
concatList([],String,String).
concatList([S|Ss],StringSoFar,String):-
    concat(StringSoFar,S,NewStringSoFar),
    concatList(Ss,NewStringSoFar,String).
concat(Str1,Str2,Str1andStr2):-
    name(Str1,ASCStr1),
    name(Str2,ASCStr2),
    append(ASCStr1,ASCStr2,ASCStr1andStr2),
    name(Str1andStr2,ASCStr1andStr2).

% Create a program whih will start a LINDA Server
createServerProgram:-
    tell('server.pl'),
    write(':- use_module(library(\'linda/server\')),'),nl,
    write('   use_module(library(\'system\')),'),nl,
    write('   pid(PId),'),nl,
    write('   linda((Host:Port)-(tell(\'server.addr\'),'),nl,
    write('                      '),
    write('write(\''),write(\),write('\'\'),'),nl,
    write('                      '),
    write('write(Host),'),nl,
    write('                      '),
    write('write(\''),write(\),write('\':\'),'),nl,
    write('                      '),
    write('write(Port-PId),'),nl,
    write('                      '),
    write('write(\'.\'),'),nl,
    write('                      '),
    write('told)).'),
    told.

% Execute server.pl file
startServerAux:-
    exec('echo "[\'server.pl\']." | sicstus > /dev/null &',
         [null,null,null],
          _).

%%%%%%%%%%%%%%%%%%%%%% START SERVER AND CLIENT

:- startServer(_),initialiseClient(_).










