:- use_module(library(system)),use_module(library(lists)),  
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(charsio)).


:- dynamic justification/1.




lucas :-
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
	nl,nl,nl,write('What is the goal? '),nl,nl,
	read(GoalIn),
	defaultGoal(GoalIn,Goal),
	lucas(Goal).

lucas(Goal) :-
	write('goal is: '),write(Goal),nl,
	write('translating ...'),nl,nl,
	translation(Goal),
	write('need to find a plan ...'),nl,nl,
	findPlan(Plan),
	write('This is the plan:'),nl,
%	Plan = [convertPaper(lucas,isabellePaperDvi,isabellePaperPs),submitPaper(lucas,isabellePaperPs,cade),register(lucas,cade),findConfAccomInfo(lucas,cade),bookAccom(lucas,cade),bookFlight(lucas,cade),reimburse(lucas,cade)],
	write(Plan),nl,nl,nl,
	reconsult('centralSig'),
	reconsult('centralThy'),
	(   deconstruct(Plan,Just),!,
	    write('deconstructing the plan ...'),
	    assert(justification(Just))
	;
	    write('problem with the deconstruction'),nl, fail
	),
	process_plan(Plan,Goal).


defaultGoal('m','atGoal(lucas,goal)').
defaultGoal(Other,Other).



% we must process the plan by performing every step.
% firstly, we need, over the course of processing the plan, to build up a list of everything that has been asserted and retracted at each action so that we know how to translate these changes into the KIF ontology.

process_plan(Plan,Goal) :-
	nl,nl,nl,write('executing the plan ...'),nl,nl,nl,
	process_plan(Plan,[[start]],[[start]],Goal).


% when there are no more steps to perform, the plan is completed. 
process_plan([],Asserted,_Retracted,_) :-
	updateKIFOnt(Asserted),
        write('The plan is completed'),nl,nl.


% if the plan ends in failure, a message is returned.  This is just a catch all - the specific cases should have been dealt with and refinement performed by this stage.  This should eventually have an final line calling for replanning.
process_plan(failure,Asserted,_Retracted,_) :-
	updateKIFOnt(Asserted),
	nl,nl,write('Attempting to execute the plan resulted in failure'),nl,nl.
 
process_plan([H|T],Asserted,Retracted,Goal) :- 
        perform_action(H,Outcome,AssertedHere,RetractedHere,Asserted,Goal),!,
	(   Outcome = failure,
	    diagnose_failure(lucas,Action,CorrectAgent,QueryList,Just,SurprisingQuestions,DiagnosisOutcome),
	    (	DiagnosisOutcome = success,
		Outcome = refined,
		Asserted = [none],
		Retracted = [none],
		lucas(Goal)
	    ;
		DiagnosisOutcome = failure,
		Outcome = failure
	    )
	
	;
	    process_plan(T,[[H,AssertedHere]|Asserted],[[H,RetractedHere]|Retracted],Goal)
	).



% an action is performed by placing a query for the request to be performed.  A summary of what happened is returned, which is either success or failure.



perform_action(Action,Outcome,Asserted,Retracted,AssertedSoFar,Goal) :-
	justification(Just),
	locateRule(Action,Just,Preconds,Postconds),
	query(request,Action,Answer,CorrectAgent,QueryList,Preconds,SurprisingQuestions),
	summary(Action,Answer,ExpectedOutcome),!,
	(   Answer = ok ->
	    % if the request succeeds, update ontology accordingly
	    ( updateOnt(CorrectAgent,Action,Preconds,Postconds,Asserted,Retracted) ->
		Outcome = ExpectedOutcome
	    ;
		justification(Just),
		write('I could not update the ontology appropriately'),nl,
%		diagnose_failure_retract(Action,CorrectAgent,Just),
		% this happens if the agent cannot update the actions, which happens because it cannot retract a fact
		Outcome = plan_failure
	    )
	;
	    % else update the ontology and then find out why it failed
	    updateKIFOnt(AssertedSoFar),
	    justification(Just),
	    Outcome = failure
	).

	


% for every step, a line is written saying it has been completed.  If it fails, this is highlighted.

summary(Action,ok,ok) :-
	write(Action),write(' completed satisfactorily'),nl,nl.
summary(Action,_,plan_failure) :-
	write('plan has failed at '),write(Action),nl,nl.



% querys are things asked of other agents.  Options are: request an action to be performed; request the truth value of a statement; request an instantiation of variables for a statement; request the type of an object.

% these querys are common to all agents.  The last three are the same for any agent; the request query will depend on what tasks the agents can perform.



query(request,Task,Outcome,CorrectAgent,QueryList,ExpectedQuestions,SurprisingQuestions) :-
	chooseAgent(CorrectAgent,Task),!,
	(   CorrectAgent = noAgent,
	    Outcome = ok
	% sometimes performing tasks requires no interaction; in this case they always succeed.
	;
	    write('im going to ask '),write(CorrectAgent),write(' to perform '),write(Task),write(' for me'),nl,
	    out(query(lucas,CorrectAgent,request,Task)),
	   % sometimes tasks must be performed by other agents; in this case they need to communicate with the appropriate agent.
	    % waitForAnswer has three arguments: who he should talk to, what the outcome of that is, and what the most recent question has been, in case of failure.

%	    rule(Task,[_,ExpectedQuestions,_]),
%	    waitForAnswer(CorrectAgent,Outcome,ExpectedQuestions,[request,[]],[QueryList,SurprisingQuestions])
	    waitForAnswer(CorrectAgent,Outcome,ExpectedQuestions,[request],[],QueryList,SurprisingQuestions)
	).





% 2) query the truth of a statement:
%     B can take the values true or false


query(truth,A,B) :-
	write('i want to know the truth'),
	out(query(truth,A)),
	in(reply(B)).


% 3) request for the variables of a statement to be instantiated:
%     B can take the value fail; otherwise it will be the same as a but with all variables instantiated.

query(var,A,B) :-
	out(query(var,fact(A))),
	in(reply(B)).


% 4) query the type/class of an individaul:
%     not sure how this is going to work yet - more on this later.

query(type,A,B) :-
	out(query(type,A)),
	in(reply(B)).



	



% once we have asked for a request to be performed, we have to wait to receive information about whether this is going to happen.  this involves checking for new messages until we receive the answer.



waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,CurrentQuery,CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions) :-
	( in_noblock(reply(SolvingAgent,lucas,Outcome)) ->
	    
	    %when you have a definate answer, latest is set to current.
	    QueryList = CurrentQuery,
	    LatestSurprisingQuestions = CurrentSurprisingQuestions
	;
	    ( 
		(
		    in_noblock(query(SolvingAgent,lucas,Type,Ques))
		;
		    sleep(2),
		    in_noblock(query(SolvingAgent,lucas,Type,Ques))
		),
	      %else if there is some kind of query there

		solve(Type,Ques,Answer,SolvingAgent),
		
		out(reply(lucas,SolvingAgent,Type,Answer)),
				%respond to that query
		
		( member(Ques,ExpectedQuestions) ->
	    
		    waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,[Ques|CurrentQuery],CurrentSurprisingQuestions,QueryList,LatestSurprisingQuestions)
		;   
		    waitForAnswer(SolvingAgent,Outcome,ExpectedQuestions,[Ques|CurrentQuery],[Ques|CurrentSurprisingQuestions],QueryList,LatestSurprisingQuestions)
		)
	
	% and repeat
	% if there is no question there then fail - the other agent should either reply to our request or else ask for more info.
	    ;	
		write(SolvingAgent),write(' wont bloody talk to me '),nl,
		fail
	    )
	).





      
% this needs to perform the action as normal
solve(request,_,failed,SolvingAgent) :-
	Message = 'sorry, i dont perform actions',
	out(reply(qAgent,SolvingAgent,Message,_)).


solve(question,Question,Answer,_) :-
	nonFacts(NonFactList),
	Question =.. [QuestionName|_],
	( member(QuestionName,NonFactList) ->
	    (	Question,
		Answer = Question
	    ;
		Answer = no
	    )
	;
	    ( fact(Question) ->
		Answer = Question
	    ;	
		Answer = no
	    )
	).


here.

% this instantiates variables:
%solve(var,Ques,Answer,_) :-
%	( fact(Ques) ->
%	    Answer = Ques
%	;   
%	    write('i cant answer this question properly'),nl,
%	    write('Note: this is a problem that we havent fixed yet.'),
%	    fail
	% NOTE: this is not an adequate solution to this problem.  this will have to be dealt with in a more appropriate manner.
%	).



% this needs to check statements
%solve(truth,Query,true,_) :-
%	solve(truth,Query).

%solve(truth,true) :- !.
%solve(truth,((A,B))) :- !,
%	solve(truth,A),
%	solve(truth,B).
%solve(truth,A) :-
%	clause(A,B),solve(truth,B).
	
% this needs to check classes
%solve(type,_,ha,_).



updateOnt(Agent,_,_,Postconds,Asserted,Retracted) :-

	% first find the preconds and postconds
%	rule(Task,[_,Preconds,Postconds]),
	
	% next, instantiate any variables in the preconds
%	instantiatePreconds(Preconds),!,

	% call the rule again to pass this instantiation information to the postconds
%	rule(Task,[_,Preconds,Postconds]),
	
	% update the postconds
	processPostconds(Agent,Postconds,[],[],Asserted,Retracted).



locateRule(_,[],_,_) :-
	write('this rule does not appear to be executable'),nl,
	fail.

locateRule(Action,[[ActionName,_,PrecondsList,PostcondsList,_,_]|RestJust],Preconds,Postconds) :-
	( Action = ActionName ->
	    Preconds = PrecondsList,
	    Postconds = PostcondsList
	;
	    locateRule(Action,RestJust,Preconds,Postconds)
	).

	

	


processPostconds(_,[],Asserted,Retracted,Asserted,Retracted).

processPostconds(Agent,[H|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
	( H = inform(Condition) ->
	    askForInfo(Agent,Condition),!,
	    processPostconds(Agent,T,[Condition|AssertedSoFar],RetractedSoFar,Asserted,Retracted)
	;   
	    ( H = not(Condition) ->
		( fact(Condition) ->
		    retract(fact(Condition)),
		    processPostconds(Agent,T,AssertedSoFar,[H|RetractedSoFar],Asserted,Retracted)
		;   
		    write('The is a problem with the postconditions: I am trying to retract a fact that doesnt exist'),nl,
		    fail
		)
	    ;	
		( H = calculation(Calc) ->
		    Calc,
		    processPostconds(Agent,T,AssertedSoFar,RetractedSoFar,Asserted,Retracted)
		;   
		    (	H = class(_,_),
			assert(H),
			processPostconds(Agent,T,[H|AssertedSoFar],RetractedSoFar,Asserted,Retracted)
		    ;	
			assert(fact(H)),
			processPostconds(Agent,T,[H|AssertedSoFar],RetractedSoFar,Asserted,Retracted)
		    )
		)
	    )
	).



instantiatePreconds([]).

instantiatePreconds([H|T]) :-
	(   fact(H),!
	;
	    (	H = calculation(Sum),
		Sum
		;
		(   H = class(Thing,Class),
		    checkClass(Thing,Class)
		;
		    H
		)
	    )
	),
	instantiatePreconds(T).



instantiatePreconds([]).

instantiatePreconds([H|T]) :-
	(   fact(H)
	;
	    (	H = calculation(Sum),
		Sum
		;
		H
	    )
	),
	instantiatePreconds(T).


askForInfo(Agent,Postcond) :-
	out(query(lucas,Agent,question,Postcond)),
	( in_noblock(reply(Agent,lucas,Postcond)) ->
	    assert(fact(Postcond))
	;
	    write(Agent),write(' does not have the information he should have about the postcond '),write(Postcond),nl,
	    fail
	).



%chooseAgent(Agent,Task) :-
%	Task =.. [ActionName|_],
%	agentNeeded(Agent,ActionName).

chooseAgent(Agent,Task) :-
	write_to_chars(Task,TaskChar),
	name('(',RightBracket),
	matchExpression(ActionName,RightBracket,_,TaskChar),
	agentNeeded(Agent,Action),
	write_to_chars(Action,ActionName).

%chooseagent(Agent,Task) :-
%	write_to_chars(Task,Taskchar),
%	name('(',Rightbracket),
%	matchexpression(Actionname,Rightbracket,_,Taskchar),
%	name('Action',Extra),
%	append(Actionname,Extra,Fullactionname),
%	agentneeded(Agent,Action),
%	write_to_chars(Action,Fullactionname).
	



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










%lucas(Goal) :-
%	reconsult(translation),
%	translateGoal(Goal),
%	callPlanner(??),
%	read(Plan),
%	(   deconstruct(Plan,Just),!,
%	    assert(justification(Just))
%	;
%	    write('problem with the deconstruction'),nl, fail
%	),
%	process_plan(Plan).

%lucas :-
%	reconsult('planning/planfinder'),
%	reconsult('translation/translation'),
%	reconsult('translation/translateGoal'),
%	listen.

%listen :-
%	write('What is the goal?  '),read(Goal),nl,nl,
%	translateGoal(Goal,TransGoal),
%	translateToPDDL(TransGoal),
%	findPlan(Plan),
%	write(Plan),
%	listen.

	
