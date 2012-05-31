:- use_module(library(system)),use_module(library(lists)),  
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)),
   use_module(library(codesio)).


:- dynamic justification/1.



start :-
    scenario_name(Sce),
    set_prolog_flag(single_var_warnings, off),
    set_prolog_flag(discontiguous_warnings, off),
    get_absolute_path('/agent_environment/PA/planningAgent', PAAbsolutePath),
    reconsult(PAAbsolutePath),
    ontoType(OntologyType),
    experience_sharing(OnOrOff),
    plan(OntologyType,Sce,OnOrOff).


% get_absolute_path(+RelativePath, ?AbsolutePath)
% e.g. get_absolute_path('/agent_environment', AbsolutePath).

get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).




% get_current_scenario_path(?CurrentScenarioPath)
% gives something like '/afs/inf.ed.ac.uk/user/s09/s0958589/ontologyrefinement/new-ors/agent_environment/scenarios/sem_matching'
get_current_scenario_path(CurrentScenarioPath) :-
        scenario_name(Sc),  % asserted earlier; also loaded from the file config.pl
	get_absolute_path('/agent_environment/scenarios/',ScenariosPath),
        atom_concat(ScenariosPath, Sc, CurrentScenarioPath).




% this is the code for the planning agent, who is able to form plans, execute them through communication with other agents, and diagnose and repair mismatches when necessary, which leads on to replanning until either the goal is achieved, it becomes impossible to form a plan, or repair cannot be done.


% plan/0 loads the necessary files and finds the goal.

plan(OntType,Scenario,OnOrOff) :-
        get_current_scenario_path(CurrentScenarioPath),
        atom_concat(CurrentScenarioPath, '/PA-onts/original/ont.in', OntInPath),
        atom_concat(CurrentScenarioPath, '/PA-onts/updated/', UpdatedPath),
        atom_concat('cp ', OntInPath, CommandPart1),
        atom_concat(CommandPart1, ' ', CommandPart2),
        atom_concat(CommandPart2, UpdatedPath, FinalCommand),
	process_create('/bin/sh', ['-c', FinalCommand]),
	copyOntFiles(OntType,Scenario),
	write('_________________________________________________'),nl,
	write('Consulting the PLANNER...'),nl,
	get_absolute_path('/agent_environment/PA/planning/planner', PlannerPath),
	consult(PlannerPath),
	write('Consulting the ONTOLOGY UPDATER...'),nl,
	get_absolute_path('/agent_environment/PA/update/onto_update', UpdaterPath),
	consult(UpdaterPath),
	write('Consulting ORS...'),nl,
	get_absolute_path('/ORS/ors',ORSPath),
	consult(ORSPath),nl,
        get_current_scenario_path(CurrentScenarioPath),
	goal(Goal), % unifies with term asserted after loading the scenario-based Planning Agent
	ors(pa),
	plan(OntType,Scenario,OnOrOff,Goal,[start],[]).


% The sleep predicate is not strictly speaking necessary here but since
% the Prolog and the Python interpreter work independently (i.e. Prolog
% doesn't wait for Python to finish), it is wise to wait long enough
% for Python to create the centralSig.pl, centralThy.pl and metaOnt.pl 
% files. 1.5 seconds is more than enough, so it is unlikely that Prolog
% will attempt to read the files before they are created.

translate(sumo,_Scenario) :-
        scenario_name(ScN),
        atom_concat('python $ORS_HOME/ORS/translation/SUMO_to_Prolog/suo-kif_to_prolog.py --ont ont.in --sce ', ScN, TranslationCommand),
	process_create('/bin/sh', ['-c', TranslationCommand]),
        sleep(2).

translate(onto,Scenario) :-
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translation', Ontol_transAbsolutePath),
	consult(Ontol_transAbsolutePath),nl,nl,
	find_ontology_path(Scenario,ScenarioPath),
	translation(Scenario,ScenarioPath).


% plan(+Goal,+ActionsSoFar,+RepairsSoFar)
% performs the translation process and finds the plan.  repairsSoFar so far will be an empty list if this is the first time of planning, but if replanning is occurring, it will not be empty.


plan(OntType,Scenario,OnOrOff,Goal,ActionsSoFar,RefinementsSoFar) :-
	preprocess_semantics(OntType),
	nl,nl,write(' __________'),nl,
	write('|          |'),nl,write('| Goal is: |   '),
	write(Goal),nl,write('|__________|'),
	nl,nl,write('Translating ...'),nl,nl,
        translate(OntType,Scenario),
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/centralSig',CentralSigPath),
	atom_concat(ScenarioPath,'/centralThy',CentralThyPath),
	atom_concat(ScenarioPath,'/metaOnt',MetaOntPath),
	consult(CentralSigPath),
	consult(CentralThyPath),
	consult(MetaOntPath),
	nl,nl,write('Need to find a plan ...'),nl,nl,
	findPlan([Goal],Plan),
	plan(OntType,Scenario,OnOrOff,Goal,Plan,ActionsSoFar,RefinementsSoFar).


% plan(+Goal,+Plan,+ActionsSoFar,+RepairsSoFar)
% reads the newly translated ontology and begins to process the plan.

plan(_OntType,_Scenario,_OnOrOff,_Goal,fail,ActionsSoFar,RepairsSoFar) :-
    write('  all is lost; the plan cannot be completed'),nl,nl,
    write('Actions performed so far: '),nl,
    write(ActionsSoFar),nl,
    write('repairs performed so far: '),nl,
    write(RepairsSoFar).

plan(OntType,Scenario,OnOrOff,Goal,Plan,ActionsSoFar,RepairsSoFar) :-
    write('This is the plan:'),nl,
    write(Plan),nl,nl,nl,
    processPlan(OntType,Scenario,OnOrOff,Plan,Goal,ActionsSoFar,RepairsSoFar).


preprocess_semantics(onto).

preprocess_semantics(sumo) :-
	nl,nl,write('Pre-computing NESTED LISTS, BAGS OF WORDS & TF-IDF...'),nl,
        scenario_name(ScN), 
        atom_concat('python $ORS_HOME/ORS/sem-matching/compute_nested_lists.py --sce ', ScN, HalfCommand),
        atom_concat(HalfCommand, ' --ont ont.in', FullCommand),
	process_create('/bin/sh', ['-c', FullCommand]),
	sleep(5),
	process_create('/bin/sh', ['-c', 'python $ORS_HOME/ORS/sem-matching/tf_idf.py --ont ont.in --sce sem_matching']).
         % This has been asserted in scenario_based_pa.pl	



% processPlan(+Plan,+Goal,+ActionsSoFar,+RepairsSoFar)
% builds up the necessary lists to begin plan execution.

processPlan(OntType,Scenario,OnOrOff,Plan,Goal,ActionsSoFar,RepairsSoFar) :-
	nl,write('Executing the plan ...'),nl,nl,nl,
	processPlan(OntType,Scenario,OnOrOff,Plan,[[start]],Goal,ActionsSoFar,RepairsSoFar).


% processPlan(+RemainingPlan,-AssertedFacts,+Goal,+ActionList,+RepairList)
% executes the first plan step, unless the plan is finished, and recurses.  If the plan is finished, updates the KIF ontology according to what has been asserted and retracted during plan execution, and prints a list of what repairs have been performed

processPlan(OntType,Scenario,_OnOrOff,[],Asserted,_,ActionList,RepairList) :-
	updateOnt(OntType,Scenario,Asserted,ActionList),
        %process_create('/bin/sh', ['-c', 'rm $ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/updated/bags_tfidf.py']),
        %process_create('/bin/sh', ['-c', 'rm $ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/updated/bags_tfidf.pyc']),
        %process_create('/bin/sh', ['-c', 'rm $ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/updated/nested_lists.py']),
        %process_create('/bin/sh', ['-c', 'rm $ORS_HOME/agent_environment/scenarios/sem_matching/PA-onts/updated/nested_lists.pyc']),
	write('The plan is completed'),nl,nl,
	write('The following actions have been performed: '),nl,
	write(ActionList),nl,nl,
	write('The following repairs have been performed: '),nl,
	write(RepairList),nl,nl,
        write('To terminate the program, type "t."').



processPlan(OntType,Scenario,_OnOrOff,failure,Asserted,_,ActionList,RepairList) :-
	updateOnt(OntType,Scenario,Asserted,ActionList),
	nl,nl,write('Attempting to execute the plan resulted in failure'),nl,nl,
	write('The following actions have already been performed: '),
	write(ActionList),nl,nl,
	write('The following repairs have already been performed: '),
	write(RepairList),nl,nl.
 
processPlan(OntType,Scenario,OnOrOff,[Action|LaterActions],Asserted,Goal,ActionList,RepairList) :- 
        performAction(Action,ActionSPA,Outcome),!,
	postActionProcess(OntType,Scenario,OnOrOff,Action,LaterActions,Asserted,Goal,ActionList,RepairList,ActionSPA,[Action|LaterActions],Outcome).

t :- process_create('/bin/sh', ['-c', 'killall xterm']).


% performAction(+Action,-Outcome,-ActionSPA)
% finds the correct agent, sends a request to perform the task and waits for the response

performAction(Task,noAgent,ok) :-
	chooseAgent(noAgent,Task),!.

performAction(Task,ActionSPA,Outcome) :-
	chooseAgent(ActionSPA,Task),!,
	write('I\'m going to ask '),write(ActionSPA),write(' to perform '),write(Task),write(' for me'),nl,
	proxyOut(query(pa,ActionSPA,request,Task)),
	waitForAnswer(ActionSPA,Outcome).


% postActionProcess(+Action,+Asserted,+Goal,+ActionList,+RepairList,+ActionSPA,+Outcome)
% if Action was perfomed successfully, updates the actions and continues with the plan.  If not, it asks ORS for a diagnosis, agrees to the repair (this is fixed at the moment) and replans once ORS has implemented the repair.

postActionProcess(OntType,Scenario,OnOrOff,Action,LaterActions,Asserted,Goal,ActionList,RepairList,ActionSPA,_Plan,ok) :-
	write(Action),write(' performed successfully'),nl,
	processPostconds(Action,ActionSPA,AssertedHere),
	processPlan(OntType,Scenario,OnOrOff,LaterActions,[[Action,AssertedHere]|Asserted],Goal,[Action|ActionList],RepairList).


% PROTECTED

postActionProcess(OntType,Scenario,OnOrOff,Action,_LaterActions,Asserted,Goal,ActionList,RepairList,ActionSPA,Plan,problem) :-
	updateOnt(OntType,Scenario,Asserted,ActionList),
	write(Action),write(' failed.  I am asking ORS for a diagnosis.'),nl,nl,
	findDiagnosis(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList,ActionSPA,Plan).


findDiagnosis(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList,_ActionSPA,Plan) :-
	requestDiagnosis(Plan,ProposedRepair,ok,OntType,Scenario,OnOrOff),
	write('ORS proposed the following diagnosis repair: '),write(ProposedRepair),nl,
	plan(OntType,Scenario,OnOrOff,Goal,ActionList,[ProposedRepair|RepairList]).


findDiagnosis(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList,ActionSPA,Plan) :-
	requestDiagnosis(Plan,ProposedRepair,[protected,ok,SPARepair],OntType,Scenario,OnOrOff),
	write('ORS proposed the following diagnosis repair: '),write(ProposedRepair),nl,write(' but this part of my ontology is protected, so '),write(ActionSPA),write(' has implemented the inverse of this: '),write(SPARepair),nl,
	plan(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList).

findDiagnosis(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList,ActionSPA,Plan) :-
	requestDiagnosis(Plan,ProposedRepair,[protected,no,_PARepair],OntType,Scenario,OnOrOff),
	write('ORS proposed the following diagnosis repair: '),write(ProposedRepair),nl,write(' but this part of my ontology is protected.  '),write(ActionSPA), write(' has refused to implement the inverse.'),nl,
	plan(OntType,Scenario,OnOrOff,Goal,ActionList,RepairList).


confirmRepair(success) :-
	write('I agreed to this being performed.'),nl,
	write('This repair has now been implemented.  I will begin planning again.').
	 
confirmRepair(_).

% postActionProcess(OntType,Scenario,OnOrOff,Action,_LaterActions,Asserted,Goal,ActionList,RepairList,_ActionSPA,Plan,problem) :-
%	updateOnt(OntType,Scenario,Asserted,ActionList),
%	write(Action),write(' failed.  I am asking ORS for a diagnosis.'),nl,nl,
%	requestDiagnosis(Plan,ProposedRepair,OntType,Scenario,OnOrOff),
%	write('ORS proposed the following diagnosis repair: '),write(ProposedRepair),nl,
%	confirmDiagnosis(ok),
%	write('I agreed to this being performed.'),nl,
%	confirmRepair(ok),
%	write('This repair has now been implemented.  I will begin planning again.'),
%	plan(OntType,Scenario,OnOrOff,Goal,ActionList,[ProposedRepair|RepairList]).


%confirmDiagnosis(ok).
%confirmRepair(ok).

% ~PROTECTED


% waitForAnswer(+ActionSPA,-Outcome)
% listens for a reply, which may be either an outcome to the action or a request for more information.  if a query is received, this is checked against expected queries to see if it is surprising.  if no reply is forthcoming, sleeps for 2 seconds and tries again.  if there is still no answer, a failure notice is passed returned.

waitForAnswer(ActionSPA,Outcome) :-
	proxyIn(reply(ActionSPA,pa,_Action,Outcome)).

waitForAnswer(ActionSPA,Outcome) :-
	proxyIn(query(ActionSPA,pa,Type,Ques)),
	write(ActionSPA),write(' asked me '),write(Ques),nl,
	solve(Type,Ques,Answer,ActionSPA),
	write('I told '),write(ActionSPA),write(' '),write(Answer),nl,
	proxyOut(reply(pa,ActionSPA,Type,Answer)),
	waitForAnswer(ActionSPA,Outcome).

waitForAnswer(ActionSPA,Outcome) :-
	sleep(1),
	proxyIn(reply(ActionSPA,pa,_Action,Outcome)).

waitForAnswer(ActionSPA,Outcome) :-
	sleep(1),
	proxyIn(query(ActionSPA,pa,Type,Ques)),
	solve(Type,Ques,Answer,ActionSPA),
	proxyOut(reply(pa,ActionSPA,Type,Answer)),
	waitForAnswer(ActionSPA,Outcome).

waitForAnswer(ActionSPA,_Outcome) :-
	write(ActionSPA),write(' wont bloody talk to me '),nl.



% solve(+type,+Question,-Answer,+ActionSPA)
% attempts to respond to a question, which may be a request to perform an action (which planning agent cannot do) or to answer a question.
      
solve(request,_,failed,ActionSPA) :-
	Message = 'Sorry, I don\'t perform actions!',
	proxyOut(reply(qAgent,ActionSPA,Message,_)).

solve(question,class(Class,Object),class(Class,Object),_) :-
	checkClass(Class,Object).

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




processPostconds(Action,SPA,Asserted) :-
	rule(Action,[_RuleNo,Preconds,Postconds]),
	instantiatePreconds(Preconds),
	processPostconds(SPA,Postconds,[],Asserted).

% processPostconds(+Agent,+Postconditions,+AssertedSoFar,+Asserted)
% updates the temporary Prolog ontology which the agent is using by processing the postconditions of the action.

processPostconds(_,[],Asserted,Asserted).

processPostconds(Agent,[inform(Condition)|T],AssertedSoFar,Asserted) :-
	askForInfo(Agent,Condition),!,
	processPostconds(Agent,T,[Condition|AssertedSoFar],Asserted).


% WARNING!  ignoring negation in the update as a temporary hack!  Needs to be fixed.

processPostconds(Agent,[not(Condition)|T],AssertedSoFar,Asserted) :-
	retract(fact(Condition)),
	processPostconds(Agent,T,AssertedSoFar,Asserted).


% processPostconds(Agent,[not(Condition)|T],AssertedSoFar,RetractedSoFar,Asserted,Retracted) :-
% 	retract(fact(Condition)),
% 	processPostconds(Agent,T,AssertedSoFar,[not(Condition)|RetractedSoFar],Asserted,Retracted).

processPostconds(Agent,[calculation(Calc)|T],AssertedSoFar,Asserted) :-
	Calc,
	processPostconds(Agent,T,AssertedSoFar,Asserted).



processPostconds(Agent,[class(Object,Class)|T],AssertedSoFar,Asserted) :-
	assert(class(Object,Class)),
	processPostconds(Agent,T,[class(Object,Class)|AssertedSoFar],Asserted).
	
processPostconds(Agent,[H|T],AssertedSoFar,Asserted) :-
	assert(fact(H)),
	processPostconds(Agent,T,[H|AssertedSoFar],Asserted).


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
	proxyOut(query(pa,Agent,question,Postcond)),
	proxyIn(reply(Agent,pa,question,Postcond)),
	assert(fact(Postcond)).


% chooseAgent(-Agent,+Task)
% returns the appropriate agent to perform task

chooseAgent(Agent,Task) :-
	write_to_codes(Task,TaskChar),
	name('(',RightBracket),
	matchExpression(ActionName,RightBracket,_,TaskChar),
	agentNeeded(Agent,Action),
	write_to_codes(Action,ActionName).


updateOnt(onto,Scenario,Asserted,ActionList) :-
	updateKIFOnt(Scenario,Asserted,ActionList).

updateOnt(sumo,_Scenario,Asserted,_ActionList) :-
        generateAsserted(Asserted,ProcessedAsserted),
	generateCommand(ProcessedAsserted,UpdateCommand),
	process_create('/bin/sh', ['-c', UpdateCommand]).


generateAsserted(Asserted,ProcessedAsserted) :-
	generateAsserted(Asserted,[],AssertedList),
	createProcessedAsserted(AssertedList,ProcessedAsserted).

generateAsserted([[start]],ProcessedAsserted,ProcessedAsserted).

generateAsserted([[_Action|[Facts]]|Rest],ProcessedSoFar,ProcessedAsserted) :-
	append(Facts,ProcessedSoFar,NewProcessed),
	generateAsserted(Rest,NewProcessed,ProcessedAsserted).

createProcessedAsserted([],ProcessedAsserted) :-
	atom_concat('"','[',Proc1),
	atom_concat(Proc1,']',Proc2),
	atom_concat(Proc2,'"',ProcessedAsserted).

createProcessedAsserted(Asserted,ProcessedAsserted) :-
	createProcessedAsserted(Asserted,'',ProcessedAsserted).

createProcessedAsserted([LastAsserted],ProcessedSoFar,ProcessedAsserted) :-
	LastAsserted =.. LastList,
	processPred(LastList,LastPred),
	atom_concat('"[',LastPred,Proc1),
	atom_concat(Proc1,ProcessedSoFar,Proc2),
	atom_concat(Proc2,']"',ProcessedAsserted).

createProcessedAsserted([FirstAsserted|Rest],ProcessedSoFar,ProcessedAsserted) :-
	FirstAsserted =.. FirstList,
	processPred(FirstList,FirstPred),
	atom_concat(',',FirstPred,Start),
	atom_concat(Start,ProcessedSoFar,NewProcessed),
	createProcessedAsserted(Rest,NewProcessed,ProcessedAsserted).

processPred([Pred|Args],FirstPred) :-
	atom_concat(Pred,'(',Pred1),
	processArgs(Pred1,Args,FirstPred).

processArgs(Pred,[LastArg],FirstPred) :-
	atom_concat(Pred,LastArg,Pred1),
	atom_concat(Pred1,')',FirstPred).

processArgs(Pred,[FirstArg|Rest],FirstPred) :-
	atom_concat(Pred,FirstArg,Pred1),
	atom_concat(Pred1,',',Pred2),
	processArgs(Pred2,Rest,FirstPred).


generateCommand(Facts,Command) :-
	atom_concat('python $ORS_HOME/agent_environment/PA/update/sumo_update.py --facts ',Facts,Command1),
	atom_concat(Command1,' --ont ont.in',Command).





find_ontology_path(Scenario,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/PA-onts/updated',ScenarioPath).


find_original_ontology_path(Scenario,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/PA-onts/original',ScenarioPath).

%copyOntFiles makes sure that the original version of the ontology files are the ones in use.  This happens in some other way for SUMO onts: must find out about that.


copyOntFiles(sumo,_Scenario).

copyOntFiles(onto,Scenario) :-
	find_original_ontology_path(Scenario,OrigScenarioPath),
	find_ontology_path(Scenario,ScenarioPath),
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


% this is to connect the pa to the server:



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client

initialiseClient(PId):-
%	sleep(5),
	get_absolute_path('/agent_environment/server.addr', ServerAddressAbsPath),
	find_host_port(ServerAddressAbsPath,Host,Port,PId),
	linda_client(Host:Port).


find_host_port(ServerAddressAbsPath,Host,Port,PId) :-
	file_exists(ServerAddressAbsPath),  
	see(ServerAddressAbsPath),
	read(Host:Port-PId),
	seen.

find_host_port(ServerAddressAbsPath,Host,Port,PId) :-
	sleep(1),
	find_host_port(ServerAddressAbsPath,Host,Port,PId).






%%%%%%%%%%%%%%%%%%%%%% START CLIENT


:- initialiseClient(_).
