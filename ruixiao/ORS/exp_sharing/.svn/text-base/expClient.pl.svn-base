% experience client manages experience locally and determines how they are used
% the request has the form [SPA, Action, SurprisingQuestions, Questions]
% the response has the form [OldOnt, NewOnt, Repair, Result]

:- use_module(library(lists)),use_module(library(random)),
	use_module(library(system)).

%%% set up temp database to stored save exps, avoids infinite loop %%%%%
%:- get_absoulte_path('/ORS/exp_sharing/savedExps.pl',SavedExps),tell(SavedExps),nl,nl,write('savedExps(agent,action,exps).'),nl,nl,told.

%%%%%% Experience Client %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% requestExp(+SPA,+Action,+SurpriseQuestions,+QueryList,-ReturnedExp) send a request to the server for an experience


requestExps(SPA,Action,SurpriseQuestions,QueryList,ReturnedExps) :-
	RequestInfo = [SPA,Action,SurpriseQuestions,QueryList],
	out(request(pa,expServerAgent,retrieve,RequestInfo)),
	waitForExp(ReturnedExps,5).

% waitForExp(-Response,+Countdown) sleep until experience server replies or timeout

waitForExp(Response,Countdown) :-
	in_noblock(reply(expServerAgent,pa,Response)),!.

waitForExp(Response,Countdown) :-
	Countdown =< 0,
	write('No reply from Experience Server, skip to Diagnosis.'),nl,
	Response = [],!.
	
waitForExp(Response,Countdown) :-
	sleep(1),
	waitForExp(Response,Countdown - 1).

% shareExps(+RefinementList) send all the experiences to server

shareExps(off,_Info).

%note: this is a temp fix to keep ors running.  next i have to alter the code so that this actually does something!

shareExps(on,[]) :-
	write('Im done Sharing Experiences'),nl,nl.

shareExps(on,[SPA,Action,SuprisingQuestions,QueryList,[OldOnt,NewOnt,Repair]]) :-
	write('Im Sharing a refinement Experience regarding agent: '),write(SPA),nl,
	write('executing action: '),write(Action),nl,nl,
	RequestInfo = [SPA,Action,SuprisingQuestions,QueryList,OldOnt,NewOnt,Repair,success],
	out(request(pa,expServerAgent,store,RequestInfo)).

shareExps(on,Experience) :-
	write('I could not share experience: '),nl,write(Experience),nl,nl.
%	shareExps(Rest).


%%%%% Experience Consumer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% examineAction(+Action,+Just,+CorrectAgent,+QueryList,+SurprisingQuestions,+RefinementList,-NewRefinementList)
% examines the failed action and determine if to perform diagnosis or consume experiences


examineAction(Action,Just,CorrectAgent,QueryList,SurprisingQuestions,Experiences,on) :-

	hasSavedExps(CorrectAgent,Action,Experiences).

examineAction(Action,Just,CorrectAgent,QueryList,SurprisingQuestions,Experiences,on) :-
	requestExps(CorrectAgent,Action,SurprisingQuestions,QueryList,Experiences).


examineAction(_Action,_Just,_CorrectAgent,_QueryList,_SurprisingQuestions,none,on) :-
	write('I didnt consume any experience'),nl.


examineAction(_Action,_Just,_CorrectAgent,_QueryList,_SurprisingQuestions,none,off).

%%% start local exp storage %%%%%
% hasSavedExps(+Agent,+Action,-Experiences) checks for already retrieved actions, avoid infinit loop

hasSavedExps(Agent,Action,Experiences) :-
	get_absolute_path('/ORS/exp_sharing/savedExps.pl',SavedExps),
	reconsult(SavedExps),
	savedExps(Agent,Action,Experiences),!.


% saveExps(+Agent,+Action,+Experiences) saves retrieved exps, avoid infinit loop

saveExps(Agent,Action,Experiences) :-
	get_absolute_path('/agent_environment/PA/experiences/savedExps.pl',SavedExps),
	open(SavedExps,append,DbStream),
	nl(DbStream),write(DbStream,'savedExps('),
	write(DbStream,Agent),write(DbStream,','),
	write(DbStream,Action),write(DbStream,','),
	write(DbStream,Experiences),
	write(DbStream,').'),nl(DbStream),
	flush_output(DbStream),
	close(DbStream).
%%% end local exp storage %%%%%
	
% constructRefinementVariable(+Agent,+Action,+SQ,+QL,+Refinement,-NewRefinement)
% if the repair is an experience supported type, then construct valid format
% otherewise, simply track what repair was done

constructRefinementVariable(CorrectAgent,Action,SurprisingQuestions,QueryList,[ExPrecond,RSQ,Repair],[CorrectAgent,Action,SurprisingQuestions,QueryList,ExPrecond,RSQ,Repair]) :-
	!.

constructRefinementVariable(_,_,_,_,Refinement,[Refinement]).

% consumeExperiences(+Agent,+Action,+Experiences,-RefinementList)
% the base predicate called by PA to consume the retrieved experiences
% it sits as the layer between PA and diagnosis component

consumeExperiences(_,_,none,_,_,_) :-
	fail,!.

consumeExperiences(_,_,[],[],_,_).

consumeExperiences(Agent,Action,[[OldOnt,NewOnt,[Type,Info],success]|RestExps],[Type|TempRefinement],OntType,Scenario) :-
	write('trying to consume experience'),nl,
	\+ predicateExists(NewOnt,Action,Scenario),
	consumeRefinement(Agent,Action,OldOnt,NewOnt,Type,Info,OntType,Scenario,Outcome).

consumeExperiences(Agent,Action,[_FirstExp|RestExps],[Type|TempRefinement],OntType,Scenario) :-
	consumeExperiences(Agent,Action,RestExps,TempRefinement,OntType,Scenario),!.

% consumeExperiences(Agent,Action,[_|RestExps],RefinementList) :-
% 	write('failed to consume experience'),nl,
% 	consumeExperiences(Agent,Action,RestExps,RefinementList).

% predicatExists(+Ont,+Action) checks if Ont already exist in local ont, fails if not

predicateExists(none,_,_) :- !.

predicateExists(meta,_,_) :- fail.

predicateExists(shapiro,_,_) :- fail.

predicateExists(Predicate,Action,Scenario) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/centralSig',CentralSigPath),
	atom_concat(ScenarioPath,'/centralThy',CentralThyPath),
	consult(CentralSigPath),
	consult(CentralThyPath),
	rule(Action,[_,Precond,_]),
	checkPredicateExists(Predicate,Precond,true).

classExists(Class,Action,Scenario) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/centralSig',CentralSigPath),
	atom_concat(ScenarioPath,'/centralThy',CentralThyPath),
	consult(CentralSigPath),
	consult(CentralThyPath),
	rule(Action,[_,Precond,_]),
	checkPredicateExists(class(_,Class),Precond,true).

checkPredicateExists(_,[],false).

checkPredicateExists(Predicate,[Predicate|_],true) :- !.

checkPredicateExists(Predicate,[_|Rest],Outcome) :-
	checkPredicateExists(Predicate,Rest,Outcome).

% consumeRefinement(+Agent,+Action,+OldOnt,+NewOnt,+Type,+Info,-Outcome) attempts different refinements

consumeRefinement(Agent,_,Precond,meta,_,_,_OntType,Scenario,Outcome) :-
	predicateExists(Precond,_,Scenario),
	translatePred(Agent,TransAgent),
	attemptRefineMeta(Scenario,agent,TransAgent,Outcome),!.

consumeRefinement(Agent,Action,ProblemPrecond,shapiro,_,Just,OntType,Scenario,Outcome) :-
	predicateExists(ProblemPrecond,Action,Scenario),
	shapiroCheck(Scenario,pa,Agent,ProblemPrecond,OntType,Just,Outcome),!.

consumeRefinement(_,Action,Precond,_,propositionalA,[NameRSQ,Position],OntType,Scenario,Outcome) :-
	predicateExists(Precond,Action,Scenario),
	translatePred(NameRSQ,TransName),
	attemptRefine(Scenario,propositionalA,OntType,[TransName,Position],Outcome),!.

consumeRefinement(_,Action,Precond,_,switchArgs,PredName,OntType,Scenario,Outcome) :-
	predicateExists(Precond,Action,Scenario),
	translatePred(PredName,TransPred),
	attemptRefine(Scenario,switchArgs,OntType,TransPred,Outcome),!.

consumeRefinement(_,Action,Precond,RSQ,domainA,[ExPArgsClassList,MyDiff,HisDiff],OntType,Scenario,Outcome) :-
	predicateExists(Precond,Action,Scenario),
	domTranslate(RSQ,HisDiff,MyDiff,ExPArgsClassList,TransInfo),
	attemptRefine(Scenario,domainA,OntType,TransInfo,Outcome),!.

consumeRefinement(_,Action,Precond,RSQ,domainAA,[ExPArgsClassList,MyDiff,HisDiff],OntType,Scenario,Outcome) :-
	predicateExists(Precond,Action,Scenario),
	domTranslate(RSQ,MyDiff,HisDiff,ExPArgsClassList,TransInfo),
	attemptRefine(Scenario,domainAA,OntType,TransInfo,Outcome),!.

consumeRefinement(_,Action,WrongClass,_,predicateAA,[NameRSQ,NameFP],OntType,Scenario,Outcome) :-
	predicateExists(WrongClass,Action,Scenario),
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	attemptRefine(Scenario,predicateAA,OntType,TransInfo,Outcome),!.
	
consumeRefinement(Agent,Action,WrongClass,RightClass,precondAA,class,OntType,Scenario,Outcome) :-
	classExists(WrongClass,Action,Scenario),
	precondDomTranslate(pa,Agent,Action,class,RightClass,WrongClass,TransInfo),
	attemptRefine(Scenario,precondAA,OntType,TransInfo,Outcome),!.

consumeRefinement(_,Action,Precond,RelevantSurprisingQuestion,precondAA,_,OntType,Scenario,Outcome) :-
	predicateExists(Precond,Action,Scenario),
	precondTranslate(pa,Agent,Action,RelevantSurprisingQuestion,TransInfo),
	attemptRefine(Scenario,precondAA,OntType,TransInfo,Outcome).

%get_absolute_path(RelativePath, AbsolutePath) :- 
%    environ('ORS_HOME', ORSHomePath), 
%    atom_concat(ORSHomePath, RelativePath, AbsolutePath).
