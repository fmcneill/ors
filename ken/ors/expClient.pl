% experience client manages experience locally and determines how they are used
% the request has the form [SPA, Action, SurprisingQuestions, Questions]
% the response has the form [OldOnt, NewOnt, Repair, Result]

:- use_module(library(lists)),use_module(library(random)),
	use_module(library(system)).

%%% set up temp database to stored save exps, avoids infinit loop %%%%%
:- tell('savedExps.pl'),nl,nl,write('savedExps(agent,action,exps).'),nl,nl,told.

%%%%%% Experience Client %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% requestExp(+SPA,+Action,+SurpriseQuestions,+QueryList,-ReturnedExp) send a request to the server for an experience

requestExps(off).
shareExps(off).

requestExps(SPA,Action,SurpriseQuestions,QueryList,ReturnedExps) :-
	RequestInfo = [SPA,Action,SurpriseQuestions,QueryList],
	out(request(lucas,expServerAgent,retrieve,RequestInfo)),
	write('Waiting for Experience Server reply...'),nl,
	waitForExp(ReturnedExps,5).

% waitForExp(-Response,+Countdown) sleep until experience server replies or timeout

waitForExp(Response,Countdown) :-
	in_noblock(reply(expServerAgent,lucas,Response)),!.

waitForExp(Response,Countdown) :-
	Countdown =< 0,
	write('No reply from Experience Server, skip to Diagnosis.'),nl,
	Response = [],!.
	
waitForExp(Response,Countdown) :-
	sleep(1),
	waitForExp(Response,Countdown - 1).

% shareExps(+RefinementList) send all the experiences to server

shareExps([]) :-
	write('Im done Sharing Experiences'),nl,nl.

shareExps([[SPA,Action,SuprisingQuestions,QueryList,OldOnt,NewOnt,Repair]|Rest]) :-
	write('Im Sharing a refinement Experience regarding agent: '),write(SPA),nl,
	write('executing action: '),write(Action),nl,nl,
	RequestInfo = [SPA,Action,SuprisingQuestions,QueryList,OldOnt,NewOnt,Repair,success],
	out(request(lucas,expServerAgent,store,RequestInfo)),
	shareExps(Rest),!.

shareExps([Experience|Rest]) :-
	write('I could not share experience: '),nl,write(Experience),nl,nl,
	shareExps(Rest).

%%%%% Experience Consumer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% examineAction(+Action,+Just,+CorrectAgent,+QueryList,+SurprisingQuestions,+RefinementList,-NewRefinementList)
% examines the failed action and determine if to perform diagnosis or consume experiences

examineAction(Action,Just,CorrectAgent,QueryList,SurprisingQuestions,RefinementList,[Refinements|RefinementList]) :-
	hasSavedExps(CorrectAgent,Action,_),
	write('Im requesting for relevant experiences ...'),nl,nl,
	requestExps(CorrectAgent,Action,SurprisingQuestions,QueryList,Experiences),
	write('Im Attempting to consume experiences ...'),nl,nl,
	consumeExperiences(CorrectAgent,Action,Experiences,Refinements),
	Refinements \= [],
	saveExps(CorrectAgent,Action,Experiences),!.

examineAction(Action,Just,CorrectAgent,QueryList,SurprisingQuestions,RefinementList,[NewRefinement|RefinementList]) :-
	write('I didnt consume any experience'),nl,
	write('Im diagnosing what the problem is ...'),nl,nl,
	diagnoseFailure(lucas,Action,CorrectAgent,QueryList,Just,SurprisingQuestions,success,Refinement),
	constructRefinementVariable(CorrectAgent,Action,SurprisingQuestions,QueryList,Refinement,NewRefinement).

%%% start local exp storage %%%%%
% hasSavedExps(+Agent,+Action,-Experiences) checks for already retrieved actions, avoid infinit loop

hasSavedExps(Agent,Action,Experiences) :-
	reconsult('savedExps'),
	\+ savedExps(Agent,Action,Experiences),!.

hasSavedExps(_,_,_) :-
	nl,write('already retrieved experiences for this action, skipping to diagnosis ...'),nl,nl,
	fail.

% saveExps(+Agent,+Action,+Experiences) saves retrieved exps, avoid infinit loop

saveExps(Agent,Action,Experiences) :-
	open('savedExps.pl',append,DbStream),
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

consumeExperiences(_,_,[],[]).

consumeExperiences(Agent,Action,[[OldOnt,NewOnt,[Type,Info],success]|RestExps],[Type|TempRefinement]) :-
	write('trying to consume experience'),nl,
	\+ predicateExists(NewOnt,Action),
	consumeRefinement(Agent,Action,OldOnt,NewOnt,Type,Info,Outcome),
	consumeExperiences(Agent,Action,RestExps,TempRefinement),!.

consumeExperiences(Agent,Action,[_|RestExps],RefinementList) :-
	write('failed to consume experience'),nl,
	consumeExperiences(Agent,Action,RestExps,RefinementList).

% predicatExists(+Ont,+Action) checks if Ont already exist in local ont, fails if not

predicateExists(none,_) :- !.

predicateExists(meta,_) :- fail.

predicateExists(shapiro,_) :- fail.

predicateExists(Predicate,Action) :-
	reconsult('centralSig'),
	reconsult('centralThy'),
	rule(Action,[_,Precond,_]),
	predicateExists(Predicate,Precond,true).

classExists(Class,Action) :-
	reconsult('centralSig'),
	reconsult('centralThy'),
	rule(Action,[_,Precond,_]),
	predicateExists(class(_,Class),Precond,true).

predicateExists(_,[],false).

predicateExists(Predicate,[Predicate|_],true) :- !.

predicateExists(Predicate,[_|Rest],Outcome) :-
	predicateExists(Predicate,Rest,Outcome).

% consumeRefinement(+Agent,+Action,+OldOnt,+NewOnt,+Type,+Info,-Outcome) attempts different refinements

consumeRefinement(Agent,_,Precond,meta,_,_,Outcome) :-
	predicateExists(Precond,_),
	translatePred(Agent,TransAgent),
	attemptRefineMeta(agent,TransAgent,Outcome),!.

consumeRefinement(Agent,Action,ProblemPrecond,shapiro,_,Just,Outcome) :-
	predicateExists(ProblemPrecond,Action),
	shapiroCheck(lucas,Agent,ProblemPrecond,Just,Outcome),!.

consumeRefinement(_,Action,Precond,_,propositionalA,[NameRSQ,Position],Outcome) :-
	predicateExists(Precond,Action),
	translatePred(NameRSQ,TransName),
	attemptRefine(propositionalA,[TransName,Position],Outcome),!.

consumeRefinement(_,Action,Precond,_,switchArgs,PredName,Outcome) :-
	predicateExists(Precond,Action),
	translatePred(PredName,TransPred),
	attemptRefine(switchArgs,TransPred,Outcome),!.

consumeRefinement(_,Action,Precond,RSQ,domainA,[ExPArgsClassList,MyDiff,HisDiff],Outcome) :-
	predicateExists(Precond,Action),
	domTranslate(RSQ,HisDiff,MyDiff,ExPArgsClassList,TransInfo),
	attemptRefine(domainA,TransInfo,Outcome),!.

consumeRefinement(_,Action,Precond,RSQ,domainAA,[ExPArgsClassList,MyDiff,HisDiff],Outcome) :-
	predicateExists(Precond,Action),
	domTranslate(RSQ,MyDiff,HisDiff,ExPArgsClassList,TransInfo),
	attemptRefine(domainAA,TransInfo,Outcome),!.

consumeRefinement(_,Action,WrongClass,_,predicateAA,[NameRSQ,NameFP],Outcome) :-
	classExists(WrongClass,Action),
	predTranslate(NameFP,NameRSQ,Action,TransInfo),
	attemptRefine(predicateAA,TransInfo,Outcome),!.
	
consumeRefinement(Agent,Action,WrongClass,RightClass,precondAA,class,Outcome) :-
	classExists(WrongClass,Action),
	precondDomTranslate(lucas,Agent,Action,class,RightClass,WrongClass,TransInfo),
	attemptRefine(precondAA,TransInfo,Outcome),!.

consumeRefinement(_,Action,Precond,RelevantSurprisingQuestion,precondAA,_,Outcome) :-
	predicateExists(Precond,Action),
	precondTranslate(lucas,Agent,Action,RelevantSurprisingQuestion,TransInfo),
	attemptRefine(precondAA,TransInfo,Outcome).
