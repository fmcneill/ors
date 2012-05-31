%:- use_module(library('linda/client')).




% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).

% Experiences take the form of [[SPA, Action], [[SurprisingQuestions, QueryList], [OldOnt, NewOnt, Repair, Result]]]
% where SQL and QL are both lists themselves
% also can consider them as [index,value]

expServerAgent :-
	get_absolute_path('/agent_environment/ExpA/expDb',ExpDb),
	reconsult(ExpDb),
	nl,nl,write('Experience server is ready now'),nl,nl,
	listen.

listen :-
	in(request(Requestor,expServerAgent,RequestType,RequestInfo)),!,
	write('expServerAgent has received a query from '),write(Requestor),nl,
	resolve(RequestType,RequestInfo,Requestor),
	listen.

% name(ExpServerAgent).

% expServerAgent, like all agents, attempts to resolve queries addressed to him

resolve(RequestType,RequestInfo,Requestor) :-
	write('attempting to resolve request'),nl,
	handleRequest(RequestType,RequestInfo,Response),
	out(reply(expServerAgent,Requestor,Response)),
	write('sent the following info: '),write(Response),nl,nl.

% expServerAgent handles two types of requests: retrieve and store

handleRequest(retrieve,RequestInfo,Response) :-
	% reconsult('expDb'),
	write('this is a retrieval request'),nl,
	retrieveExp(RequestInfo,Response),!.

handleRequest(store,RequestInfo,Response) :-
	write('this is a storage request'),nl,
	open('expDb.pl',append,DbStream),
	storeExp(RequestInfo,DbStream,Response),
	flush_output(DbStream),
	close(DbStream),!.

handleRequest(RequestType,RequestInfo,invalidRequest) :-
	write('doesnt recognize request type'),nl,
	write('request type is '),write(RequestType),nl,
	write('request info is '),write(RequestInfo),nl.

%% Retrieval predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% retrieveExp(+RequestInfo,+ExpDB,-Response) retrieves experiences
% retrieval requests has the form [SPA, Action, SQs, Qs]

retrieveExp(RequestInfo,Selected) :-
	RequestInfo = [SPA, Action, ReqSQs, ReqQs],
	Index = [SPA,Action],
	findall(Value,experienceDatabase(Index,Value),Exps),
	scoreRequest(ReqSQs,4.0,Max1),
	scoreRequest(ReqQs,1.0,Max2),
	NewMax is Max1 + Max2,
	selectExp(Exps,ReqSQs,ReqQs,NewMax,Selected),!.

retrieveExp(RequestInfo,invalidRequest) :-
	write('request was not in proper format, request is:'),nl,
	write(RequestInfo),nl,nl.

% selectExp(+Exps,+ReqSQs,+ReqQs,+MaxScore,-RelevantExps) selects the TOP relevant experience
% Experiences take the form of [[SQL, QL], [OldOnt, NewOnt, Repair, Result]]
% Returned experience takes the form of [OldOnt, NewOnt, Repair, Result]

selectExp([],_,_,_,none).

selectExp([[[ExpSQs,ExpQs],Refine]|RestExps],ReqSQs,ReqQs,MaxScore,[Refine|TempExps]) :-
	scoreExp(ReqSQs,ReqQs,ExpSQs,ExpQs,MaxScore,ExpScore),
	((ExpScore + 1) / (MaxScore + 1)) > 0.5, % threshold
	selectExp(RestExps,ReqSQs,ReqQs,MaxScore,TempExps),!.

selectExp([_|RestExps],ReqSQs,ReqQs,MaxScore,TempExps) :-
	selectExp(RestExps,ReqSQs,ReqQs,MaxScore,TempExps).

% scoreExp(+ReqSQs,+ReqQs,+ExpSQs,+ExpQs,+Max,-Output)
% if no questions were asked, experiences are scored differently

scoreExp(ReqSQs,ReqQs,ExpSQs,ExpQs,0,Score1 + Score2) :-
	scoreExp(ExpSQs,ReqSQs,4.0,Score1),
	scoreExp(ExpQs,ReqQs,1.0,Score2),!.

scoreExp(ReqSQs,ReqQs,ExpSQs,ExpQs,_,Score1 + Score2) :-
	scoreExp(ReqSQs,ExpSQs,4.0,Score1),
	scoreExp(ReqQs,ExpQs,1.0,Score2).
 
% scoreExp(+ReqQs,+ExpQs,+Type,-Output) scores the experience in terms of relevancy
% for each Q in expQs: if isMember(Q,RequestQs): score += 2 (for SQ), 1 (for Q)

scoreExp([],_,_,0).

scoreExp([Q|RestQs],ExpQs,Type,TempScore + Type) :-
	member(Q,ExpQs),
	scoreExp(RestQs,ExpQs,Type,TempScore),!.

scoreExp([_|RestQs],ExpQs,Type,TempScore) :-
	scoreExp(RestQs,ExpQs,Type,TempScore),!.

scoreRequest([],_,0).
	
scoreRequest([_|RestQs],Type,TempScore + Type) :-
	scoreRequest(RestQs,Type,TempScore).

%% storage predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% storeExp(?RequestInfo,+ExpDB,-Response) structure and store experiences
% Experiences take the form of [[SPA, Action], [[SQs, Qs], [OldOnt, NewOnt, Repair, Result]]]

storeExp(RequestInfo,DbStream,success) :-
	RequestInfo = [SPA, Action, SQs, Qs, OldOnt, NewOnt, Repair, Result],
	[Index,Value] = [[SPA,Action],[[SQs,Qs],[OldOnt,NewOnt,Repair,Result]]],
	nl(DbStream),write(DbStream,'experienceDatabase('),
	write(DbStream,Index),write(DbStream,','),write(DbStream,Value),
	write(DbStream,').'),nl(DbStream),
	write('successfully saved experience'),nl,!.

storeExp(_,_,failure) :-
	write('failed to save experience'),nl.

% get_absolute_path(+RelativePath, ?AbsolutePath)
% e.g. get_absolute_path('/agent_environment', AbsolutePath).

get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client

initialiseClient(PId):-
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


%%%%%%%%%%%%%%%%%%%%%% START  CLIENT

:- initialiseClient(_),expServerAgent.


