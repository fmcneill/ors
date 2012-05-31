%:- use_module(library('linda/client')).




% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).

% Experiences take the form of [[SPA, Action], [[SurprisingQuestions, QueryList], [OldOnt, NewOnt, Repair, Result]]]
% where SQL and QL are both lists themselves
% also can consider them as [index,value]

expServerAgent :-
	reconsult('expDb'),
	nl,nl,write('Experience server is ready now'),nl,nl,
	listen.

listen :-
	in(request(Requestor,ExpServerAgent,RequestType,RequestInfo)),!,
	write('expServerAgent has received a query from '),write(Requestor),nl,
	resolve(RequestType,RequestInfo,Requestor),
	listen.

% name(ExpServerAgent).

% expServerAgent, like all agents, attempts to resolve queries addressed to him

resolve(RequestType,RequestInfo,Requestor) :-
	write('attempting to resolve request'),nl,
	handleRequest(RequestType,RequestInfo,Response),
	out(reply(expServerAgent,Requestor,Response)),
	write('replied to request'),nl,nl.

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
	selectExp(Exps,ReqSQs,ReqQs,Max1 + Max2,Selected),!.

retrieveExp(RequestInfo,invalidRequest) :-
	write('request was not in proper formate, request is:'),nl,
	write(RequestInfo),nl,nl.

% selectExp(+Exps,+ReqSQs,+ReqQs,+MaxScore,-RelevantExps) selects the TOP relevant experience
% Experiences take the form of [[SQL, QL], [OldOnt, NewOnt, Repair, Result]]
% Returned experience takes the form of [OldOnt, NewOnt, Repair, Result]

selectExp([],_,_,_,[]).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client
initialiseClient(PId):-
    see('/tmp/server.addr'),
    read(Host:Port-PId),
    seen,
    linda_client(Host:Port).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%

% Main start server predicate
startServer(PId):-
        file_exists('/tmp/server.addr'),    % if file "server.addr" (handle) exists
        see('/tmp/server.addr'),            % open it,
        read(_:_-PId),                 % get PId,
        seen,                          % close it, and
	up(PId),		% (1) check that process is up
        !.
startServer(PId):-                         % otherwise...
        createServerProgram,               % create server program "server.pl"
        startServerAux,                    % start it up
        sleep(2),                          % wait a bit... (to update file)
        see('/tmp/server.addr'),                % open file with Linda handle
        read(_:_-PId),                     % get PId
        seen.                              % close it

% Check if process is running
% up(PId):-
%         concat(['ps -p ',PId,' | wc'],Command), % type this command in UNIX
% 	exec(Command,[null,pipe(Out),null],_), % and PId will be running
	
% 	get(Out,50).  % if first returned Char is 2

% up(PId):-
%        process_create('ps', ['-p', PId, '|wc'], [stdout(pipe(Out))]),
%        get(Out,50).

up(PId):-
       concat(['ps -p ',PId,' | wc'],Command), % type this command in UNIX
       process_create('/bin/sh', ['-c', Command], [stdout(pipe(Out))]),  %and PId will be running
       write(Out).
     %  get0(Out,50).


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
    tell('../server.pl'),
    write(':- use_module(library(\'linda/server\')),'),nl,
    write('   use_module(library(\'system\')),'),nl,
    write('   pid(PId),'),nl,
    write('   linda((Host:Port)-(tell(\'/tmp/server.addr\'),'),nl,
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

:- startServer(_),initialiseClient(_),expServerAgent.


