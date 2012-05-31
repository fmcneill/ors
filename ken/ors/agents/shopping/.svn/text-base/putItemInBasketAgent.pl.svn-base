

% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).


putItemInBasketAgent :-
	reconsult(putItemInBasketAgentThy),
	reconsult(putItemInBasketAgentSig),
	reconsult('../spa'),
%	reconsult('agents/shopping/putItemInBasketAgentThy'),
%	reconsult('agents/shopping/putItemInBasketAgentSig'),
%	reconsult('agents/spa'),
	spa(putItemInBasketAgent).




% this is to connect the spa to the server:



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

:- startServer(_),initialiseClient(_),putItemInBasketAgent.
