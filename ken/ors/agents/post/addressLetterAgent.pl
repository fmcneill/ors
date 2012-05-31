%:- use_module(library('linda/client')).




% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).



addressLetterAgent :-
	reconsult(addressLetterAgentThy),
	reconsult(addressLetterAgentSig),
	listen.

listen :-
	in(query(QAgent,addressLetterAgent,QueryType,Query)),!,
	write('addressLetterAgent has received a query ..'),nl,
	resolve(QueryType,Query,QAgent),
	listen.

name(AddressLetterAgent).


% addressLetterAgent, like all agents, attempts to resolve queries addressed to him


resolve(QueryType,Query,QAgent) :-
	solve(QueryType,Query,Answer,QAgent),
	out(reply(addressLetterAgent,QAgent,Answer)).

resolve(_,_,QAgent) :-
	out(reply(addressLetterAgent,QAgent,problem)).


% 1) currently, external agents cannot request that a task be performed. 


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






% this is what addressLetterAgent actually does:


solve(request,Action,ok,Agent) :-
	write('I want to perform '),nl,write(Action),write('forrr '),write(Agent),nl,
	tasksIPerform(TasksList),
	Action =.. [ActionName|_],
	( member(ActionName,TasksList) ->
	    ( 
		rule(Action,[_,Preconds,_]),
%		ActionID =.. [ActionName|_],
		% what facts do we need to check with the agent?
		ask(AskList),
		write('asklist is '),write(AskList),nl,
		% what needs to wait for verification till we've checked?
		wait(WaitList),
		checkPreconds(Preconds,AskList,WaitList,Agent,[],[]),
		write('preconds are ok '),nl
	    ;	
		write('whats going on?'),nl,
		fail
	    )
	;
	    write('dont perform that task'),nl,
	    fail
	).


	

solve(request,_,problem,_) :-
	write('sorry, cannot perform the task you request'),nl.





solve(question,Question,Answer,_) :-
	write('question is '),write(Question),nl,
	( Question = performTask(Action,_) ->
	    write('checking tasks ...'),
	    tasksIPerform(TasksList),
	    write(TasksList),nl,
	    Action =.. [ActionName|_],
	    ( member(ActionName,TasksList) ->
		Answer = yes
	    ;
		Answer = no
	    )
	;
	    nonFacts(PredList),
	    ( member(Question,PredList) ->
		Question,
		Answer = Question
	    ;
		( fact(Question) ->
		    Answer = Question,
		    write('answer is '),write(Answer),nl
		;
		    Answer = invalid
		)
	    )
	).



solve(truth,Query,Answer,_) :-
	write('what is the truth of '),write(Query),nl,
	nonFacts(PredList),
	( Query = class(Thing,Class) ->
	    ( checkClass(Thing,Class) ->
		Answer = yes
	    ;
		Answer = no
	    )
	;   
	    ( Query = subclass(Class1,Class2) ->
		write('its about subclasses'),nl,
		( checkSubClass(Class1,Class2) ->
		    Answer = subclass(Class1,Class2),
		    write('this is my answer '),write(Answer),nl
		;   
		    Answer = no
		)
	    ;	
		( member(Query,PredList) ->
		    Query,
		    Answer = yes
		;   
		    ( fact(Query) ->
			Answer = yes
		    ;	
			write('not a fact'),nl,
			Answer = no
		    )
		)
	    )
	).




checkPreconds([],_,_,Agent,ThingsToAsk,WaitingThings) :-
	checkWithAgent(ThingsToAsk,Agent),!,
	checkWaitingThings(WaitingThings).


% if the precond is a class check, check whether we know anything about its class.  If we know it is correct, continue.  If we know it is false, fail.  If we know nothing about it, ask the agent.

checkPreconds([class(Object,Class)|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	(   checkClass(Object,Class),!,
	    checkPreconds(Rest,AskList,WaitList,Agent,ThingsToAsk,WaitingThings)
	;
	    checkClass(Object,WrongClass),!,
	    fail
	;
	    checkPreconds(Rest,AskList,WaitList,Agent,[class(Object,Class)|ThingsToAsk],WaitingThings)
	).

checkPreconds([First|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	First =.. [FirstPred|_FirstArgs],
	member(FirstPred,AskList),
	checkPreconds(Rest,AskList,WaitList,Agent,[First|ThingsToAsk],WaitingThings).

checkPreconds([First|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-	
	First =.. [FirstPred|_FirstArgs],!,
	member(FirstPred,WaitList),
	checkPreconds(Rest,AskList,WaitList,Agent,ThingsToAsk,[First|WaitingThings]).

checkPreconds([First|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	nonFacts(PredList),
	First =.. [FirstPred|_FirstArgs],
	member(FirstPred,PredList),
	FirstPred,
	checkPreconds(Rest,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds([First|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	fact(First),
	checkPreconds(Rest,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds([First|Rest],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	First = calculation(Sum),
	Sum,
	checkPreconds(Rest,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).





% some preconditions need to be checked with the other agent:

checkWithAgent([],_).

checkWithAgent([H|T],Agent) :-
	out(query(addressLetterAgent,Agent,question,H)),
	write('have asked about '),write(H),nl,
	in(reply(Agent,addressLetterAgent,question,Answer)),
	write('lucas has replied '),write(Answer),nl,
	Answer = H,
	checkWithAgent(T,Agent).
	    
% some do not need to be checked with the other agent, but rely on information the other agent will provide during the questioning.  Therefore they must wait until the questioning is completed.

checkWaitingThings([]).

checkWaitingThings([H|T]) :-
	write('these are the waiting things '),write([H|T]),nl,
	( H = calculation(Sum) ->
	    Sum,!
	;
	    fact(H)
	),
	checkWaitingThings(T).


%checkClass(Thing,Class) :-
%	write('checking classes '),nl,
%	class(Thing,Class)
%	;
%	checkSubClass(Thing,Class).


checkClass(Thing,Class) :-
        class(Thing,Class)
        ;
	setof(SubClass,subclass(SubClass,Class),SubClassList),
	checkSubClasses(Thing,SubClassList).

checkSubClasses(_,[]) :-
	fail.
checkSubClasses(Thing,[FirstSubClass|RestSubClasses]) :-
	(   class(Thing,FirstSubClass)
	;   
	    checkClass(Thing,FirstSubClass)
	)
	;
	checkSubClasses(Thing,RestSubClasses).



checkSubClass(Thing,Class) :-
        subclass(Thing,Class)
        ;
	setof(SubClass,subclass(SubClass,Class),SubClassList),
	checkSubsubClasses(Thing,SubClassList).

checkSubsubClasses(_,[]) :-
	fail.
checkSubsubClasses(Thing,[FirstSubClass|RestSubClasses]) :-
	(   subclass(Thing,FirstSubClass)
	;   
	    checkSubClass(Thing,FirstSubClass)
	)
	;
	checkSubsubClasses(Thing,RestSubClasses).






% this is to connect addressLetterAgent to the server:

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



:- startServer(_),initialiseClient(_),addressLetterAgent.


%['agents/post/addressLetterAgentSig'].
%['agents/post/addressLetterAgentThy'].
%addressLetterAgent.