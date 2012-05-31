
findPlan(Plan) :-
	(   process_create('~/bin/Metric-FF/ff',['-o','domainOnt.pddl','-f','problemOnt.pddl'],[stdout(pipe(Stream))]),
	   % popen('../Metric-FF/ff -o domainOnt.pddl -f problemOnt.pddl',read,Stream),
	    readPlanOutput(Stream,PlanLines),
	    reverse(PlanLines,Plan)
	;
	    write('it is not possible to find a plan to reach this goal.'),
	    Plan = fail
	).


readPlanOutput(Stream,PlanLines) :-
	readPlanOutput(Stream,PlanLines,[]).
	

readPlanOutput(Stream,RightLines,RightLinesSoFar) :-
	readLine(Stream, Line),!,
	% first of all, we find the line where the plan begins
	(   checkFalse(Line),!,
	    fail
	;   
	    searchForPlan(Stream,RightLines,Line,RightLinesSoFar)
	).


% this is supposed to print a sensible error message if no plan can be found.  doesn't work yet.
%searchForPlan(_,[],Line,_) :-
%	checkFalse(Line).

checkFalse(Line) :-
	name('FALSE',False),
	matchExpression(_,False,_,Line),!.

searchForPlan(_,[],Line,_) :-
	name('TRUE',True),
	matchExpression(_,True,_,Line).

searchForPlan(Stream,RightLines,Line,RightLinesSoFar) :-
	name('step',PlanBegin),
	(   matchExpression([],PlanBegin,PlanLine,Line),
	    processPlanLine(PlanLine,ProcPlanLine),
	    findRestOfPlan(Stream,RightLines,[ProcPlanLine|RightLinesSoFar])
	;
	    readPlanOutput(Stream,RightLines,RightLinesSoFar)
	).

	
findRestOfPlan(Stream,RightLines,RightLinesSoFar) :-
	readLine(Stream, Line),!,
	(   whitespace(Line),
	    RightLines = RightLinesSoFar
	;
	    processPlanLine(Line,ProcLine),
	    findRestOfPlan(Stream,RightLines,[ProcLine|RightLinesSoFar])
	).


processPlanLine(Line,ProcLine) :-
	% first, find where the actual step begins
	name(': ',ActionBegin),
	matchExpression(_,ActionBegin,Action,Line),
	% now find the action name:
	name(' ',Space),
	matchExpression(Name,Space,Args,Action),
	% put the item into lower case:
	processName(Name,ProcName,[]),
	% process all the args:
	processArgs(Args,ProcArgs),
	writeToTerm(ProcName,ProcArgs,ProcLine).

	
	% put the relevant brackets in:
	%name('(',RightBracket),
%	append(ProcName,RightBracket,ActionStart),
%	append(ActionStart,ProcArgs,MostAction),
%	% get rid of the last comma:
%	name(')',LeftBracket),
%	append(MostAction,LeftBracket,CommaAction),
%	name(',)',EndArgs),
%	matchExpression(AllArgs,EndArgs,_,CommaAction),
%	append(AllArgs,LeftBracket,FullAction),
%	name(ProcLine,FullAction).


processName([],ProcName,Name) :-
	reverse(Name,ProcName).

processName([45|[N|T]],ProcName,NameSoFar) :-
	% look out for dashes:
	processName(T,ProcName,[N|NameSoFar]).


processName([H|T],ProcName,NameSoFar) :-
	NewH is H + 32,
	processName(T,ProcName,[NewH|NameSoFar]).


processArgs(Args,ProcArgs) :-
	buildArgsList(Args,ArgsList,[]),
	processArgsList(ArgsList,ProcArgs,[]).

buildArgsList(Args,ArgsList,ArgsListSoFar) :-
	name(' ',ArgEnd),
	matchExpression(FirstArg,ArgEnd,RestArgs,Args),
	buildArgsList(RestArgs,ArgsList,[FirstArg|ArgsListSoFar]).

buildArgsList(LastArg,[LastArg|ArgsList],ArgsList).

processArgsList([],ProcArgs,ProcArgs).

processArgsList([FirstArg|Rest],ProcArgs,ArgsSoFar) :-
	processName(FirstArg,ProcFirst,[]),
%	name(',',Comma),
%	append(ProcFirst,Comma,FullFirst),
	processArgsList(Rest,ProcArgs,[ProcFirst|ArgsSoFar]).


%Writetoterm(Procname,Procargs,Procline) :-
%	Name(Predname,Procname),
%	Procline =.. [Procname,S,E

writeToTerm(ProcName,ProcArgs,ProcLine) :-
	name(PredName,ProcName),
	findall(X,mapName(X,ProcArgs),Xs),
	ProcLine =.. [PredName|Xs].

mapName(X,List) :-
	member(Y,List),
	name(X,Y).