translateMetaOnt(MetaList,Scenario,ScenarioPath) :-
    	buildMyFacts(MyFacts,[],MetaList),
    	buildTransitive(Transitive,[],MetaList),
    	buildInform(Inform,[],MetaList),
    	buildAgentNeeded(Agents,[],MetaList),
    	buildPredicateHierarchy(PredHier,[],MetaList),
    	% PROTECTED
    	buildFunction(Function,[],MetaList),
    	buildArgument(Argument,[],MetaList),
    	buildTasksIPerform(Tasks,[],MetaList),
    	buildAsk(Ask,[],MetaList),
    	buildWait(Wait,[],MetaList),
    	buildNonFacts(NonFacts,[],MetaList),
    	% ~PROTECTED
    	writeMetaFile(MyFacts,Transitive,Inform,Agents,PredHier,Function,Argument,Tasks,Ask,Wait,NonFacts,Scenario,ScenarioPath).


writeList([]).

writeList([FirstEl|Rest]) :-
    	%write('this el is '),nl,
    	write(FirstEl),nl,nl,
    	name(FirstName,FirstEl),nl,
    	write(FirstName),nl,nl,
    	writeList(Rest).
   

buildMyFacts(MyFacts,MyFacts,[]).

buildMyFacts(MyFacts,MyFactsSoFar,[FirstIndiv|Rest]) :-
    	name('My-Facts ',ID),
    	matchExpression(_,ID,_,FirstIndiv),
    	name('Define-Frame ',Begin),
    	matchExpression(_,Begin,NameAndMore,FirstIndiv),
    	name(' :',EndName),
    	matchExpression(Name,EndName,_,NameAndMore),
    	buildMyFacts(MyFacts,[Name|MyFactsSoFar],Rest).

buildMyFacts(MyFacts,MyFactsSoFar,[_|Rest]) :-
    	buildMyFacts(MyFacts,MyFactsSoFar,Rest).


buildTransitive(Transitive,Transitive,[]).

buildTransitive(Transitive,TransitiveSoFar,[FirstIndiv|Rest]) :-
    	name('Transitive ',ID),
    	matchExpression(_,ID,_,FirstIndiv),
    	name('Define-Frame ',Begin),
    	matchExpression(_,Begin,NameAndMore,FirstIndiv),
    	name(' :',EndName),
    	matchExpression(Name,EndName,_,NameAndMore),
    	buildTransitive(Transitive,[Name|TransitiveSoFar],Rest).

buildTransitive(Transitive,TransitiveSoFar,[_|Rest]) :-
    	buildTransitive(Transitive,TransitiveSoFar,Rest).


buildInform(Inform,Inform,[]).

buildInform(Inform,InformSoFar,[FirstIndiv|Rest]) :-
    	name('Inform ',ID),
    	matchExpression(_,ID,_,FirstIndiv),
    	name('Define-Frame ',Begin),
    	matchExpression(_,Begin,NameAndMore,FirstIndiv),
    	name(' :',EndName),
    	matchExpression(Name,EndName,_,NameAndMore),
    	buildInform(Inform,[Name|InformSoFar],Rest).

buildInform(Inform,InformSoFar,[_|Rest]) :-
    	buildInform(Inform,InformSoFar,Rest).


buildAgentNeeded(AgentList,AgentList,[]).

buildAgentNeeded(AgentList,AgentsSoFar,[FirstIndiv|Rest]) :-
    	name('Agent-Needed ',ID),
    	matchExpression(_,ID,AfterID,FirstIndiv),
    	name(' ',Space),
    	matchExpression(AgentName,Space,AfterName,AfterID),
    	name(')',LeftBracket),
    	matchExpression(ActionName,LeftBracket,_,AfterName),
    	buildAgentNeeded(AgentList,[AgentName|[ActionName|AgentsSoFar]],Rest).

buildAgentNeeded(AgentList,AgentsSoFar,[_|Rest]) :-
    	buildAgentNeeded(AgentList,AgentsSoFar,Rest).


buildPredicateHierarchy(HierList,HierList,[]).

buildPredicateHierarchy(HierList,HierSoFar,[FirstIndiv|Rest]) :-
    	name('Define-Class ',ClassID),
    	matchExpression(_,ClassID,RestLine,FirstIndiv),
    	name(' ',Space),
    	matchExpression(ClassName,Space,RestClass,RestLine),
    	name(':Def (And (',SuperClassBegin),
    	matchExpression(_,SuperClassBegin,SuperClassInfo,RestClass),
    	name(' ?X)',SuperClassEnd),
    	matchExpression(SuperClass,SuperClassEnd,_,SuperClassInfo),
	buildPredicateHierarchy(HierList,[ClassName|[SuperClass|HierSoFar]],Rest).

buildPredicateHierarchy(HierList,HierSoFar,[_|Rest]) :-
 	buildPredicateHierarchy(HierList,HierSoFar,Rest).


% PROTECTED

buildFunction(FunctionList,FunctionList,[]).

buildFunction(FunctionList,FunctionSoFar,[FirstIndiv|Rest]) :-
	name('Axioms ((',AxiomsID),
	matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
	name('(',SplitID),
	split(AfterAxiomsID,SplitID,SplitResult),
	buildFunctionAxioms(FunctionAxioms,[],SplitResult),
	append(FunctionAxioms,FunctionSoFar,FunctionSoFar2),
	buildFunction(FunctionList,FunctionSoFar2,Rest).

buildFunction(FunctionList,FunctionSoFar,[_|Rest]) :-
	buildFunction(FunctionList,FunctionSoFar,Rest).


buildFunctionAxioms(FunctionAxiomsList,FunctionAxiomsList,[]).

buildFunctionAxioms(FunctionAxiomsList,FunctionAxiomsSoFar,[FirstAxiom|Rest]) :-
	name('Protect-Function ',ProtectID),
	matchExpression(_,ProtectID,AfterProtectID,FirstAxiom),
	name(' ',Space),
	matchExpression(FunctionName,Space,AfterFunctionName,AfterProtectID),
	matchExpression(ProtectedName,Space,AfterProtectedName,AfterFunctionName),
	name(')',RightBracket),
	matchExpression(LevelName,RightBracket,_,AfterProtectedName),
	buildFunctionAxioms(FunctionAxiomsList,[FunctionName|[ProtectedName|[LevelName|FunctionAxiomsSoFar]]],Rest).

buildFunctionAxioms(FunctionAxiomsList,FunctionAxiomsSoFar,[_|Rest]) :-
	buildFunctionAxioms(FunctionAxiomsList,FunctionAxiomsSoFar,Rest).


buildArgument(ArgumentList,ArgumentList,[]).

buildArgument(ArgumentList,ArgumentSoFar,[FirstIndiv|Rest]) :-
	name('Axioms ((',AxiomsID),
	matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
	name('(',SplitID),
	split(AfterAxiomsID,SplitID,SplitResult),
	buildArgumentAxioms(ArgumentAxioms,[],SplitResult),
	append(ArgumentAxioms,ArgumentSoFar,ArgumentSoFar2),
	buildArgument(ArgumentList,ArgumentSoFar2,Rest).

buildArgument(ArgumentList,ArgumentSoFar,[_|Rest]) :-
	buildArgument(ArgumentList,ArgumentSoFar,Rest).


buildArgumentAxioms(ArgumentAxiomsList,ArgumentAxiomsList,[]).

buildArgumentAxioms(ArgumentAxiomsList,ArgumentAxiomsSoFar,[FirstSplit|Rest]) :-
	name('Protect-Argument ',ProtectID),
	matchExpression(_,ProtectID,AfterProtectID,FirstSplit),
	name(' ',Space),
	matchExpression(FunctionName,Space,AfterFunctionName,AfterProtectID),
	matchExpression(ArgumentName,Space,AfterArgumentName,AfterFunctionName),
	matchExpression(ProtectedName,Space,AfterProtectedName,AfterArgumentName),
	name(')',RightBracket),
	matchExpression(LevelName,RightBracket,_,AfterProtectedName),
	buildArgumentAxioms(ArgumentAxiomsList,[FunctionName|[ArgumentName|[ProtectedName|[LevelName|ArgumentAxiomsSoFar]]]],Rest).

buildArgumentAxioms(ArgumentAxiomsList,ArgumentAxiomsSoFar,[_|Rest]) :-
	buildArgumentAxioms(ArgumentAxiomsList,ArgumentAxiomsSoFar,Rest).


buildTasksIPerform(Tasks,Tasks,[]).

buildTasksIPerform(Tasks,TasksSoFar,[FirstIndiv|Rest]) :-
	name('Axioms ((',AxiomsID),
	matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
	name('(',SplitID),
	split(AfterAxiomsID,SplitID,SplitResult),
	buildTasksIPerformAxioms(TasksAxioms,[],SplitResult),
	append(TasksAxioms,TasksSoFar,TasksSoFar2),
	buildTasksIPerform(Tasks,TasksSoFar2,Rest).


buildTasksIPerform(Tasks,TasksSoFar,[_|Rest]) :-
   	buildTasksIPerform(Tasks,TasksSoFar,Rest).


buildTasksIPerformAxioms(TasksAxioms,TasksAxioms,[]).

buildTasksIPerformAxioms(TasksAxioms,TasksAxiomsSoFar,[FirstSplit|Rest]) :-
	name('Tasks-I-Perform ',ID),
   	matchExpression(_,ID,AfterID,FirstSplit),
    	name(')',RightBracket),
    	matchExpression(TaskName,RightBracket,_,AfterID), 
	buildTasksIPerformAxioms(TasksAxioms,[TaskName|TasksAxiomsSoFar],Rest).

buildTasksIPerformAxioms(TasksAxioms,TasksAxiomsSoFar,[_|Rest]) :-
	buildTasksIPerformAxioms(TasksAxioms,TasksAxiomsSoFar,Rest).


buildAsk(Ask,Ask,[]).

buildAsk(Ask,AskSoFar,[FirstIndiv|Rest]) :- 
        name('Axioms ((',AxiomsID),
        matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
        name('(',SplitID),
        split(AfterAxiomsID,SplitID,SplitResult),
        buildAskAxioms(AskAxioms,[],SplitResult),
	append(AskAxioms,AskSoFar,AskSoFar2),
	buildAsk(Ask,AskSoFar2,Rest).

buildAsk(Ask,AskSoFar,[_|Rest]) :-
    	buildAsk(Ask,AskSoFar,Rest).


buildAskAxioms(AskAxioms,AskAxioms,[]).

buildAskAxioms(AskAxioms,AskAxiomsSoFar,[FirstSplit|Rest]) :-
    	name('Ask ',ID),
    	matchExpression(_,ID,AfterID,FirstSplit),
    	name(')',RightBracket),
    	matchExpression(AskName,RightBracket,_,AfterID),   
    	buildAskAxioms(AskAxioms,[AskName|AskAxiomsSoFar],Rest).

buildAskAxioms(AskAxioms,AskAxiomsSoFar,[_|Rest]) :-
	buildAskAxioms(AskAxioms,AskAxiomsSoFar,Rest). 


buildWait(Wait,Wait,[]).

buildWait(Wait,WaitSoFar,[FirstIndiv|Rest]) :-
	name('Axioms ((',AxiomsID),
        matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
        name('(',SplitID),
        split(AfterAxiomsID,SplitID,SplitResult),
        buildWaitAxioms(WaitAxioms,[],SplitResult),
	append(WaitAxioms,WaitSoFar,WaitSoFar2),
	buildWait(Wait,WaitSoFar2,Rest).
    	
buildWait(Wait,WaitSoFar,[_|Rest]) :-
    	buildWait(Wait,WaitSoFar,Rest).


buildWaitAxioms(WaitAxioms,WaitAxioms,[]).

buildWaitAxioms(WaitAxioms,WaitAxiomsSoFar,[FirstSplit|Rest]) :-
	name('Wait ',ID),
	matchExpression(_,ID,AfterID,FirstSplit),
	name(')',RightBracket),
	matchExpression(WaitName,RightBracket,_,AfterID),  
	buildWaitAxioms(WaitAxioms,[WaitName|WaitAxiomsSoFar],Rest).

buildWaitAxioms(WaitAxioms,WaitAxiomsSoFar,[_|Rest]) :-
    	buildWaitAxioms(WaitAxioms,WaitAxiomsSoFar,Rest).


buildNonFacts(NonFacts,NonFacts,[]).

buildNonFacts(NonFacts,NonFactsSoFar,[FirstIndiv|Rest]) :-
	name('Axioms ((',AxiomsID),
        matchExpression(_,AxiomsID,AfterAxiomsID,FirstIndiv),
        name('(',SplitID),
        split(AfterAxiomsID,SplitID,SplitResult),
        buildNonFactsAxioms(NonFactsAxioms,[],SplitResult),
	append(NonFactsAxioms,NonFactsSoFar,NonFactsSoFar2),
	buildTasksIPerform(NonFacts,NonFactsSoFar2,Rest).
    
buildNonFacts(NonFacts,NonFactsSoFar,[_|Rest]) :-
    	buildNonFacts(NonFacts,NonFactsSoFar,Rest).


buildNonFactsAxioms(NonFactsAxioms,NonFactsAxioms,[]).

buildNonFactsAxioms(NonFactsAxioms,NonFactsAxiomsSoFar,[FirstSplit|Rest]) :-
	name('Non-Facts ',ID),
	matchExpression(_,ID,AfterID,FirstSplit),
	name(')',RightBracket),
    	matchExpression(NonFactsName,RightBracket,_,AfterID),   
    	buildNonFactsAxioms(NonFactsAxioms,[NonFactsName|NonFactsAxiomsSoFar],Rest).

buildNonFactsAxioms(NonFactsAxioms,NonFactsAxiomsSoFar,[_|Rest]) :-
	buildNonFactsAxioms(NonFactsAxioms,NonFactsAxiomsSoFar,Rest).


% ~PROTECTED

writeMetaFile(MyFacts,Transitive,Inform,Agents,PredHierList,Function,Argument,Tasks,Ask,Wait,NonFacts,Scenario,ScenarioPath) :-
   	 atom_concat(ScenarioPath,'/metaOnt.pl',MetaPath),
    	tell(MetaPath),
    	% write('nonFacts([class,member,assert]).'),nl,nl,
    	write('myFacts(['),
    	writeMetaMyFacts(MyFacts),
    	write(']).'),nl,nl,
    	write('transitivePreds(['),
    	writeMetaList(Transitive),
    	write(']).'),nl,nl,
    	write('inform(['),
    	writeMetaList(Inform),
    	write(']).'),nl,nl,
    	writeAgents(Agents),nl,nl,
    	writePredicateHierarchy(PredHierList),nl,nl,
    	% PROTECTED
	write('protectFunction(['),
    	writeFunction(Function),
	write(']).'),nl,nl,
	write('protectArgument(['),
    	writeArgument(Argument),
	write(']).'),nl,nl,
    	write('tasksIPerform(['),
    	writeMetaList(Tasks),
    	write(']).'),nl,nl,   
    	write('ask(['),
    	writeMetaList(Ask),
    	write(']).'),nl,nl,
    	write('wait(['),
    	writeMetaList(Wait),
    	write(']).'),nl,nl,
    	write('nonFacts(['),
    	writeMetaList(NonFacts),
    	write(']).'),
    	% ~PROTECTED
    	told.


writeMetaList([]).

writeMetaList([H]) :-
    	convertName(H,ConvH,[]),
    	name(GoodH,ConvH),
    	write(GoodH).
	
writeMetaList([H|T]) :-
    	convertName(H,ConvH,[]),
    	name(GoodH,ConvH),
    	write(GoodH),
    	write(','),
    	writeMetaList(T).


writeMetaMyFacts([]).


writeMetaMyFacts([H|T]) :-
    	convertName(H,ConvH,[]),
    	name(GoodH,ConvH),
    	write(GoodH),
    	write(','),
    	writeMetaList(T).
	

writeAgents([]).

writeAgents([FirstAgent|[FirstAction|Rest]]) :-
    	convertName(FirstAgent,ConvAgent,[]),
    	name(AgentName,ConvAgent),
    	convertName(FirstAction,ConvAction,[]),
    	name(ActionName,ConvAction),
    	write('agentNeeded('),
    	write(AgentName),
    	write(','),
    	write(ActionName),
   	write(').'),nl,
    	writeAgents(Rest).


writePredicateHierarchy([]).

writePredicateHierarchy([FirstClass|[FirstSuperClass|Rest]]) :-
    	convertName(FirstClass,ConvClass,[]),
    	name(ClassName,ConvClass),
    	convertName(FirstSuperClass,ConvSuper,[]),
    	name(SuperName,ConvSuper),
    	write('predSubclass('),
    	write(ClassName),
    	write(','),
    	write(SuperName),
    	write(').'),nl,
    	writePredicateHierarchy(Rest).


writeFunction([]).

writeFunction([FirstFunction|[FirstProtected|[FirstLevel|[]]]]) :-
	convertName(FirstFunction,ConvFunction,[]),
    	name(FunctionName,ConvFunction),
    	convertName(FirstProtected,ConvProtected,[]),
    	name(ProtectedName,ConvProtected),
    	convertName(FirstLevel,ConvLevel,[]),
    	name(LevelName,ConvLevel),
    	write('\'('),
    	write(FunctionName),
    	write(','),
    	write(ProtectedName),
    	write(','),
    	write(LevelName),
    	write(')\'').
    	

writeFunction([FirstFunction|[FirstProtected|[FirstLevel|Rest]]]) :-
    	convertName(FirstFunction,ConvFunction,[]),
    	name(FunctionName,ConvFunction),
    	convertName(FirstProtected,ConvProtected,[]),
    	name(ProtectedName,ConvProtected),
    	convertName(FirstLevel,ConvLevel,[]),
    	name(LevelName,ConvLevel),
    	write('\'('),
    	write(FunctionName),
    	write(','),
    	write(ProtectedName),
    	write(','),
    	write(LevelName),
    	write(')\''),
    	writeFunction(Rest).
	

writeArgument([]).

writeArgument([FirstFunction|[FirstArgument|[FirstProtected|[FirstLevel|[]]]]]) :-
    	convertName(FirstFunction,ConvFunction,[]),
    	name(FunctionName,ConvFunction),
    	convertName(FirstArgument,ConvArgument,[]),
    	name(ArgumentName,ConvArgument),
    	convertName(FirstProtected,ConvProtected,[]),
    	name(ProtectedName,ConvProtected),
    	convertName(FirstLevel,ConvLevel,[]),
    	name(LevelName,ConvLevel),
    	write('\'('),
    	write(FunctionName),
    	write(','),
    	write(ArgumentName),
    	write(','),
    	write(ProtectedName),
    	write(','),
    	write(LevelName),
	write(')\'').

writeArgument([FirstFunction|[FirstArgument|[FirstProtected|[FirstLevel|Rest]]]]) :-
    	convertName(FirstFunction,ConvFunction,[]),
    	name(FunctionName,ConvFunction),
    	convertName(FirstArgument,ConvArgument,[]),
    	name(ArgumentName,ConvArgument),
    	convertName(FirstProtected,ConvProtected,[]),
    	name(ProtectedName,ConvProtected),
    	convertName(FirstLevel,ConvLevel,[]),
    	name(LevelName,ConvLevel),
    	write('\'('),
    	write(FunctionName),
    	write(','),
    	write(ArgumentName),
    	write(','),
    	write(ProtectedName),
    	write(','),
    	write(LevelName),
    	write(')\''),
    	writeArgument(Rest).

split(String, "", SplitResult) :- 
	!,
	name(String, SplitResult).
	split(String, Delimiters, SplitResult) :-
	properSplit(String, Delimiters, SplitResult).


properSplit(String, Delimiters, SplitResult) :-
	(append(Substring, [Delimiter|Rest], String),
	memberchk(Delimiter, Delimiters) -> SplitResult = [Substring|Substrings],
	properSplit(Rest, Delimiters, Substrings)
	; 
	SplitResult = [String]
	).


% ~PROTECTED


