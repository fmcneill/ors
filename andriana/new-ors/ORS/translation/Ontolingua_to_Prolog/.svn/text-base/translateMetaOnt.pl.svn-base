translateMetaOnt(MetaList,Scenario,ScenarioPath) :-
	writeList(MetaList),
	buildMyFacts(MyFacts,[],MetaList),
	buildTransitive(Transitive,[],MetaList),
	buildInform(Inform,[],MetaList),
	buildAgentNeeded(Agents,[],MetaList),
	% PROTECTED
	buildFunction(Function,[],MetaList),
	buildArgument(Argument,[],MetaList),
	% ~PROTECTED
	buildPredicateHierarchy(PredHier,[],MetaList),
	writeMetaFile(MyFacts,Transitive,Inform,Agents,Function,Argument,PredHier,Scenario,ScenarioPath).


writeList([]).

writeList([FirstEl|Rest]) :-
	% write('this el is '),nl,
	name(FirstName,FirstEl),
	% write(FirstName),
	writeList(Rest).
	

buildMyFacts(MyFacts,MyFacts,[]).

buildMyFacts(MyFacts,MyFactsSoFar,[FirstIndiv|Rest]) :-
	name('My-Fact ',ID),
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

% PROTECTED

buildFunction(FunctionList,FunctionList,[]).

buildFunction(FunctionList,FunctionSoFar,[FirstIndiv|Rest]) :-
	name('Protect-Fun ',ProtectID),
	matchExpression(_,ProtectID,AfterProtectID,FirstIndiv),
	name(' ',Space),
	matchExpression(FunctionName,Space,AfterFunctionName,AfterProtectID),
	matchExpression(ProtectedName,Space,AfterProtectedName,AfterFunctionName),
	name(')',RightBracket),
	matchExpression(LevelName,RightBracket,_,AfterProtectedName),
	buildFunction(FunctionList,[FunctionName|[ProtectedName|[LevelName|FunctionSoFar]]],Rest).

buildFunction(FunctionList,FunctionSoFar,[_|Rest]) :-
	buildFunction(FunctionList,FunctionSoFar,Rest).


buildArgument(ArgumentList,ArgumentList,[]).

buildArgument(ArgumentList,ArgumentSoFar,[FirstIndiv|Rest]) :-
	name('Protect-Arg ',ProtectID),
	matchExpression(_,ProtectID,AfterProtectID,FirstIndiv),
	name(' ',Space),
	matchExpression(FunctionName,Space,AfterFunctionName,AfterProtectID),
	matchExpression(ArgumentName,Space,AfterArgumentName,AfterFunctionName),
	matchExpression(ProtectedName,Space,AfterProtectedName,AfterArgumentName),
	name(')',RightBracket),
	matchExpression(LevelName,RightBracket,_,AfterProtectedName),
	buildArgument(ArgumentList,[FunctionName|[ArgumentName|[ProtectedName|[LevelName|ArgumentSoFar]]]],Rest).

buildArgument(ArgumentList,ArgumentSoFar,[_|Rest]) :-
	buildArgument(ArgumentList,ArgumentSoFar,Rest).

% ~PROTECTED

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


writeMetaFile(MyFacts,Transitive,Inform,Agents,Function,Argument,PredHierList,Scenario,ScenarioPath) :-
	atom_concat(ScenarioPath,'/metaOnt.pl',MetaPath),
	tell(MetaPath),
	write('nonFacts([class,member,assert]).'),nl,nl,
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
	% PROTECTED
	writeFunction(Function),nl,nl,
	writeArgument(Argument),nl,nl,
	% ~PROTECTED
	writePredicateHierarchy(PredHierList),
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


writeMetaMyFacts([]) :-
	write('calculuation').

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

% PROTECTED

writeFunction([]).

writeFunction([FirstFunction|[FirstProtected|[FirstLevel|Rest]]]) :-
	convertName(FirstFunction,ConvFunction,[]),
	name(FunctionName,ConvFunction),
	convertName(FirstProtected,ConvProtected,[]),
	name(ProtectedName,ConvProtected),
	convertName(FirstLevel,ConvLevel,[]),
	name(LevelName,ConvLevel),
	write('protect('),
	write(FunctionName),
	write(','),
	write(ProtectedName),
	write(','),
	write(LevelName),
	write(').'),nl,
	writeFunction(Rest).

writeArgument([]).

writeArgument([FirstFunction|[FirstArgument|[FirstProtected|[FirstLevel|Rest]]]]) :-
	convertName(FirstFunction,ConvFunction,[]),
	name(FunctionName,ConvFunction),
	convertName(FirstArgument,ConvArgument,[]),
	name(ArgumentName,ConvArgument),
	convertName(FirstProtected,ConvProtected,[]),
	name(ProtectedName,ConvProtected),
	convertName(FirstLevel,ConvLevel,[]),
	name(LevelName,ConvLevel),
	write('protect('),
	write(FunctionName),
	write(','),
	write(ArgumentName),
	write(','),
	write(ProtectedName),
	write(','),
	write(LevelName),
	write(').'),nl,
	writeArgument(Rest).

% ~PROTECTED

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
