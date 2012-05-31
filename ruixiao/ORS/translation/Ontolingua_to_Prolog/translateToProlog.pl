:- use_module(library(lists)),use_module(library(random)).


% ****************************************

% translateToProlog(+ProbClassList,+FactList,+RuleList,+NumericalPredicateList,+ClassHierarchyList)
% takes the information that has been stripped from the ontology during the translation process and uses it to write the signature and theory files required by the central agent.  in order to adapt this to writing the signature and theory for any agent, all that is required is to change the names of the files that are written to.

translateToProlog(ProbClassList,FactList,RuleList,NumericalPredicateList,ClassHierarchyList,PredicateList,Scenario,ScenarioPath) :-
	atom_concat(ScenarioPath,'/centralThy.pl',CentralThyPath),
	tell(CentralThyPath),
	buildTheoryFile(ProbClassList,FactList),
	told,
	atom_concat(ScenarioPath,'/centralSig.pl',CentralSigPath),
	tell(CentralSigPath),
	buildSignatureFile(RuleList,NumericalPredicateList,ClassHierarchyList,PredicateList),
	told.

% buildTheoryFile(+ClassList,+FactList)
% writes the file which contains information about individuals

buildTheoryFile(ClassList,FactList) :-
	write(':- dynamic fact/1,class/2.'),nl,nl,
	processClasses(ClassList),nl,nl,
	processFacts(FactList).

% processClass(+ClassList)
% writes a list of the classes of each individual.  the superclasses are not listed, as the class hierarchy is dealt with in the signature file

processClasses([]).

processClasses([[_|Class]|Rest]) :-
	name('Meta-Variable',Class),
	processClasses(Rest).

processClasses([[FirstIndiv|Class]|Rest]) :-
	convertName(FirstIndiv,PrologIndiv,[]),
	convertName(Class,PrologClass,[]),
	name(FirstIndivName,PrologIndiv),
	name(ClassName,PrologClass),
	write('class('),
	write(FirstIndivName),
	write(','),
	write(ClassName),
	write(').'),nl,
	processClasses(Rest).


% convertName(+Name,-ConvertedName,+NameSoFar)
% converts a name so that it is readable by prolog

convertName([],NewName,RevName) :-
	reverse(RevName,MostName),
	name('metaVar',Meta),
	matchExpression(BeforeMeta,Meta,AfterMeta,MostName),
	name('MetaVar',NewMeta),
	append(BeforeMeta,NewMeta,NewBefore),
	append(NewBefore,AfterMeta,NewName).

convertName([],RevName,NewName) :-
	reverse(RevName,NewName).

convertName([FirstChar|Rest],NewName,NewSoFar) :-
	FirstChar > 64,
	FirstChar < 91,
	NewFirstChar is FirstChar + 32,
	convertName(Rest,NewName,[NewFirstChar|NewSoFar]).

convertName([Dash|[Next|Rest]],NewName,NewSoFar) :-
	name('-',[Dash]),
	convertName(Rest,NewName,[Next|NewSoFar]).


convertName([FirstChar|Rest],NewName,NewSoFar) :-
	convertName(Rest,NewName,[FirstChar|NewSoFar]).



convertClassName([],RevName,NewName) :-
	reverse(RevName,NewName).

convertClassName([Dash|[Next|Rest]],NewName,NewSoFar) :-
	name('-',[Dash]),
	convertClassName(Rest,NewName,[Next|NewSoFar]).


convertClassName([FirstChar|Rest],NewName,NewSoFar) :-
	convertClassName(Rest,NewName,[FirstChar|NewSoFar]).

% processFacts(+FactList)
% processes the facts so that they are readable

processFacts([]).

processFacts([FirstFact|Rest]) :-
	findFact(FirstFact),
	processFacts(Rest).

% findFact(+Fact)
% writes the fact

findFact(Fact) :-
	name('Agent-Needed ',Id),
	matchExpression(_,Id,Args,Fact),
	name(' ',Space),
	matchExpression(Agent,Space,Action,Args),
	convertName(Agent,PrologAgent,[]),
	convertName(Action,PrologAction,[]),
	name(AgentName,PrologAgent),
	name(ActionName,PrologAction),
	write('agentNeeded('),
	write(AgentName),
	write(','),
	write(ActionName),
	write('.'),nl.


findFact(Fact) :-
	name('= ',Equ),
	matchExpression(_,Equ,Info,Fact),
	write('fact'),
	writeArgs(Info),nl.

findFact(Fact) :-
	write('fact'),
	writeArgs(Fact),nl.

% writeArgs(+Info)
% writes doen the first arg.  checks to see if it is the last one, as this affects the punctuation.

writeArgs(Info) :-
	findFirstArg(Info,FirstArg,Rest,notLast),
	convertName(FirstArg,PrologArg,[]),
	name(ArgName,PrologArg),
	write(ArgName),
	write('('),
	writeArgsRec(Rest).

% writeArgsRec(+Info)
% writes the rest of the args

writeArgsRec(Info) :-
	findFirstArg(Info,FirstArg,Rest,notLast),
	convertName(FirstArg,PrologArg,[]),
	name(ArgName,PrologArg),
	write(ArgName),
	write(','),
	writeArgsRec(Rest).

writeArgsRec(Info) :-
	findFirstArg(Info,FirstArg,_,last),
	convertName(FirstArg,PrologArg,[]),
	name(ArgName,PrologArg),
	write(ArgName),
	write(').').


% findFirstArg(+Info,-FirstArg,-Rest,+position)
% finds the first arg from a list and removes a bracket so that it can be written down

findFirstArg(Info,FirstArg,Rest,notLast) :-
	findArg(Info,First,Rest),
	removeBracket(First,FirstArg).

findFirstArg(Info,FirstArg,Rest,notLast) :-
	findArg(Info,FirstArg,Rest).

findFirstArg(Info,Info,[],last).

% findArg(+Info,-First,-Rest)
% returns the first argument from a string and returns the rest

findArg(Info,First,Rest) :-
	name(' ',Space),
	matchExpression(First,Space,Rest,Info),!.

% removeBracket(+First,-FirstArg)
% removes the end bracket from a list of arguments

removeBracket(First,FirstArg) :-
	name(')',RightBracket),
	matchExpression(FirstArg,RightBracket,[],First),!.








% *************************************************



buildSignatureFile(RuleList,NumPredList,ClassHierarchyList,PredicateList) :-
	processRules(RuleList,NumPredList,PredicateList,ClassHierarchyList),
%	processMetaInf,
	processPredicates(PredicateList),
	processClassHierarchy(ClassHierarchyList).



processRules(RulesList,NumPredList,PredicateList,ClassHierList) :-
	processRules(RulesList,NumPredList,1,PredicateList,ClassHierList,[]).

processRules([],_,_,_,ClassHierList,AxiomInfo) :-
	writeRules(AxiomInfo,ClassHierList).

processRules([FirstAction|Rest],NumPredList,Counter,PredicateList,ClassHierList,AxiomInfoSoFar) :-
	processAction(FirstAction,NumPredList,Counter,PredicateList,NewAxiomInfo),
	NewCounter is Counter + 1,
	processRules(Rest,NumPredList,NewCounter,PredicateList,ClassHierList,[NewAxiomInfo|AxiomInfoSoFar]).


processAction([FirstActionName|FullList],NumPredList,Counter,PredicateList,NewAxiomInfo) :-
	convertName(FirstActionName,PrologAction,[]),
	name(ActionName,PrologAction),
	FullList = [_|[Preconds|Postconds]],
	rewriteNumPreconds(Preconds,NumPredList,PrecondMarkers,PrecondsNoNum,_,_),
	rewriteNumPostconds(Postconds,NumPredList,PrecondMarkers,PostcondsNoNum),
	findRelevantClasses(Preconds,PredicateList,PrecondClasses),
	findRelevantClasses(Postconds,PredicateList,PostcondClasses),
	buildVars(PrecondsNoNum,PostcondsNoNum,[],VarsList),
	NewAxiomInfo = [ActionName,VarsList,Preconds,PrecondClasses,Postconds,PostcondClasses,Counter].



writeRules([],_).

writeRules([FirstAxiomInfo|OtherAxioms],ClassHierarchy) :-
	writeThisRule(FirstAxiomInfo,ClassHierarchy),
	writeRules(OtherAxioms,ClassHierarchy).

writeThisRule([ActionName,VarsList,Preconds,PrecondClasses,Postconds,PostcondClasses,Counter],ClassHierarchy) :-
	write('rule('),
	write(ActionName),
	write('('),
	writePrologVars(VarsList),
	write('),'),nl,
	write('     [rule'),
	write(Counter),
	write(','),nl,
	write('      ['),
	writeCondsClasses(PrecondClasses),
	writePreconds(Preconds,ClassHierarchy,NumberPseuds),
	write('],'),nl,
	write('      ['),
	writeCondsClasses(PostcondClasses),
	writePostconds(Postconds,ClassHierarchy,NumberPseuds),
	write(']'),nl,
	write('    ]).'),nl,nl.




writePrologVars([LastVar]) :-
	name('?',Que),
	matchExpression(_,Que,RealVar,LastVar),
	removeDashes(RealVar,NDRealVar,[]),
	name(LastVarName,NDRealVar),
	write(LastVarName).	

writePrologVars([FirstVar|Rest]) :-
	name('?',Que),
	matchExpression(_,Que,RealVar,FirstVar),
	removeDashes(RealVar,NDRealVar,[]),
	name(FirstVarName,NDRealVar),
	write(FirstVarName),
	write(','),
	writePrologVars(Rest).


writePreconds(Conds,ClassHierList,NumberPseuds) :-
	name('(And (',Begin),
	matchExpression([],Begin,GoodConds,Conds),
	removeLast(GoodConds,FullConds),
	writeGoodPreconds(FullConds,ClassHierList,0,NumberPseuds).

writeGoodPreconds(Conds,ClassHierList,NumberPseudsSoFar,NumberPseuds) :-
	name(') (',Ind),
	matchExpression(Predicate,Ind,OtherConds,Conds),
	writePrecondPredicate(Predicate,ClassHierList,NumberPseudsSoFar,NumberPseudsThisTime,notLast),
	NewNumberPseuds is NumberPseudsSoFar + NumberPseudsThisTime,
	writeGoodPreconds(OtherConds,ClassHierList,NewNumberPseuds,NumberPseuds).

writeGoodPreconds(Conds,ClassHierList,NumberPseudsSoFar,NumberPseuds) :-
	writePrecondPredicate(Conds,ClassHierList,NumberPseudsSoFar,NumberPseudsThisTime,last),
	NumberPseuds is NumberPseudsSoFar + NumberPseudsThisTime.


writePostconds(Conds,ClassHierList,NumberPseuds) :-
	name('(And (',Begin),
	matchExpression([],Begin,GoodConds,Conds),
	removeLast(GoodConds,FullConds),
	writeGoodPostconds(FullConds,ClassHierList,NumberPseuds).

writeGoodPostconds(Conds,ClassHierList,NumberPseuds) :-
	name(') (',Ind),
	matchExpression(Predicate,Ind,OtherConds,Conds),
	writePostcondPredicate(Predicate,ClassHierList,NumberPseuds,NewPseuds,notLast),
	NewNumberPseuds is NumberPseuds + NewPseuds,
	writeGoodPostconds(OtherConds,ClassHierList,NewNumberPseuds).

writeGoodPostconds(Conds,ClassHierList,NumberPseuds) :-
	writePostcondPredicate(Conds,ClassHierList,NumberPseuds,_,last).

writeCondsClasses([]).

writeCondsClasses(Classes) :-
	name('(',RightBracket),
	matchExpression(_,RightBracket,GoodClasses,Classes),
	writeClasses(GoodClasses).

writeClasses(Classes) :-
	checkFirstClassType(Classes,OtherClasses,FirstClassName,_),
	name('Number',FirstClassName),
	writeClasses(OtherClasses).

writeClasses(Classes) :-
	checkFirstClassType(Classes,OtherClasses,FirstClassName,_),
	name('Confirmation-Number',FirstClassName),
	writeClasses(OtherClasses).

writeClasses(Classes) :-
	checkFirstClassType(Classes,OtherClasses,[FirstNameLetter|RestName],[Que|[FirstObjectLetter|RestObject]]),
	NewFirstLetter is FirstNameLetter + 32,
	name('?',[Que]),
	write('class('),
	convertClassName(RestObject,GoodRestObject,[]),
	name(ObjectName,[FirstObjectLetter|GoodRestObject]),
	write(ObjectName),
	write(','),
	name(Class,[NewFirstLetter|RestName]),
	write(Class),
	write('),'),
	writeClasses(OtherClasses).

writeClasses(Classes) :-
	checkFirstClassType(Classes,OtherClasses,[FirstNameLetter|RestName],[FirstObjectLetter|RestObject]),
	NewFirstLetter is FirstNameLetter + 32,
	write('class('),
	convertClassName(RestObject,GoodRestObject,[]),
	name(ObjectName,[FirstObjectLetter|GoodRestObject]),
	write(ObjectName),
	write(','),
	name(Class,[NewFirstLetter|RestName]),
	write(Class),
	write('),'),
	writeClasses(OtherClasses).

writeClasses(LastClass) :-
	name(' ',Space),
	matchExpression([FirstLetter|RestName],Space,[Que|[FirstObjectLetter|RestObject]],LastClass),
	NewFirstLetter is FirstLetter + 32,
	name('?',[Que]),
	name(')',LeftBracket),
	matchExpression(RestBestObject,LeftBracket,_,RestObject),
	write('class('),
	convertClassName(RestBestObject,RestRealObject,[]),
	name(ObjectName,[FirstObjectLetter|RestRealObject]),
	write(ObjectName),
	write(','),
	name(Class,[NewFirstLetter|RestName]),
	write(Class),
	write('),').

writeClasses(LastClass) :-
	name(' ',Space),
	matchExpression([FirstLetter|RestName],Space,[FirstObjectLetter|RestObject],LastClass),
	NewFirstLetter is FirstLetter + 32,
	name(')',LeftBracket),
	matchExpression(RestBestObject,LeftBracket,_,RestObject),
	write('class('),
	convertClassName(RestBestObject,RestRealObject,[]),
	name(ObjectName,[FirstObjectLetter|RestRealObject]),
	write(ObjectName),
	write(','),
	name(Class,[NewFirstLetter|RestName]),
	write(Class),
	write('),').

	

checkFirstClassType(Classes,OtherClasses,FirstClassName,FirstClassObject) :-
	name(') (',MiddleSpace),
	name(' ',Space),
	matchExpression(FirstClass,MiddleSpace,OtherClasses,Classes),!,
	matchExpression(FirstClassName,Space,FirstClassObject,FirstClass),!.



writePrecondPredicate(Predicate,_,NumberPseuds,NumberPseuds,Place) :-
	name('= ',Equ),
	matchExpression(_,Equ,Rest,Predicate),
	name(' ',Space),
	matchExpression(ArgVal,Space,RestArgs,Rest),
	write('calculation('),
	name('?',Que),
	matchExpression(_,Que,GoodArg,ArgVal),
	convertCalcArgs(RestArgs,CalcArgs),
	convertVarsName(GoodArg,PrologArg),
	name(ArgName,PrologArg),
	write(ArgName),
	write(' is '),
	write(CalcArgs),
	writePunct(Place).


writePrecondPredicate(Predicate,_,NumberPseuds,NumberPseuds,Place) :-
	(   name('<',Operator),
	    matchExpression(_,Operator,Args,Predicate)
	;
	    name('>',Operator),
	    matchExpression(_,Operator,Args,Predicate)
	),
	name('?',Que),
	matchExpression(_,Que,GoodArgs,Args),
	matchExpression(FirstArg,Que,SecondArg,GoodArgs),
	name(' ',Space),
	append(FirstArg,Space,GoodFirst),
	append(GoodFirst,Operator,Begin),
	append(Begin,Space,FullBegin),
	append(FullBegin,SecondArg,FullExpression),
	name(FullName,FullExpression),
	write('calculation('),
	write(FullName),
	writePunct(Place).


writePrecondPredicate(Predicate,ClassHier,NumberPseuds,NumberSoFar,Place) :-
	name(' ',Space),
	name('Not',Not),
	matchExpression(Not,Space,Args,Predicate),
	write('not'),
	writePrecondPredicate(Args,ClassHier,NumberPseuds,NumberSoFar,Place).
	      
writePrecondPredicate(Predicate,ClassHierList,NumberPseuds,NumberPseuds,Place) :-
	checkClass(Predicate,ClassHierList,PredName,Args),
	convertName(PredName,PrologPred,[]),
	write('class('),
	name('?',Que),
	matchExpression(_,Que,GoodArg,Args),
	(   name(')',RightBracket),
	    matchExpression([FirstLetter|RestArg],RightBracket,_,GoodArg)
	;
	    GoodArg = [FirstLetter|RestArg]
	),
	convertName(RestArg,ConvArg,[]),
	name(ArgsName,[FirstLetter|ConvArg]),
	write(ArgsName),
	write(','),
	name(NamePred,PrologPred),
	write(NamePred),
	write(')'),
	writePunct(Place).

writePrecondPredicate(Predicate,_,NewNumberPseuds,NumberPseuds,Place) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Predicate),
	convertName(PredName,PrologPred,[]),
	name(NamePred,PrologPred),
	write(NamePred),
	write('('),
	writePredArgs(Args,NewNumberPseuds,NumberPseuds,Place).


writePostcondPredicate(Predicate,_,_,0,Place) :-
	name('= ',Equ),
	matchExpression(_,Equ,Rest,Predicate),
	name(' ',Space),
	matchExpression(ArgVal,Space,RestArgs,Rest),
	write('calculation('),
	name('?',Que),
	matchExpression(_,Que,GoodArg,ArgVal),
	convertCalcArgs(RestArgs,CalcArgs),
	convertVarsName(GoodArg,PrologArg),
	name(ArgName,PrologArg),
	write(ArgName),
	write(' is '),
	write(CalcArgs),
	writePunct(Place).


writePostcondPredicate(Predicate,_,_,0,Place) :-
	(   name('<',Operator),
	    matchExpression(_,Operator,Args,Predicate)
	;
	    name('>',Operator),
	    matchExpression(_,Operator,Args,Predicate)
	),
	name('?',Que),
	matchExpression(_,Que,GoodArgs,Args),
	matchExpression(FirstArg,Que,SecondArg,GoodArgs),
	name(' ',Space),
	append(FirstArg,Space,GoodFirst),
	append(GoodFirst,Operator,Begin),
	append(Begin,Space,FullBegin),
	append(FullBegin,SecondArg,FullExpression),
	name(FullName,FullExpression),
	write('calculation('),
	write(FullName),
	writePunct(Place).


writePostcondPredicate(Predicate,ClassHier,NumberPseuds,NewPseuds,Place) :-
	name(' ',Space),
	name('Not',Not),
	matchExpression(Not,Space,Args,Predicate),
	write('not'),
	writePostcondPredicate(Args,ClassHier,NumberPseuds,NewPseuds,Place).
	      
writePostcondPredicate(Predicate,ClassHierList,_,0,Place) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Predicate),
	member([PredName|_],ClassHierList),
	convertName(PredName,PrologPred,[]),
	write('class('),
	name('?',Que),
	matchExpression(_,Que,GoodArg,Args),
	name(')',RightBracket),
	matchExpression([FirstLetter|RestArg],RightBracket,_,GoodArg),
	convertName(RestArg,ConvArg,[]),
	name(ArgsName,[FirstLetter|ConvArg]),
	write(ArgsName),
	write(','),
	name(NamePred,PrologPred),
	write(NamePred),
	write(')'),
	writePunct(Place).

writePostcondPredicate(Predicate,_,NumberPseuds,NewPseuds,Place) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Predicate),
	informPred(PredName,NamePred),
	write('inform('),
	write(NamePred),
	write('('),
	writePredArgs(Args,inform,NumberPseuds,NewPseuds,Place).


writePostcondPredicate(Predicate,_,NumberPseuds,NewPseuds,Place) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Predicate),
	convertName(PredName,PrologPred,[]),
	name(NamePred,PrologPred),
	write(NamePred),
	write('('),
	writePredArgs(Args,NumberPseuds,NewPseuds,Place).


informPred(PredName,NamePred) :-
	convertName(PredName,PrologPred,[]),!,
	name(NamePred,PrologPred),
	inform(InformList),
	member(NamePred,InformList).


writePunct(notLast) :-
	write(',').

writePunct(last).




writePredArgs(Args,NewNumberPseuds,NumberPseuds,notLast) :-
	writeIndivArgs(Args,NewNumberPseuds,NumberPseuds),
	write('),').

writePredArgs(Args,NewNumberPseuds,NumberPseuds,last) :-
	writeIndivArgs(Args,NewNumberPseuds,NumberPseuds).

writePredArgs(Args,inform,NewNumberPseuds,NumberPseuds,notLast) :-
	writeIndivArgs(Args,NewNumberPseuds,NumberPseuds),
	write(')),').

writePredArgs(Args,inform,NewNumberPseuds,NumberPseuds,last) :-
	writeIndivArgs(Args,NewNumberPseuds,NumberPseuds),
	write(')').


writeIndivArgs(Args,NewNumberPseuds,NumberPseuds) :-
	name(' ',Space),
	matchExpression(FirstArg,Space,OtherArgs,Args),
	name('?',Que),
	matchExpression([],Que,GoodFirst,FirstArg),
	findPseudo(GoodFirst,NewNumberPseuds,Number,PseudoFirst),
	convertVarsName(PseudoFirst,PrologFirst),
	name(GoodName,PrologFirst),
	write(GoodName),
	write(','),
%	NewPseuds is NewNumberPseuds + Number,
%	write('new number pseuds is '),write(NewNumberPseuds),nl,
%	write('number is '),write(Number),nl,
	writeIndivArgs(OtherArgs,Number,NumberPseuds).


writeIndivArgs(Args,NewNumberPseuds,NumberPseuds) :-
	name(' ',Space),
	matchExpression(FirstArg,Space,OtherArgs,Args),
	findPseudo(FirstArg,NewNumberPseuds,Number,PseudoFirst),
	convertVarsName(PseudoFirst,PrologArg),
	name(GoodName,PrologArg),
	write(GoodName),
	write(','),
%	NewPseuds is NewNumberPseuds + Number,
%	write('new number pseuds is '),write(NewNumberPseuds),nl,
%	write('number is '),write(Number),nl,
	writeIndivArgs(OtherArgs,Number,NumberPseuds).


writeIndivArgs(Args,NewNumberPseuds,Number) :-
	name('?',Que),
	matchExpression([],Que,GoodFirst,Args),
	findPseudo(GoodFirst,NewNumberPseuds,Number,PseudoFirst),
	convertVarsName(PseudoFirst,PrologFirst),
	name(GoodName,PrologFirst),
	write(GoodName).
%	NumberPseuds is NewNumberPseuds + Number.


writeIndivArgs(Args,NewNumberPseuds,Number) :-
	findPseudo(Args,NewNumberPseuds,Number,PseudoFirst),
	convertVarsName(PseudoFirst,PrologArgs),
	name(GoodName,PrologArgs),
	write(GoodName).
%	NumberPseuds is NewNumberPseuds + Number.


findPseudo(Arg,NumberSoFar,NumberHere,NewArg) :-
	name('Pseudo-Var',PV),
	matchExpression(BeforePseudo,PV,AfterPseudo,Arg),
	spy,
	name(NumberSoFar,No),
	name('PseudoVar',BetterPV),
	append(BetterPV,No,NewPV),
	append(BeforePseudo,NewPV,NewBefore),
	append(NewBefore,AfterPseudo,BetterArg),
	NewNumberSoFar is NumberSoFar + 1,
	findPseudo(BetterArg,NewNumberSoFar,NumberHere,NewArg).

findPseudo(NewArg,NumberHere,NumberHere,NewArg).

spy.

convertVarsName(Var,NewVar) :-
	convertVarsName(Var,NewVar,[]).

convertVarsName([H|Rest],NewVar,[]) :-
	convertVarsName(Rest,NewVar,[H]).

convertVarsName([],RevVar,NewVar) :-
	reverse(RevVar,NewVar).

convertVarsName([Dash|Rest],NewVar,NewVarSoFar) :-
	name('-',[Dash]),
	convertVarsName(Rest,NewVar,NewVarSoFar).

convertVarsName([H|Rest],NewVar,NewVarSoFar) :-
	convertVarsName(Rest,NewVar,[H|NewVarSoFar]).



convertCalcArgs(Args,CalcArgs) :-
	name('(',LeftBracket),
	matchExpression(_,LeftBracket,GoodArgs,Args),
	name(' ',Space),
	% if the operator is plus, there may be nested arguments.
	name('+',Plus),
	matchExpression(Plus,Space,OtherArgs,GoodArgs),
	findPlusArgs(OtherArgs,PlusArgs),
	append(PlusArgs,Plus,PlusPlusArgs),
	name(' + +',SpacePlus),
	matchExpression(GoodPlusArgs,SpacePlus,_,PlusPlusArgs),
	name(')',RightBracket),
	append(GoodPlusArgs,RightBracket,FullPlusArgs),
	name(CalcArgs,FullPlusArgs).
	

convertCalcArgs(Args,CalcArgs) :-
	name('(',LeftBracket),
	matchExpression(_,LeftBracket,GoodArgs,Args),
	name(' ',Space),
	matchExpression(Operator,Space,OtherArgs,GoodArgs),
	name('?',SpQue),
	matchExpression(_,SpQue,RestArgs,OtherArgs),
	matchExpression(FirstArg,SpQue,SecondArg,RestArgs),
	append(Operator,Space,FullOperator),
	append(FirstArg,FullOperator,Begin),
	append(Begin,SecondArg,FullArgs),
	name(CalcArgs,FullArgs).


findPlusArgs(Args,PlusArgs) :-
	name('?',Que),
	matchExpression(_,Que,GoodArgs,Args),
	findPlusArgs(GoodArgs,PlusArgs,[]).

findPlusArgs(Args,PlusArgs,PlusArgsSoFar) :-
	name('?',Que),
	matchExpression(FirstArg,Que,OtherArgs,Args),
	convertVarsName(FirstArg,GoodFirstArg),
	name(' + ',Plus),
	append(GoodFirstArg,Plus,FullFirstArg),
	append(FullFirstArg,PlusArgsSoFar,NewPlus),
	findPlusArgs(OtherArgs,PlusArgs,NewPlus).

findPlusArgs(LastArg,PlusArgs,PlusArgsSoFar) :-
	name(')',LeftBracket),
	matchExpression(GoodLastArg,LeftBracket,_,LastArg),
	convertVarsName(GoodLastArg,ConvLastArg),
	name(' + ',Plus),
	append(ConvLastArg,Plus,FullLastArg),
	append(FullLastArg,PlusArgsSoFar,PlusArgs).

removeLast(List,NewList) :-
	reverse(List,[_|Rest]),
	reverse(Rest,NewList).


processPredicates([]) :-
	nl.

processPredicates([FirstPred|Rest]) :-
	processPred(FirstPred),
	processPredicates(Rest).


processPred([PredName|PredArgs]) :-
	convertName(PredName,PrologPred,[]),
	name(PrologPredName,PrologPred),
	write('predicate('),
	write(PrologPredName),
	write('('),
	processPredArgs(PredArgs).

processPredArgs(PredArgs) :-
	reverse(PredArgs,RevPredArgs),
	processRevPredArgs(RevPredArgs).

processRevPredArgs([LastArg]) :-
	findType(LastArg,Type),
	convertName(Type,PrologArg,[]),
	name(ArgName,PrologArg),
	write(ArgName),
	write(')).'),nl.

processRevPredArgs([FirstArg|Rest]) :-
	findType(FirstArg,Type),
	convertName(Type,PrologArg,[]),
	name(ArgName,PrologArg),
	write(ArgName),
	write(','),
	processRevPredArgs(Rest).


findType(Arg,Type) :-
	name('(',Start),
	name(' ',Space),
	matchExpression(Start,After,[],Arg),
	matchExpression(Type,Space,_Rest,After).



processClassHierarchy([]).

processClassHierarchy([[FirstClass|SuperClass]|Rest]) :-
	convertName(FirstClass,PrologClass,[]),
	convertName(SuperClass,PrologSuper,[]),
	name(FirstClassName,PrologClass),
	name(SuperName,PrologSuper),
	write('subclass('),
	write(FirstClassName),
	write(','),
	write(SuperName),
	write(').'),nl,
	processClassHierarchy(Rest).



removeDashes([],NonDashConds,NDSoFar) :-
	reverse(NonDashConds,NDSoFar).

removeDashes([Dash|[Next|Rest]],NonDashConds,NDSoFar) :-
	name('-',[Dash]),
	Next > 96,
	Next < 123,
	NewNext is Next - 32,
	removeDashes(Rest,NonDashConds,[NewNext|NDSoFar]).

removeDashes([Dash|[Space|Rest]],NonDashConds,NDSoFar) :-
	name('-',[Dash]),
	name('-',[Space]),
	removeDashes(Rest,NonDashConds,[Space|[Dash|NDSoFar]]).

removeDashes([Dash|Rest],NonDashConds,NDSoFar) :-
	name('-',[Dash]),
	removeDashes(Rest,NonDashConds,NDSoFar).

removeDashes([First|Rest],NonDashConds,NDSoFar) :-
	removeDashes(Rest,NonDashConds,[First|NDSoFar]).




findRelevantClasses(Conds,PredicateList,CondsClasses) :-
	name('(And',And),
	matchExpression(_,And,Args,Conds),
	findPreds(Args,PredsPresent,PredArgs,[],[]),
	addNewAtFront(PredsPresent,PredicateList,PredArgs,CondsClasses).
	


addNewAtFront(PredsPresent,PredicateList,PredArgs,CondsClasses) :-
	findArgsList(PredsPresent,PredicateList,ArgsList,[]),
	makeNewArgs(ArgsList,PredArgs,PredArgs,NewCondsClasses,[]),
	translateNewArgs(NewCondsClasses,CondsClasses).


translateNewArgs(NewConds,GoodConds) :-
	name('-',Dash),
	matchExpression(BeforeDash,Dash,AfterDash,NewConds),
	append(BeforeDash,AfterDash,BetterConds),
	translateNewArgs(BetterConds,GoodConds).

translateNewArgs(GoodConds,GoodConds).


processMetaInf :-
	nonFacts(NonFactList),
	myFacts(MyFactList),
	transitivePreds(TransPredList),
	write('nonFactsList('),
	write(NonFactList),
	write(').'),nl,nl,
	write('myFactsList('),
	write(MyFactList),
	write(').'),nl,nl,
	write('transitivePredsList('),
	write(TransPredList),
	write(').'),nl,nl.


checkClass(Predicate,ClassHierList,PredName,Args) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Predicate),!,
	member([PredName|_],ClassHierList).