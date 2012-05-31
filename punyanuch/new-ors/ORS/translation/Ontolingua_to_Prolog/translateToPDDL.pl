:- use_module(library(lists)),use_module(library(random)).

% ********************************************

% translateToPDDL

% this predicate reads a file called ont.in, which contains the KIF ontology, and writes two plan files, domainOnt.pddl (domain) and problemOnt.pddl (problem).

translateToPDDL(Goal,IndivList,ProbClassList,DomClassList,FactList,PredicateList,RuleList,NumericalPredicateList,ClassHierarchyList) :-
	tell('domainOnt.pddl'),
	buildDomainFile(PredicateList,DomClassList,RuleList,NumericalPredicateList,LargestPseud,FullPseudoPostconds),
	told,
	tell('problemOnt.pddl'),
	buildProblemFile(IndivList,ProbClassList,FactList,ClassHierarchyList,PredicateList,FullPseudoPostconds,Goal,LargestPseud),
%	write('full pseudo postconds are '),write(FullPseudoPostconds),nl,
	told.







% **************************************************************

% writing the problem file

% buildProblemFile(+IndivList,+ClassList,+FactList)
% this takes the three lists built up in buildProblemLists and uses them to write the problem file

buildProblemFile(IndivList,ClassList,FactList,ClassHierarchyList,PredicateList,PseudoPostconds,Goal,LargestPseud) :-
	write('(define (problem problemOnt)'),nl,
	write(' (:domain domainOnt)'),nl,
	write(' (:objects'),nl,
	processIndivList(IndivList,LargestPseud),
	write('   Meta-Var'),nl,
	write(' )'),nl,nl,
	write(' (:init'),nl,
	write('   (Meta-Vars Meta-Var)'),nl,
	writeMetaClass(PredicateList,FactList),
	processClassList(ClassList,ClassHierarchyList,LargestPseud),
	processFactList(FactList,PredicateList,ClassList,PseudoPostconds),
	write(' )'),nl,nl,
	write(' (:goal'),nl,nl,
	write('   '),
	write(Goal),nl,nl,
	write(' )'),nl,
	write(')'),nl.


% processIndivList(+IndivList)
% finds the name of each individual and writes it down

processIndivList([],LargestPseud) :-
	GoodPseud is LargestPseud - 1,
	writePseuds(GoodPseud).

processIndivList([FirstIndiv|Rest],LargestPseud) :-
	name(FirstIndivName,FirstIndiv),
	write('   '),
	write(FirstIndivName),nl,
	processIndivList(Rest,LargestPseud).



writeMetaClass(_,[]).

writeMetaClass(PredicateList,[FirstFact|RestFacts]) :-
	name('Meta-Var',MetaVar),
	matchExpression(EarlyPred,MetaVar,_,FirstFact),
	name(' ',Space),
	matchExpression(Pred,Space,RestPred,EarlyPred),
	name('(',RightBracket),
	matchExpression([],RightBracket,PredName,Pred),
	findMetaPosition(RestPred,1,Position),
	matchExpression(_,[[PredName|Args]|_],_,PredicateList),
	reverse(Args,RevArgs),
	findMetaArg(RevArgs,Position,MetaArg),
	matchExpression(ClassName,Space,_,MetaArg),
	matchExpression([],RightBracket,GoodClass,ClassName),
	name(GoodClassName,GoodClass),
	write('   ('),
	write(GoodClassName),
	write(' Meta-Var)'),nl,
	writeMetaClass(PredicateList,RestFacts).

writeMetaClass(PredicateList,[_FirstFact|RestFacts]) :-
	writeMetaClass(PredicateList,RestFacts).


findMetaPosition(RestPred,PosSoFar,Pos) :-
	name(' ',Space),
	matchExpression(_,Space,OtherArgs,RestPred),
	NewPos is PosSoFar + 1,
	findMetaPosition(OtherArgs,NewPos,Pos).

findMetaPosition(_RestPred,Pos,Pos).


findMetaArg([MetaArg|_],1,MetaArg).
	
findMetaArg([_|RestArgs],Pos,MetaArg) :-
	NewPos is Pos - 1,
	findMetaArg(RestArgs,NewPos,MetaArg).


% processClassList(+ClassList)
% finds the name of each individual and the class it belongs to, and writes it down in an appropriate fashion

processClassList([],_,LargestPseud) :-
	GoodPseud is LargestPseud - 1,
	writePseudClass(GoodPseud).

processClassList([[FirstName|FirstClass]|Rest],ClassHierarchyList,LargestPseud) :-
	member([FirstClass|SuperClass],ClassHierarchyList),
	writeClassList([FirstName|FirstClass]),
	processClassList([[FirstName|SuperClass]|Rest],ClassHierarchyList,LargestPseud).

processClassList([[FirstName|FirstClass]|Rest],ClassHierarchyList,LargestPseud) :-
	writeClassList([FirstName|FirstClass]),
	processClassList(Rest,ClassHierarchyList,LargestPseud).

% writeClassList(+ClassList)
% writes each object and its class

writeClassList([FirstName|FirstClass]) :-
	name(FirstNameName,FirstName),
	name(FirstClassName,FirstClass),
	write('   ('),
	write(FirstClassName),
	write(' '),
	write(FirstNameName),
	write(')'),nl.


% processFactList(+FactList)
% finds the name of each fact and writes it down

processFactList([],_,_,_).

processFactList([FirstFact|Rest],PredicateList,ClassList,PseudoPostconds) :-
	name('Meta-Var',Meta),
	matchExpression(BeforeMeta,Meta,AfterMeta,FirstFact),
	name(':Axioms',AxBegin),
	matchExpression(_,AxBegin,_,BeforeMeta),
	(
	    name('( = (',NumericalID),
	    matchExpression(_,NumericalID,AfterID,BeforeMeta)
	;
	    name('(',NonNumID),
	    matchExpression(_,NonNumID,AfterID,FirstFact)
        ),
	name(' ',Space),
	matchExpression(PredName,Space,BeforeArgs,AfterID),
	member([PredName|Args],PredicateList),
	findArgsPlace(BeforeArgs,Number,1),
	findElement(Args,Number,ClassDef),
	matchExpression(ClassName,Space,_,ClassDef),
	name('(',RightBracket),
	matchExpression(_,RightBracket,GoodClass,ClassName),
	member([ClassObject|GoodClass],ClassList),
	append(BeforeMeta,ClassObject,NewBegin),
	append(NewBegin,AfterMeta,NewFirstFact),
	name(FirstFactName,NewFirstFact),
	write('   '),
	write(FirstFactName),nl,
	processFactList(Rest,PredicateList,ClassList,PseudoPostconds).

processFactList([FirstFact|Rest],PredicateList,ClassList,PseudoPostconds) :-
	name('Meta-Var',Meta),
	matchExpression(BeforeMeta,Meta,AfterMeta,FirstFact),
	(
	    name('( = (',NumericalID),
	    matchExpression(_,NumericalID,AfterID,BeforeMeta)
	;
	    name('(',NonNumID),
	    matchExpression(_,NonNumID,AfterID,FirstFact)
        ),
	name(' ',Space),
	matchExpression(PredName,Space,BeforeArgs,AfterID),
	member([PredName|Args],PredicateList),
	reverse(Args,RevArgs),
	findRightArg(RevArgs,BeforeArgs,RightArg),
	name('(',BeginBracket),
	matchExpression(_,BeginBracket,GoodBegin,RightArg),
	matchExpression(GoodClass,Space,_BadArg,GoodBegin),
	member([ClassObject|GoodClass],ClassList),
	append(BeforeMeta,ClassObject,NewBegin),
	append(NewBegin,AfterMeta,NewFirstFact),
	name(FirstFactName,NewFirstFact),
	write('   '),
	write(FirstFactName),nl,
	processFactList(Rest,PredicateList,ClassList,PseudoPostconds).

processFactList([FirstFact|Rest],PredicateList,ClassList,PseudoPostconds) :-
	findPseudoFact(FirstFact,PseudoPostconds,NewFact),
	name(FirstFactName,NewFact),
	write('   '),
	write(FirstFactName),nl,
	processFactList(Rest,PredicateList,ClassList,PseudoPostconds).

processFactList([FirstFact|Rest],PredicateList,ClassList,PseudoPostconds) :-
	name(FirstFactName,FirstFact),
	write('   '),
	write(FirstFactName),nl,
	processFactList(Rest,PredicateList,ClassList,PseudoPostconds).

findRightArg([_|RestPredArgs],BeforeArgs,RightArg) :-
	name(' ',Space),
	matchExpression(_,Space,NewBefore,BeforeArgs),
	findRightArg(RestPredArgs,NewBefore,RightArg).

findRightArg([FirstPredArg|[LastArg]],_BeforeArgs,LastArg) :-
	name('(Number',No),
	matchExpression(_,No,_,FirstPredArg).

findRightArg([FirstPredArg|_],_BeforeArgs,FirstPredArg).
	

%findRightArg(PredName,PredicateList,AfterID,RightArg) :-
%	name(')',EndBracket),
%	matchExpression(WrongArg,EndBracket,_,AfterID),
%	removeWrongArg(WrongArg,Args,RightArg).

%removeWrongArg(WrongArg,[FirstArg|SecondArg],FirstArg) :-
%	sulist(WrongArg,SecondArg).

%removeWrongArg(WrongArg,[_|[SecondArg]],SecondArg).
	

% findPseudoFact(+Fact,+PseudoPostconds,-NewFact)
% sometimes, we will have numerical facts where we wish to consider the number to be a pseudo-var.  therefore, the numerical value is removed and it is replaced with the appropriatly numbered pseudo-var.  we determine what the appropriate pseudo-var is - it must be in line with the axioms - from the list of pseudo postconditions, which is built up during the processes of the domain file.

findPseudoFact(Fact,PseudoPostconds,NewFact) :-
	name('( = (',NumericalID),
	matchExpression(_,NumericalID,AfterID,Fact),
	name(' ',Space),
	matchExpression(FactName,Space,FactArgs,AfterID),!,
	matchExpression(_,FactName,AfterFact,PseudoPostconds),
	name('(',BeginBracket),
	name(')',EndBracket),
	matchExpression(RestFact,EndBracket,_,AfterFact),!,
	name('PseudoVar',Pseudo),
	matchExpression(_,Pseudo,[PseudoNo|_],RestFact),
	% WARNING! this assumes that pseudo IDs can only be single figures
	replaceNumerical(FactArgs,PseudoNo,NewFactArgs),
	append(Space,NewFactArgs,GoodArgs),
	append(BeginBracket,FactName,GoodFactName),
	append(GoodFactName,GoodArgs,NewFact).

% sometimes this applies even when we have non-numerical facts: this applies to anything that can be uninstantiated.  this implies that a pseudo-var will be used in the rule, and thus a pseudo-var must be used in the fact otherwise it will not match.  the information about what the actual value is is lost, but this does not matter, as it is retained in the KIF ont and the Prolog ont.

findPseudoFact(Fact,PseudoPostconds,NewFact) :-
	name(' ',Space),
	matchExpression(PredName,Space,PredArgs,Fact),
	findPseudoArgs(PredName,PseudoPostconds,PseudoPredArgs),
	% is there really a pseudo there, or does this just come along as part of the other postconds?
	matchExpression([],Space,RealPseudoArgs,PseudoPredArgs),
	replacePseudoArgs(PredArgs,RealPseudoArgs,[],NewPredArgs),
	rebuildPseudoFact(PredName,NewPredArgs,NewFact).


findPseudoFact(Fact,_,Fact).


findPseudoArgs(PredName,PseudoPostconds,PseudoPredArgs) :-
	matchExpression(_,PredName,PseudoPredArgsAndAfter,PseudoPostconds),!,
	% now that we have determined we need a pseudo, we need to find out how to replace it
	name(')',EndBracket),
	matchExpression(PseudoPredArgs,EndBracket,_After,PseudoPredArgsAndAfter),!,
	name('Pseudo',Pseudo),
	matchExpression(_,Pseudo,_,PseudoPredArgs),!.


replacePseudoArgs(PredArgs,PseudoPredArgs,NewPredArgsSoFar,NewPredArgs) :-
	% first, we need to find out if the first pseudoarg is a variable
	name('?',Que),
	matchExpression([],Que,MostPseudoArgs,PseudoPredArgs),
	% if so, we ignore the variable and keep the real pred arg
	name(' ',Space),
	matchExpression(_FirstVar,Space,RestPseudoArgs,MostPseudoArgs),
	matchExpression(FirstRealArg,Space,RestRealArgs,PredArgs),
	replacePseudoArgs(RestRealArgs,RestPseudoArgs,[FirstRealArg|NewPredArgsSoFar],NewPredArgs).

replacePseudoArgs(PredArgs,PseudoPredArgs,NewPredArgsSoFar,[FirstRealArg|NewPredArgsSoFar]) :-
	% first, we need to find out if the first pseudoarg is a variable
	name('?',Que),
	matchExpression([],Que,MostPseudoArgs,PseudoPredArgs),
	% if so, we ignore the variable and keep the real pred arg
	name(')',EndBracket),
	matchExpression(_FirstVar,EndBracket,_RestPseudoArgs,MostPseudoArgs),
	matchExpression(FirstRealArg,EndBracket,_RestRealArgs,PredArgs).

replacePseudoArgs(PredArgs,PseudoPredArgs,NewPredArgsSoFar,NewPredArgs) :-
	% if the first arg is not a variable, it's a pseudo-arg - in which case, that's the one we want to keep
	name(' ',Space),
	matchExpression(_PseudoVar,Space,RestPseudoArgs,PseudoPredArgs),
	matchExpression(_FirstRealArg,Space,RestRealArgs,PredArgs),
	replacePseudoArgs(RestRealArgs,RestPseudoArgs,[PseudoPredArgs|NewPredArgsSoFar],NewPredArgs).

replacePseudoArgs(PredArgs,PseudoPredArgs,NewPredArgsSoFar,[PseudoVar|NewPredArgsSoFar]) :-
	% if the first arg is not a variable, it's a pseudo-arg - in which case, that's the one we want to keep
	name(')',EndBracket),
	matchExpression(_FirstRealArg,EndBracket,_RestRealArgs,PredArgs),
	append(PseudoPredArgs,EndBracket,PseudoVar).


rebuildPseudoFact(PredName,NewPredArgs,NewFact) :-
	rewritePseudoArgs(NewPredArgs,[],GoodArgs),
	append(PredName,GoodArgs,NewFact).

rewritePseudoArgs([],GoodArgs,GoodArgs).

rewritePseudoArgs([FirstArg|Rest],GoodArgsSoFar,GoodArgs) :-
	name(' ',Space),
	append(Space,FirstArg,GoodBegin),
	append(GoodBegin,GoodArgsSoFar,NewArgsSoFar),
	rewritePseudoArgs(Rest,NewArgsSoFar,GoodArgs).




% replaceNumerical(+FactArgs,+PseudoNo,-NewFactArgs)
% replaces the numerical arg with the appropriate pseudo-var

replaceNumerical(FactArgs,PseudoNo,NewFactArgs) :-
	name('PseudoVar',Pseudo),
	append(Pseudo,[PseudoNo],FullPseudo),
	findNumber(FactArgs,[],BeforeNumber,AfterNumber),
	name(')',EndBracket),
	matchExpression(GoodBefore,EndBracket,Space,BeforeNumber),
	append(GoodBefore,Space,BestBefore),
	append(BestBefore,FullPseudo,NewBefore),
	append(NewBefore,AfterNumber,NewFactArgs).

% findNumber(+FactArgs,+BeginSoFar,-BeginNumber,-AfterNumber)
% returns what comes before and after the number in fact args (i.e. returns the non-numerical args	

findNumber([H|RestFact],BeginSoFar,BeginNumber,AfterNumber) :-
	H > 47,
	H < 58,
	findRestNumber(RestFact,AfterNumber),
	reverse(BeginSoFar,BeginNumber).

findNumber([H|RestFact],BeginSoFar,BeginNumber,AfterNumber) :-
	findNumber(RestFact,[H|BeginSoFar],BeginNumber,AfterNumber).

% findRestNumber(+RestFact,-AfterNumber)
% once the number has been found, returns what comes after the number

findRestNumber([H|Rest],AfterNumber) :-
	H > 47,
	H < 58,
	findRestNumber(Rest,AfterNumber).

findRestNumber(Rest,Rest).

% findArgsPlace(+BeforeArgs,-Number,+NumSoFar)
% returns a number which indicates the position of the argument

findArgsPlace(BeforeArgs,Number,NumSoFar) :-
	name(' ',Space),
	matchExpression(_,Space,RestArgs,BeforeArgs),
	NewNum is NumSoFar + 1,
	findArgsPlace(RestArgs,Number,NewNum).

findArgsPlace([],Number,Number).

findArgsPlace(_,Number,NumSoFar) :-
	Number is NumSoFar + 1.

% findElement(+List,+Number,-Element)
% returns the nth element of a list

findElement([H|_],1,H).

findElement([_|T],Number,Element) :-
	NewNum is Number - 1,
	findElement(T,NewNum,Element).

% writePseuds(+Number)
% declares all the pseudo-vars as individuals by declaring a pseudo-var with a numerical identifier until sufficient are developed.

writePseuds(-1).

writePseuds(LargestPseud) :-
	name('PseudoVar',PseudoVar),
	name(LargestPseud,PseudChars),
	append(PseudoVar,PseudChars,FullPseud),
	write('   '),
	name(PseudName,FullPseud),
	write(PseudName),nl,
	NewPseud is LargestPseud - 1,
	writePseuds(NewPseud).

% writePseudoClass(+Number)
% declares all the pseudo-var individuals as of class confirmation-number

writePseudClass(-1).

writePseudClass(LargestPseud) :-
	name('PseudoVar',PseudoVar),
	name(LargestPseud,PseudChars),
	append(PseudoVar,PseudChars,FullPseud),
	name(PseudName,FullPseud),
	write('   '),
	write('('),
	write('Confirmation-Number'),
	write(' '),
	write(PseudName),
	write(')'),nl,
	write('   '),
	write('('),
	write('Thing'),
	write(' '),
	write(PseudName),
	write(')'),nl,
	NewPseud is LargestPseud - 1,
	writePseudClass(NewPseud).
	     


% ****************************************************************

% writing the domain file


% buildDomainFile(+PredicateList,+ClassList,+RuleList,+NumPredList)
% writes the domain file uses the lists that have been generated for this

buildDomainFile(PredicateList,ClassList,RuleList,NumPredList,LargestPseud,FullPseudoPostconds) :-
	write('(define (domain domainOnt)'),nl,
	write('  (:requirements :strips :fluents :typing)'),nl,nl,
	write('  (:predicates'),nl,nl,
	processPredicates(PredicateList,ClassList,NumPredList),
	write('  (Meta-Vars ?Meta-Var)'),nl,
	write('  )'),nl,nl,
	write('  (:functions'),nl,nl,
	processFunctions(NumPredList),nl,
	write('  )'),nl,nl,
	processAxioms(RuleList,NumPredList,PredicateList,TotalPseuds,[],[],[],FullPseudoPostconds),
	largestElement(TotalPseuds,0,LargestPseud),
	write(')').

% processPredicates(+PreciateList)
% rewrites the preciates into an appropriate form and writes them to the file

processPredicates(PredicateList,ClassList,FunctionList) :-
	flatten(FunctionList,FlatFunctions),
	processPredicates(PredicateList,[],ArgsClassList,FlatFunctions),nl,
	stripDuplicates(ArgsClassList,NewClassList,[]),
	writeArgsClassList(NewClassList),
%	write('args class list is '),write(ArgsClassList),nl,
%	write('class list is '),write(ClassList),nl,
%	write('new class list is '),write(NewClassList),nl,
	processClasses(ClassList,NewClassList).

% processPredicates(+PredicateList,+ArgsListSoFar,-ArgsList)
% writes each predicate out by writing the predicate name, arranging the arguments appropriately and then writing them.  but if this predicate is a member of the numPredList (i.e. is a function) then ignore - these are written elsewhere.

processPredicates([],ArgsClassList,ArgsClassList,_).

%processPredicates([[FirstPredName|_]|Rest],ArgsSoFar,ArgsList,FlatFunctions) :-
%	sublist(FirstPredName,FlatFunctions),
%	processPredicates(Rest,ArgsSoFar,ArgsList,FlatFunctions).

processPredicates([[FirstPredName|ArgumentList]|Rest],ArgsSoFar,ArgsList,NumPredList) :-
	name(FirstPredNameName,FirstPredName),
	write('  ('),write(FirstPredNameName),write(' '),
	writeArguments(ArgumentList,ArgsClassList,[]),
	append(ArgsSoFar,ArgsClassList,NewArgs),
	processPredicates(Rest,NewArgs,ArgsList,NumPredList).

% writeArguments(+ArgumentList,+ArgsSoFar,-ArgsClassList)
% find the name of all the arguments and writes them to the file

writeArguments([Last],[Last|ArgsClassList],ArgsClassList) :-
	name(' ',ArgMarker),
	matchExpression(_,ArgMarker,Argument,Last),
	name(ArgumentName,Argument),
	write(ArgumentName),nl.

writeArguments([FirstArg|Rest],ArgsClassList,ArgsSoFar) :-
	name(' ',ArgMarker),
	matchExpression(_,ArgMarker,Argument,FirstArg),
	name(')',LeftBracket),
	matchExpression(GoodArg,LeftBracket,[],Argument),
	name(ArgumentName,GoodArg),
	write(ArgumentName),
	write(' '),
	writeArguments(Rest,ArgsClassList,[FirstArg|ArgsSoFar]).

% stripDuplicates(+ArgsClassList,-NewArgsClassList,+NewListSoFar)
% returns a list that contains no repeated predicates.

stripDuplicates([],NewArgsClassList,NewArgsClassList).

stripDuplicates([FirstArgsClass|Rest],NewArgsClassList,NewListSoFar) :-
	checkDuplicates(FirstArgsClass,NewListSoFar),
	stripDuplicates(Rest,NewArgsClassList,NewListSoFar).

stripDuplicates([FirstArgsClass|Rest],NewArgsClassList,NewListSoFar) :-
	stripDuplicates(Rest,NewArgsClassList,[FirstArgsClass|NewListSoFar]).

% checkDuplicates(+ArgsClass,+ClassList)
% succeeds if the predicate name of the argument already appears in ClassList, otherwise fails

checkDuplicates(ArgsClass,ClassList) :-
	name(' ',Space),
	matchExpression(PredName,Space,_,ArgsClass),!,
	flatten(ClassList,FlatClassList),!,
	matchExpression(_,PredName,_,FlatClassList).
%	sublist(PredName,FlatClassList).

checkDuplicates(_,_) :-
	fail,!.


% writeArgsClassList(+ArgsClassList)
% recurses through the argsclasslist, writing down each member on a new line


writeArgsClassList(ArgsClassList) :-
	name('Thing',Thing),nl,
	flatten(ArgsClassList,FlatList),
	sublist(FlatList,Thing,_,_,_),
	writeArgsClassListRec(ArgsClassList).

writeArgsClassList(ArgsClassList) :-
	write('  (Thing ?Thing)'),
	writeArgsClassListRec(ArgsClassList).

writeArgsClassListRec([]). 

writeArgsClassListRec([FirstArg|Rest]) :-
	name(FirstArgName,FirstArg),
	write('  '),
	write(FirstArgName),nl,
	writeArgsClassListRec(Rest).

% processClasses(+ClassList)
% writes down each class

processClasses([],_).

processClasses([FirstClass|Rest],NewClassList) :-
	duplicatedClass(FirstClass,NewClassList),
	processClasses(Rest,NewClassList).

processClasses([FirstClass|Rest],NewClassList) :-
	name(FirstClassName,FirstClass),
	write('  ('),
	write(FirstClassName),
	write(' ?'),
	write(FirstClassName),
	write(')'),nl,
	processClasses(Rest,NewClassList).

% duplicatedClass(+Class,+ClassList)
% succeeds if Class already appears in ClassList, otherwise fails

duplicatedClass(Class,ClassList) :-
	flatten(ClassList,FlatList),!,
	matchExpression(_,Class,_,FlatList).


% processFunctions(+FunctionList)
% writes down the functions

processFunctions([]).

processFunctions([FirstFunction|Rest]) :-
%	addVariables(FirstFunction,FirstFuncVars),
	name(FirstFuncName,FirstFunction),
	write('  '),
	write(FirstFuncName),nl,
	processFunctions(Rest).

% addVariables(+Function,-NewFunction)
% since these functions come from specific instances, they are instantiated.  these constants must be turned into variables.  since they are vars, the name does not matter; therefore, the name of the constant is used and a question mark added to turn it into a variable

addVariables(Function,NewFunction) :-
	name(' ',Space),
	matchExpression(Name,Space,Args,Function),
	name(')',RightBracket),
	matchExpression(GoodArgs,RightBracket,_,Args),
	argsToVars(GoodArgs,VarArgs,[]),
	append(Name,VarArgs,GoodFunction),
	append(GoodFunction,RightBracket,NewFunction).

% argsToVars(+Args,-VarArgs,+ArgsSoFar)
% adds a question mark to each variable

argsToVars(Args,VarArgs,ArgsSoFar) :-
	name(' ',Space),
	matchExpression(FirstArg,Space,RestArgs,Args),
	name(' ?',Que),
	append(Que,FirstArg,QueArg),
	append(QueArg,ArgsSoFar,NewArgsSoFar),
	argsToVars(RestArgs,VarArgs,NewArgsSoFar).

argsToVars(LastArg,VarArgs,ArgsSoFar) :-
	name(' ?',Que),
	append(Que,LastArg,FullLastArg),
	append(FullLastArg,ArgsSoFar,VarArgs).
			
% processAxioms(+AxiomList,+NumPredList,+PredicateList)
% processes and writes the axioms to the file, taking into account which are numerical.  this is a complicated procedure because the numerical arguments are represented in a very different way in PDDL than in KIF.  the predicate list is required because this gives the type of each argument.
% for each axiom, this rewrites the preconds and the postconds appropriately, takes out the variables from both of these and then writes the processed pre- and post- conds and the variable list to the file.

processAxioms([],_,_,TotalPseuds,TotalPseuds,PseudoPreds,AxiomInfo,FullPseudoPostconds) :-
	writeAxioms(PseudoPreds,AxiomInfo,[],FullPseudoPostconds).

processAxioms([[FirstAx|FullList]|Rest],NumPredList,PredicateList,TotalPseuds,TotalPseudsSoFar,PseudoPreds,AxiomInfoSoFar,FullPseudoPostconds) :-
	FullList = [_|PreAndPostcondsList],
	PreAndPostcondsList = [Preconds|Postconds],
	rewriteNumPreconds(Preconds,NumPredList,PrecondMarkers,PrecondsNoNum,TotalPseudos,NewPseudoPreds),
	rewriteNumPostconds(Postconds,NumPredList,PrecondMarkers,PostcondsNoNum),
	addTypeInfo(PrecondsNoNum,PostcondsNoNum,PredicateList,TypedPreconds),
	buildVars(PrecondsNoNum,PostcondsNoNum,[],VarsList),
	append(NewPseudoPreds,PseudoPreds,FullPseudoPreds),
%	append([FirstAxName,VarsList,TypedPreconds,TypedPostconds],AxiomInfoSoFar,FurtherAxiomInfo),
	processAxioms(Rest,NumPredList,PredicateList,TotalPseuds,[TotalPseudos|TotalPseudsSoFar],FullPseudoPreds,[[FirstAx,VarsList,TypedPreconds,PostcondsNoNum]|AxiomInfoSoFar],FullPseudoPostconds).

% writeAxioms(+PseudoPreds,+Axioms,+PostcondsSoFar,-PseudoPostconds)
% writes out the axioms and builds up a list of the pseudo postconds, which is needed when writing the problem file

writeAxioms(_,[],FullPseudoPostconds,FullPseudoPostconds).

writeAxioms(PseudoPreds,[HeadAxiom|Rest],CorrectPostcondsSoFar,FullPseudoPostconds) :-
	HeadAxiom = [AxiomName,VarsList,Preconds,Postconds],
	write(' (:action '),
	name(NameAx,AxiomName),
	write(NameAx),nl,
	write('   :parameters ('),
	writeVars(VarsList),
	write(')'),nl,
	write('   :precondition '),
	name(PrecondsName,Preconds),
	write(PrecondsName),nl,
	write('   :effect '),
	correctPseudos(PseudoPreds,Postconds,CorrectPostconds),
	name(PostcondsName,CorrectPostconds),
	write(PostcondsName),nl,
	write('  )'),nl,nl,
	append(CorrectPostcondsSoFar,CorrectPostconds,NewSoFar),
	writeAxioms(PseudoPreds,Rest,NewSoFar,FullPseudoPostconds).



% buildVars(+Preconds,+Postconds,+VarsSoFar,-Vars)
% appends pre- and post- conds into fullConds and finds all the variables in this

buildVars(Preconds,Postconds,VarsSoFar,Vars) :-
	append(Preconds,Postconds,FullConds),
	buildVars(FullConds,VarsSoFar,Vars).

% buildVars(+FullConds,+VarsSoFar,-Vars)
% finds all the variables in fullConds.  when the list is complete, duplicates are removed from it

buildVars(FullConds,VarsSoFar,Vars) :-
	name('?',VarId),
	matchExpression(BeforeVar,VarId,AfterVar,FullConds),
	name(' ',Space),
	matchExpression(Var,Space,RestVar,AfterVar),
	name(')',RightBracket),
	(   matchExpression(GoodVar,RightBracket,_,Var)
	;
	    GoodVar = Var
	),
	append(VarId,GoodVar,WholeVar),
	append(BeforeVar,RestVar,NewConds),
	buildVars(NewConds,[WholeVar|VarsSoFar],Vars).


buildVars(FullConds,VarsSoFar,Vars) :-
	name('?',VarId),
	matchExpression(BeforeVar,VarId,AfterVar,FullConds),
	name('))',EndBracket),
	matchExpression(Var,EndBracket,RestVar,AfterVar),
	append(VarId,Var,WholeVar),
	append(BeforeVar,RestVar,NewConds),
	buildVars(NewConds,[WholeVar|VarsSoFar],Vars).


buildVars(_,VarsSoFar,Vars) :-
	remove_dups(VarsSoFar,Vars).


% writeVars(+VarsList)
% writes each member of the variables list with a space between

writeVars([]).

writeVars([FirstVar|Rest]) :-
	name(FirstVarName,FirstVar),
	write(FirstVarName),
	write(' '),
	writeVars(Rest).


% rewriteNumPreconds(+Preconds,+NumPredList,-PrecondsNoNum)
% this predicate returns the list of preconds with all the numerical arguments removed and all the arithmetic rearranged in a way that is acceptable for PDDL.

rewriteNumPreconds(Conds,NumPredList,Markers,RelevantConds,NumberPseudos,PseudoPreds) :-
	% first, we build up a list of all the numerical arguments in this axiom:
	checkNumConds(Conds,NumPredList,Markers,[]),
	% markers is a list of all the numerical predicates contained in this axiom (with the exact arguments required by this axiom), together with their indicators
	% now we need to replace all the occurences of them, and then deal with the arithmetic.  everytime a marker is found, remove it and a space before it
	updateArith(Markers,NewMarkers,[],Conds,ArithConds),
	removeNumPreds(ArithConds,NewMarkers,NoNumConds),
	updateSums(NoNumConds,GoodSumConds),
	numberedPseudos(GoodSumConds,RelevantConds,0,NumberPseudos,PseudoPreds,[]).


% rewriteNumPostconds(+Postconds,+NumPredList,-PostcondsNoNum)
% as for preconds, except it uses the markers from the preconds as well as from the postconds, as precond variables can 'carry over' into the postconds.

rewriteNumPostconds(Conds,NumPredList,PrecondMarkers,RelevantConds) :-
	% first, we build up a list of all the numerical arguments in this axiom:
	checkNumConds(Conds,NumPredList,Markers,[]),
	append(PrecondMarkers,Markers,FullMarkers),
	% markers is a list of all the numerical predicates contained in this axiom (with the exact arguments required by this axiom), together with their indicators
	% now we need to replace all the occurences of them, and then deal with the arithmetic.  everytime a marker is found, remove it and a space before it
	updateArith(FullMarkers,NewMarkers,[],Conds,ArithConds),
	removeNumPreds(ArithConds,NewMarkers,NoNumConds),
	updateSums(NoNumConds,RelevantConds).


% checkNumConds(+Preconds,+NumericalPreds,-Markers,+MarkersSoFar)
% takes the preconds and a list of all the numerical predicates and searches through the preconds to find numerical predicates.  For every existing numerical predicate in the preconds, it finds the numerical argument identifier and appends that to the numerical predicate less the numerical argument to create the marker for the pred.  The list of all of these markers is returned in the Markers argument.

checkNumConds(_,[],Markers,Markers).
% if we have checked all the numerical predicates, then the markers list is complete

checkNumConds(Preconds,[FirstNum|Rest],Markers,MarkersSoFar) :-
	% first, find the name of the first numerical predicate:
	name(' ',Space),
	matchExpression(NameArg,Space,_,FirstNum),
	% now, check to see if this predicate is in the preconditions.  A new marker is returned, which is either a completer marker or, if this pred is not present, it is the empty list
	checkThisNum(Preconds,NameArg,FirstNum,NewMarker),
	% build up the markers list and recurse
	NewMarkersSoFar = [NewMarker|MarkersSoFar], 
	checkNumConds(Preconds,Rest,Markers,NewMarkersSoFar).

% checkThisNum(+Preconds,+NameArg,+NumericalPred,-Marker)
% checks to see if a particular numercial predicate is present in the preconditions, and, if so, builds up an appropriate marker for it.  If not, the marker is an empty list.

checkThisNum(Preconds,NameArg,FirstNum,Marker) :-
	% is the first numerical argument contained in preconds?
	matchExpression(_,NameArg,After,Preconds),
	% if so, we will find everything after the name of the predicate.  we find the rest of the arguments of the predicate by looking for the end bracket of the predicate: either this is ended with a bracket and a space if it is not the final argument or, if it is, by a double bracket.
	name(') ',End1),
	name('))',End2),
	(   matchExpression(VarArgs,End1,_,After)
	;
	    matchExpression(VarArgs,End2,_,After)
	),
	% now, VarArgs contains the arguments for the numerical predicate, one of which will be the numerical argument.  we then find ??
	findExactArgs(VarArgs,FirstNum,GoodArgs,Indicator),
	append(Indicator,GoodArgs,Marker).

checkThisNum(_,_,_,[]).
% if the name check fails, an empty list is returned.

% findExactArgs(+VarArgs,+NumericalPredicate,-NonNumericalArgs,-NumericalArg)
% takes the arguments of a numerical predicate from an axiom and compares this to the definition of this numerical predicate.  In this way, the name of the numerical arg in this instance is located (NumericalArg), together with the non-numerical args.

findExactArgs(VarArgs,FirstNum,GoodArgs,Marker) :-
	% first, remove the pred name from FirstNum
	name(' ',Space),
	matchExpression(PredName,Space,FirstVars,FirstNum),
	% next, remove the end bracket
	name(')',LeftBracket),
	matchExpression(GoodVars,LeftBracket,_,FirstVars),
	% remove the space from the beginning of varArgs
	matchExpression([],Space,RestArgs,VarArgs),
	% now, we have the same args twice: first as they appear in the axiom (RestArgs) then as they are defined (GoodVars).  we use these to find the name of the numerical arg in this case, and the others.
	findExactArgsRec(RestArgs,GoodVars,[],ExactArgs,Marker),
	% now rebuild
	append(ExactArgs,LeftBracket,ExactEnd),
	append(PredName,Space,ExactStart),
	append(ExactStart,ExactEnd,GoodArgs).

% findExactArgsRec(+VarArgs,+NumericalPredicate,+NonNumArgsSoFar,-FinalNumArgs,-Marker)
% this takes the arguments first as they appear in the axiom, then as they appear in the definition, and builds up a list of the non-numerical args and finds the numerical arg.
% ** we assume that the numerical arg is the last arg.  this will usually work due to the way that kif is defined, but maybe this should be made a bit more robust

findExactArgsRec(VarArgs,FirstNum,GoodArgs,FinalArgs,Marker) :-
	% first, we find the non-last args of VarArgs (as found in the axiom)
	name(' ',Space),
	matchExpression(FirstArg,Space,RestArgs,VarArgs),
	% now, we add this to good args and keep going
	% we reverse first arg and add it backwards, so that we can reverse the whole list at the end:
	reverse(FirstArg,RevFirstArg),
	append(Space,RevFirstArg,FullFirstArg),
	append(FullFirstArg,GoodArgs,NewGoodArgs),
	findExactArgsRec(RestArgs,FirstNum,NewGoodArgs,FinalArgs,Marker).

findExactArgsRec(Marker,_,GoodArgs,FinalArgs,Marker) :-
	% if there is no space, then VarArgs must just be the numerical arg.
	reverse(GoodArgs,FinalArgs).


% updateArith(+Markers,+CondsSoFar,-ArithConds,-RelevantMarkers,+RelMarkersSoFar)
% takes the list of makers and the conditions.  where there is an arithmetical operator, it changes the arugments as necessary and replaces the arithmetic indicator as appropriate.  this change is always done even if it is not necessary - if not nec, a place holder is added and converted back after the recursion to prevent looping.  a list of all the predicates involved is returned as relevant markers.

updateArith(Markers,NewMarkers,NewMarkersSoFar,CondsSoFar,ArithConds) :-
	% check for the particular arithmetical operator
	name('(-',Minus),
	% find the arguments before this and everything that comes after
	matchExpression(BeforeLess,Minus,AfterLess,CondsSoFar),
	% find which of the stuff that comes after is part of the predicate
	name(')',LeftBracket),
	matchExpression(NumArg,LeftBracket,RestArgs,AfterLess),
	% update this predicate appropriately and return which predicates are involved
	updateArithArg(NumArg,Markers,[],NewArg),
	% build it up again
	append(NewArg,LeftBracket,NewBit),
	append(NewBit,RestArgs,NewAfter),
	% add the new arithmetical name
	name('(decrease',Decrease),
	append(BeforeLess,Decrease,FirstBit),
	append(FirstBit,NewAfter,NewCondsSoFar),
	% recurse
	updateArith(Markers,NewMarkers,NewMarkersSoFar,NewCondsSoFar,ArithConds).

updateArith(Markers,NewMarkers,NewMarkersSoFar,CondsSoFar,ArithConds) :-
	name('(+',Plus),
	matchExpression(BeforeLess,Plus,AfterLess,CondsSoFar),
	name(')',LeftBracket),
	matchExpression(NumArg,LeftBracket,RestArgs,AfterLess),
	updateArithArg(NumArg,Markers,[],NewArg),
	append(NewArg,LeftBracket,NewBit),
	append(NewBit,RestArgs,NewAfter),
	name('(increase',Increase),
	append(BeforeLess,Increase,FirstBit),
	append(FirstBit,NewAfter,NewCondsSoFar),
	updateArith(Markers,NewMarkers,NewMarkersSoFar,NewCondsSoFar,ArithConds).

updateArith(Markers,NewMarkers,NewMarkersSoFar,CondsSoFar,ArithConds) :-
	name('(= ?',VarEquId),
	matchExpression(BeforeVarEquId,VarEquId,AfterVarEquId,CondsSoFar),
	name(' ',Space),
	matchExpression(EquID,Space,RestVar,AfterVarEquId),
	% is this a member of markers?
	(   memberMarkers(EquID,Markers),
	    name('))',DoubleLeft),
	    matchExpression(BeforeDouble,DoubleLeft,AfterDouble,RestVar),
	    name(')',SingleLeft),
	    append(BeforeDouble,SingleLeft,NewBefore),
	    append(NewBefore,AfterDouble,NewRestVar),
	    append(BeforeVarEquId,NewRestVar,NewCondsSoFar),
	    updateArith(Markers,NewMarkers,NewMarkersSoFar,NewCondsSoFar,ArithConds)
	;
	    % if not, we have to find what comes after it
	    % since this is bound to be an arithmetical expression, we know that it must begin with a (.  therefore it must end with a )).
	    name('))',DoubleLeft),
	    matchExpression(BeforeDouble,DoubleLeft,AfterDouble,RestVar),
	    name('?',Que),
	    append(Que,EquID,NewEquID),
	    append(NewEquID,BeforeDouble,NewMarker),
	    name(')',SingleLeft),
	    append(NewMarker,SingleLeft,PartNewMarker),
	    name('Equ',Equ),
	    append(Equ,PartNewMarker,FullNewMarker),
	    BetterMarkers = [FullNewMarker|NewMarkersSoFar],
	    append(BeforeDouble,SingleLeft,NewBefore),
	    append(NewBefore,AfterDouble,NewRestVar),
	    append(BeforeVarEquId,NewRestVar,NewCondsSoFar),	 
	    updateArith(Markers,NewMarkers,BetterMarkers,NewCondsSoFar,ArithConds)
	).

updateArith(Markers,NewMarkers,NewMarkersSoFar,CondsSoFar,ArithConds) :-
	name('(<',LessThan),
	matchExpression(BeforeLess,LessThan,AfterLess,CondsSoFar),
	name(')',LeftBracket),
	matchExpression(NumArg,LeftBracket,RestArgs,AfterLess),
	updateArithArg(NumArg,Markers,[],NewArg),
	append(NewArg,LeftBracket,NewBit),
	append(NewBit,RestArgs,NewAfter),
	name('(less',Decrease),
	append(BeforeLess,Decrease,FirstBit),
	append(FirstBit,NewAfter,NewCondsSoFar),
	updateArith(Markers,NewMarkers,NewMarkersSoFar,NewCondsSoFar,ArithConds).

updateArith(Markers,NewMarkers,NewMarkersSoFar,CondsSoFar,ArithConds) :-
	name('(>',GreaterThan),
	matchExpression(BeforeLess,GreaterThan,AfterLess,CondsSoFar),
	name(')',LeftBracket),
	matchExpression(NumArg,LeftBracket,RestArgs,AfterLess),
	updateArithArg(NumArg,Markers,[],NewArg),
	append(NewArg,LeftBracket,NewBit),
	append(NewBit,RestArgs,NewAfter),
	name('(more',Increase),
	append(BeforeLess,Increase,FirstBit),
	append(FirstBit,NewAfter,NewCondsSoFar),
	updateArith(Markers,NewMarkers,NewMarkersSoFar,NewCondsSoFar,ArithConds).

updateArith(Markers,NewMarkers,NewMarkersSoFar,Conds,NewConds) :-
	alterDetails(Conds,NewConds),
	append(Markers,NewMarkersSoFar,NewMarkers).

% updateArithArg(+NumArg,+Markers,+ArgsSoFar,-NewArg)
% replaces each arg in the numerical predicate with an appropriate replacement

updateArithArg(NumArg,Markers,ArgsSoFar,NewArg) :-
	name('?',Que),
	matchExpression(_,Que,Args,NumArg),
	name(' ',Space),
	matchExpression(FirstArg,Space,RestArgs,Args),
	replaceArg(FirstArg,Markers,NewFirstArg),
	append(NewFirstArg,ArgsSoFar,NewArgsSoFar),
	updateArithArg(RestArgs,Markers,NewArgsSoFar,NewArg).

updateArithArg(NumArg,Markers,ArgsSoFar,NewArg) :-
	name('?',Que),
	matchExpression(_,Que,Arg,NumArg),
	replaceArg(Arg,Markers,NewFirstArg),
	append(NewFirstArg,ArgsSoFar,RevArg),
	reverse(RevArg,NewArg).

% replaceArg(+NumArg,+Markers,-NewNumArg,-NewPreds,+NewPredsSoFar)
% this takes a numerical argument and a list of markers and, if the marker matches with anything, updates the argument.  it also returns a list of all the predicates involved.

replaceArg(NumArg,[],NewArg) :-
	reverse(NumArg,NewArg).

replaceArg(NumArg,[FirstMarker|_],NewArg) :-
	name('?',Que),
	matchExpression(_,Que,RestMarker,FirstMarker),
	name('(',RightBracket),
	% check to see if it matches:
	matchExpression(NumArg,RightBracket,MarkerArgs,RestMarker),
	% we then need to look at the related predicate
	% if so, we stick the bracket on to the marker args to form the new, number free arg
	name(' (',SpaceRB),
	append(SpaceRB,MarkerArgs,RevArg),
	reverse(RevArg,NewArg).

replaceArg(NumArg,[_|Rest],NewArg) :-
	% if it doens't match, we do nothing and recurse
	replaceArg(NumArg,Rest,NewArg).

% memberMarkers(+EquID,+Markers)
% succeeds if the equality identifier is a member of markers

memberMarkers(EquID,Markers) :-
	flatten(Markers,FlatMarkers),!,
	matchExpression(_,EquID,_,FlatMarkers).

% alterDetails(+Conds,-NewConds)
% once the updateArith process is complete, we need to alter some of the details.  to avoid looping, if the symbol required by KIF is the same as the symbol required by PDDL, we put in a place holder.  alterDetails replaces these place holders with the original symbol.

alterDetails(Conds,NewConds) :-
	name('(less',Less),
	name('(<',LessSig),
	matchExpression(BeforeLess,Less,AfterLess,Conds),
	append(BeforeLess,LessSig,NewBefore),
	append(NewBefore,AfterLess,CondsSoFar),
	alterDetails(CondsSoFar,NewConds).

alterDetails(Conds,NewConds) :-
	name('(more',Less),
	name('(>',LessSig),
	matchExpression(BeforeLess,Less,AfterLess,Conds),
	append(BeforeLess,LessSig,NewBefore),
	append(NewBefore,AfterLess,CondsSoFar),
	alterDetails(CondsSoFar,NewConds).

alterDetails(Conds,Conds).

% updateSums(+Conds,-NewConds)

updateSums(Conds,NewConds) :-
	updateSums(Conds,[],NewConds).

updateSums(Conds,BeforeConds,NewConds) :-
	name('(increase ',Increase),
	matchExpression(BeforeIncrease,Increase,AfterIncrease,Conds),
	% does AfterIncrease also contain an increase?  if so, the arguments should be rearranged and this should be changed to a plus.  if not, it can be left as it is.
	(   matchExpression(BeforeSecondIncrease,Increase,AfterSecondIncrease,AfterIncrease),
	    % find the end of the expression
	    name('))',End),
	    matchExpression(PlusArgs,End,RestOfConds,AfterSecondIncrease),
	    name('(+ ',Plus),
	    % in this case, we need to find out how many arguments there are.  KIF allows plus to take many arguments, but PDDL only two, so they must be nested.
	    nestPlus(PlusArgs,NewPlusArgs),
	    append(BeforeSecondIncrease,Plus,Begin),
	    append(Begin,NewPlusArgs,NewAfterIncrease),
	    append(NewAfterIncrease,End,FullAfterIncrease),
	    append(BeforeIncrease,Increase,NewBefore),
	    append(NewBefore,FullAfterIncrease,NewBeginning)
	;
	    name('))',End),
	    matchExpression(PlusArgs,End,RestOfConds,AfterIncrease),
	    append(BeforeIncrease,Increase,BeginIncrease),
	    append(BeginIncrease,PlusArgs,Beginning),
	    append(Beginning,End,NewBeginning)
	),
	append(NewBeginning,BeforeConds,FullBefore),
	updateSums(RestOfConds,FullBefore,NewConds).

updateSums(EndConds,BeginConds,NewConds) :-
	append(BeginConds,EndConds,NewConds).

% nestPlus(+PlusArgs,-NewPlusArgs)
% + is an n-ary function in KIF but only binary in PDDL, so we must nest the pluses

nestPlus(PlusArgs,NewPlusArgs) :-
	name(') (',IntArgs),
	matchExpression(FirstArg,IntArgs,RestArgs,PlusArgs),
	nestPlusArgs(RestArgs,NewRestArgs),
	removeFillers(NewRestArgs,GoodRestArgs),
	append(FirstArg,GoodRestArgs,FullRestArgs),
	name(')',RightBracket),
	append(FullRestArgs,RightBracket,NewPlusArgs).

nestPlusArgs(RestArgs,NewRestArgs) :-
	name(') (',IntArgs),
	matchExpression(FirstArg,IntArgs,OtherArgs,RestArgs),
	name(') plus (+ (',Plus),
	name(')',RightBracket),
	append(OtherArgs,RightBracket,EndArgs),
	append(Plus,FirstArg,NewFirst),
	name(') int (',Int),
	append(NewFirst,Int,FullFirst),
	append(FullFirst,EndArgs,NewRest),
	nestPlusArgs(NewRest,NewRestArgs).

nestPlusArgs(NewRestArgs,NewRestArgs).

% removeFillers(+Args,-NewArgs)
% removes the place holders from Args

removeFillers(Args,NewArgs) :-
	name('plus ',Plus),
	matchExpression(BeforePlus,Plus,AfterPlus,Args),
	append(BeforePlus,AfterPlus,BetterArgs),
	removeFillers(BetterArgs,NewArgs).

removeFillers(Args,NewArgs) :-
	name('int ',Int),
	matchExpression(BeforeInt,Int,AfterInt,Args),
	append(BeforeInt,AfterInt,BetterArgs),
	removeFillers(BetterArgs,NewArgs).

removeFillers(NewArgs,NewArgs).

% findArity(+Args,-Arity,+AritySoFar)
% returns the arity of a predicate

findArity(Args,Arity,AritySoFar) :-
	name('?',Que),
	matchExpression(_,Que,RestArgs,Args),
	NewArity is AritySoFar + 1,
	findArity(RestArgs,Arity,NewArity).

findArity(_,Arity,Arity) :-
	!.


% removeNumPreds(+Conds,+Markers,-RelevantConds)
% the predicates that state the numerical value of a variable are not required in PDDL, because this is kept track of independently.  here, we strip them out.  this is complicated by the fact that some of the numerical predicates contain pseudo-vars; these should not be stripped because PDDL does not acknowledge that they are variables and hence does not keep track of them.  also, if updateArity puts a place marker 'Equ' in a predicate, this needs to be treated with care
% removeNumPreds first builds up a list of all the markers with their arity attached, and then passes this to removeNumPredsRec

removeNumPreds(Conds,Markers,RelevantConds) :-
	buildMarkerArity(Markers,ArityMarkers,[]),
	removeNumPredsRec(Conds,ArityMarkers,RelevantConds).


% buildMarkerArity(+Markers,-ArityMarkers,+ArityMarkerSoFar)
% finds the arity of each marker and appends the as a single item list to the marker

buildMarkerArity([],ArityMarkers,ArityMarkers).

buildMarkerArity([FirstMarker|Rest],ArityMarkers,MarkersSoFar) :-
	findArity(FirstMarker,Arity,0),
	buildMarkerArity(Rest,ArityMarkers,[[[Arity]|FirstMarker]|MarkersSoFar]).
	
% removeNumPredsRec(+Conds,+Markers,-RelevantConds)
% for each marker, it removes the predicates indicated by that marker.  some of these may be in not brackets; these are removed afterwards.

removeNumPredsRec(Conds,[],RelevantConds) :-
	removeNots(Conds,RelevantConds).
	
removeNumPredsRec(Conds,[FirstMarker|Rest],RelevantConds) :-
	removeRelPreds(Conds,FirstMarker,[],FirstMarkerConds),
	removeNumPredsRec(FirstMarkerConds,Rest,RelevantConds).

removeNumPredsRec(Conds,[_|Rest],RelevantConds) :-
	removeNumPredsRec(Conds,Rest,RelevantConds).

% removeRelPreds(+Conds+FirstMarker,-RelevantConds
% under the relevant circumstances, the numerical predicate is removed.  if pseudo-var is found, nothing is changed.  if equ is found, this is dealt with by special predicate: removeRelPredsEqu and replaceEqu.  otherwise this is dealt with by removeRelPredsRec.

removeRelPreds(Conds,[[_]|[]],[],Conds).

removeRelPreds(Conds,FirstMarker,[],Conds) :-
	FirstMarker = [[_]|FirstName],
	name('Pseudo-Var',PseudID),
	sublist(FirstName,PseudID,_,_,_).

removeRelPreds(Conds,FirstMarker,EarlyConds,RelevantConds) :-
	FirstMarker = [[_]|FirstName],
	name('Equ',Equ),
	matchExpression([],Equ,RestEqu,FirstName),
	name('(',RightBracket),
	matchExpression(VarName,RightBracket,Vars,RestEqu),
	name('?',Que),
	matchExpression(Que,GoodName,[],VarName),
	name(' (',SpBr),
	append(SpBr,Vars,GoodVars),
	removeRelPredsEqu(Conds,Vars,EarlyConds,NoEquConds),
	replaceEqu(NoEquConds,GoodName,GoodVars,GoodConds),
	append(EarlyConds,GoodConds,RelevantConds).

removeRelPreds(Conds,FirstMarker,EarlyConds,RelevantConds) :-
	removeRelPredsRec(Conds,FirstMarker,EarlyConds,RelevantConds).

removeRelPreds(Conds,_,EarlyConds,FullConds) :-
	append(EarlyConds,Conds,FullConds).

% removeRelPredsEqu(+Conds,+Vars,+EarlyConds,+RelevantConds)
% removes the relevant predicate where and Equ place marker is found

removeRelPredsEqu(Conds,Vars,[],RelevantConds) :-
	matchExpression(BeforeVars,Vars,AfterVars,Conds),
	append(BeforeVars,AfterVars,NewConds),
	name('()',Brackets),
	matchExpression(BeforeBrackets,Brackets,AfterBrackets,NewConds),
	append(BeforeBrackets,AfterBrackets,GoodConds),
	removeRelPredsEqu(GoodConds,Vars,[],RelevantConds).

removeRelPredsEqu(Conds,_,_,Conds).

% replaceEqu(+Conds,+VarName,+Vars,-RelevantConds)
% replaces VarName with Vars every time it occurs

replaceEqu(Conds,VarName,Vars,RelevantConds) :-
	matchExpression(BeforeVarName,VarName,AfterVarName,Conds),
	append(BeforeVarName,Vars,NewBefore),
	append(NewBefore,AfterVarName,NewConds),
	replaceEqu(NewConds,VarName,Vars,RelevantConds).

replaceEqu(Conds,_,_,Conds).

% removeRelPredsRec(+Conds,+FirstMarker,+EarlyConds,-RelevantConds)
% finds all the predicates in which the marker occurs.  if this has the same arity as the marker, it is removed.  if not, then it should not be removed because it is a mention of the marker in a differnet context.  in this case, it is stuck on earlyconds and the rest of conditions is checked, to avoid looping.

removeRelPredsRec(Conds,FirstMarker,EarlyConds,RelevantConds) :-
	FirstMarker = [[Arity]|MarkerArgs],
	% we need to find the name of the marker:
	name('(',RightBracket),
	matchExpression(_,RightBracket,FullFirstName,MarkerArgs),
	name(' ',Space),
	matchExpression(FirstName,Space,_,FullFirstName),!,
	% first, we identify where the first marker is:
	append(RightBracket,FirstName,FullMarker),
	matchExpression(Before,FullMarker,After,Conds),
	% now, we remove the rest of the marker from after:
	name(')',LeftBracket),
	matchExpression(RestArgs,LeftBracket,GoodAfter,After),
	findArity(RestArgs,NewArity,0),
	(   Arity = NewArity,
	    % now, we paste back together and recurse
	    append(Before,GoodAfter,NewConds),
	    removeRelPreds(NewConds,FirstMarker,EarlyConds,RelevantConds)
	;
	    append(Before,FullMarker,FullBefore),
	    append(FullBefore,EarlyConds,NewEarlyConds),
	    removeRelPreds(After,FirstMarker,NewEarlyConds,RelevantConds)
	).

% removeNots(+Conds,-RelevantConds)
% searches for occurence of not and removes them

removeNots(Conds,RelevantConds) :-
	name('(Not )',Not),
	matchExpression(BeforeNot,Not,AfterNot,Conds),
	append(BeforeNot,AfterNot,NewConds),
	removeNots(NewConds,RelevantConds).

removeNots(RelevantConds,RelevantConds).


% addTypeInfo(+Conds,+PredList,-TypedConds).
% takes a list of conds and the predicate list and adds as extra conds all the types of the arguments.

addTypeInfo(Conds,Postconds,PredicateList,TypedConds) :-
	name('(And',And),
	matchExpression(_,And,Args,Conds),
	name('(And',And),
	matchExpression(_,And,PostcondArgs,Postconds),
	name(')',EndBracket),
	matchExpression(GoodArgs,EndBracket,[],Args),
	append(GoodArgs,PostcondArgs,FullArgs),
	findPreds(FullArgs,PredsPresent,PredArgs,[],[]),
	addNewArgs(PredsPresent,PredicateList,GoodArgs,PredArgs,MostlyTypedConds),
	name(')(',BeginExtra),
	matchExpression(RealConds,BeginExtra,NewConds,MostlyTypedConds),
	name(') (',NewBegin),
	append(RealConds,NewBegin,Middle),
	append(Middle,NewConds,Full),
	name(' ',Space),
	append(And,Space,NewAnd),
	append(NewAnd,Full,TypedConds).

addTypeInfo(Conds,_Postconds,_PredicateList,Conds).


% findPreds(+Args,+PredsPresent,-PredArgs,+PredsSoFar,+ArgsSoFar)	
% finds the class definitions of the arguments of the predicate

findPreds(Args,PredsPresent,PredArgs,PredsSoFar,ArgsSoFar) :-
	name('(',LeftBracket),
	matchExpression(_,LeftBracket,RestArgs,Args),
	name(' ',Space),
	matchExpression(PredName,Space,OtherPreds,RestArgs),
	name(')',RightBracket),
	matchExpression(WholePred,RightBracket,_,RestArgs),
	findPreds(OtherPreds,PredsPresent,PredArgs,[PredName|PredsSoFar],[WholePred|ArgsSoFar]).

findPreds(_,PredsPresent,PredArgs,PredsPresent,PredArgs).

% addNewArgs(+PredsPresent,+PredicateList,+Args,+PredArgs,-MostlyTypedConds)
% replaces the arguments of the predicates with the appropriate ones

addNewArgs(PredsPresent,PredicateList,Conds,PredArgs,TypedConds) :-
	name(')',EndConds),
	findArgsList(PredsPresent,PredicateList,ArgsList,[]),
	makeNewArgs(ArgsList,PredArgs,PredArgs,NewArgs,[]),
	removeEndSpaces(Conds,GoodConds),
	append(GoodConds,NewArgs,NewConds),
	append(NewConds,EndConds,TypedConds).


removeEndSpaces(Conds,Conds) :-
	name(')',EndBracket),
	matchExpression(_RestConds,EndBracket,[],Conds).

removeEndSpaces(Conds,GoodConds) :-
	name(' ',Space),
	matchExpression(BetterConds,Space,[],Conds),
	removeEndSpaces(BetterConds,GoodConds).



% findArgsList(+PredsPresent,+PredicateList,-ArgsList,+ArgsSoFar)
% returns the arguments of the predicate

findArgsList([],_,NewArgs,NewArgs).

findArgsList([FirstPred|Rest],PredicateList,NewArgs,NewArgsSoFar) :-
	namedPredicate(FirstPred,PredicateList,PredicateEntry),
	findArgsList(Rest,PredicateList,NewArgs,[PredicateEntry|NewArgsSoFar]).

% makeNewArgs(+ArgsList,+PredList,+PredList,-NewList,+NewListSoFar)
% uses the arguments returns from findArgsList to complete the predicate

makeNewArgs([],PredList,PredList,NewList,NewList).

makeNewArgs([[_]|Rest],PredList,PredList,NewList,NewListSoFar) :-
	makeNewArgs(Rest,PredList,PredList,NewList,NewListSoFar).

makeNewArgs([[NameFirstPred|FirstArgs]|Rest],[FirstRealPred|_],PredArgs,NewList,NewListSoFar) :-
	sublist(FirstRealPred,NameFirstPred,_,_,_),
	matchExpression(_,NameFirstPred,RealFirstArgs,FirstRealPred),
	name(' ',Space),
	matchExpression([],Space,GoodRealArgs,RealFirstArgs),
	buildArgsList(FirstArgs,GoodRealArgs,ExtraArgsList,[]),
	append(ExtraArgsList,NewListSoFar,BetterNewList),
	makeNewArgs(Rest,PredArgs,PredArgs,NewList,BetterNewList).

makeNewArgs(ArgsList,[_|RestReal],PredArgs,NewList,NewListSoFar) :-
	makeNewArgs(ArgsList,RestReal,PredArgs,NewList,NewListSoFar).

% buildArgsList(+FirstArgs,+RealFirstArgs,-ExtraArgsList,-ExtraSoFar)
% builds the list up in reverse order, making the args into the correct format for the axioms

buildArgsList([],[],ExtraArgsList,ExtraArgsList).

buildArgsList([],_,ExtraArgsList,ExtraArgsList).


buildArgsList(FirstArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar) :-
	removeLastElement(FirstArgs,RightArg,RestArgs,[]),
	name(' ',Space),
	matchExpression(Type,Space,_,RightArg),
	name('(Number',Type),
	buildArgsList(RestArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar).

buildArgsList(FirstArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar) :-
	removeLastElement(FirstArgs,RightArg,RestArgs,[]),
	name(' ',Space),
	matchExpression(Type,Space,_,RightArg),
	name('(Confirmation-Number',Type),
	matchExpression(_,Space,RestRealArgs,RealFirstArgs),
	buildArgsList(RestArgs,RestRealArgs,ExtraArgsList,ExtraSoFar).

buildArgsList(FirstArgs,_,ExtraArgsList,ExtraArgsList) :-
	removeLastElement(FirstArgs,RightArg,_,[]),
	name(' ',Space),
	matchExpression(Type,Space,_,RightArg),
	name('(Confirmation-Number',Type).

% this is screwing up the declaration of meta-vars; don't think it's really necessary.  meta-vars should not be treated differently; they have to be declared in the same way as variables with any other name.

%buildArgsList(FirstArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar) :-
%	name(' ',Space),
%	(    matchExpression(FirstRealFirstArg,Space,_RestRealArgs,RealFirstArgs)
%	;
%	     FirstRealFirstArg = RealFirstArgs
%        ),
%	name('?MetaVar',FirstRealFirstArg),
%	removeLastElement(FirstArgs,_RightArg,RestArgs,[]),
%	buildArgsList(RestArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar).
	

buildArgsList(FirstArgs,RealFirstArgs,ExtraArgsList,ExtraSoFar) :-
	removeLastElement(FirstArgs,RightArg,RestArgs,[]),
	name(' ',Space),
	matchExpression(Type,Space,_,RightArg),
	matchExpression(FirstArgId,Space,RestRealArgs,RealFirstArgs),
	name(')',RightBracket),
	append(Type,Space,TypeBegin),
	append(TypeBegin,FirstArgId,MiddleType),
	append(MiddleType,RightBracket,FullType),
	append(FullType,Space,NewExtra),
	append(NewExtra,ExtraSoFar,NewSoFar),
	buildArgsList(RestArgs,RestRealArgs,ExtraArgsList,NewSoFar).

buildArgsList([LastArg],RealLastArg,ExtraArgsList,ExtraSoFar) :-
	name(' ',Space),
	matchExpression(Type,Space,_,LastArg),
	name(')',RightBracket),
	append(Type,Space,TypeBegin),
	append(TypeBegin,RealLastArg,MiddleType),
	append(MiddleType,RightBracket,FullType),
	append(FullType,Space,NewExtra),
	append(NewExtra,ExtraSoFar,ExtraArgsList).

% numberedPseudos(+Conds,+NumberedPseudoConds,+NumberSoFar,-TotalNumber,-PseudoPreds,+PseudoPredsSoFar)
% numbers the pseudo-vars so that they are uniquely identified

numberedPseudos(Conds,NumberedPseudoConds,NumberSoFar,TotalNumber,PseudoPreds,PseudoPredsSoFar) :-
	name('Pseudo-Var',PV),
	matchExpression(BeforePseudo,PV,AfterPseudo,Conds),
	name(NumberSoFar,NumberChar),
	name('PseudoVar',NewPV),
	append(NewPV,NumberChar,NewPseud),
	append(BeforePseudo,NewPseud,NewBegin),
	append(NewBegin,AfterPseudo,NewConds),
	reverse(BeforePseudo,RevBefore),
	name('(',BeginPred),
	matchExpression(RevEarlyPred,BeginPred,_,RevBefore),
	reverse(RevEarlyPred,EarlyPred),
	name(' ',Space),
	matchExpression(PredName,Space,_,EarlyPred),
	NewNumber is NumberSoFar + 1,
	numberedPseudos(NewConds,NumberedPseudoConds,NewNumber,TotalNumber,PseudoPreds,[[PredName,NewPseud]|PseudoPredsSoFar]).
	
numberedPseudos(NumberedPseudoConds,NumberedPseudoConds,TotalNumber,TotalNumber,PseudoPreds,PseudoPreds).

% numberedPredicate(+PredPresent,+Preds,-PredEntry)
% finds the exact way the predicate is referred to in the list of predicates

namedPredicate(PredPresent,[],[PredPresent]).

namedPredicate(PredPresent,[FirstPred|_],FirstPred) :-
	member(PredPresent,FirstPred).

namedPredicate(PredPresent,[_|Rest],PredicateEntry) :-
	namedPredicate(PredPresent,Rest,PredicateEntry).

% correctPseudos(+PseudoPreds,+Conds,-Postconds)
% numbers the pseudopreds correctly

correctPseudos(PseudoPreds,Conds,Postconds) :-
	name('Pseudo-Var',PV),
	matchExpression(BeforePseudo,PV,AfterPseudo,Conds),
	reverse(BeforePseudo,RevBefore),
	name('(',BeginPred),
	matchExpression(RevEarlyPred,BeginPred,_,RevBefore),
	reverse(RevEarlyPred,EarlyPred),
	name(' ',Space),
	matchExpression(PredName,Space,_,EarlyPred),
	member([PredName,Var],PseudoPreds),
	append(BeforePseudo,Var,NewBegin),
	append(NewBegin,AfterPseudo,NewConds),
	correctPseudos(PseudoPreds,NewConds,Postconds).

correctPseudos(_,Postconds,Postconds).
	
% removeLastElement(+List,-LastElement,+RestElements,+RestSoFar)
% returns the last element of a list and the rest of the elements

removeLastElement([EarlyElement|Rest],LastElement,RestElements,RestSoFar) :-
	removeLastElement(Rest,LastElement,RestElements,[EarlyElement|RestSoFar]).

removeLastElement([LastElement],LastElement,RestElements,RestSoFar) :-
	reverse(RestSoFar,RestElements).

% largestElement(+List,+LargestSoFar,-Largest)
% returns the largest element of a list of numbers

largestElement([],Largest,Largest).

largestElement([H|T],LargestSoFar,Largest) :-
	H > LargestSoFar,
	largestElement(T,H,Largest).

largestElement([_|T],LargestSoFar,Largest) :-
	largestElement(T,LargestSoFar,Largest).