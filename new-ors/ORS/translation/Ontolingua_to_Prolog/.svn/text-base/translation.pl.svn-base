:- use_module(library(lists)),use_module(library(random)).

% ****************************************


% translation [formerly translation(Goal) because the (now defunct) translation to PDDL required the Goal]
% reads in the relevant files, the ontology and the meta ontology.  reads the ontologies, builds up the relevant lists and then passes them to the translate to PDDL and translate to Prolog processes

translation(Scenario,ScenarioPath) :-
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translateToProlog', PrologPath),
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translateToPDDL', PDDLPath),
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translateMetaOnt', MetaPath),
	consult(PrologPath),
	consult(PDDLPath),
	consult(MetaPath),
	atom_concat(ScenarioPath,'/ont.in',OntPath),		 
	see(OntPath),
%	translateGoal(Goal,TransGoal),
	buildLists(ProblemList,DomainList),!,
	seen,
	atom_concat(ScenarioPath,'/metaOnt.in',MetaOntPath),
	see(MetaOntPath),
	buildMetaList(MetaList),
	seen,
	buildProblemLists(ProblemList,IndivList,ProbClassList,NoSitFactList,[],[],[]),
	% lala
	%flatten(ProblemList, PL), printall(PL), flatten(DomainList, DL), printall(DL),
	% lala	
	buildDomainLists(DomainList,PredicateList,DomClassList,RuleList,NumericalPredicateList,ClassHierarchyList,[],[],[],[],[]),
	stripPredicateSituations(PredicateList,[],NoSitPredicateList),
	stripRuleSituations(RuleList,[],NoSitRuleList),
	stripNumericalPredicateSituations(NumericalPredicateList,[],NoSitNumericalPredicateList),
	translateMetaOnt(MetaList,Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/metaOnt.pl',MetaPlPath),
	consult(MetaPlPath),
%	translateToPDDL(TransGoal,IndivList,ProbClassList,DomClassList,NoSitFactList,NoSitPredicateList,NoSitRuleList,NoSitNumericalPredicateList,ClassHierarchyList),
	translateToProlog(ProbClassList,NoSitFactList,NoSitRuleList,NoSitNumericalPredicateList,ClassHierarchyList,NoSitPredicateList,Scenario,ScenarioPath).
%	write('translation done '),nl.

printall(L) :- name(X, L), write(X), nl.


% *********************************************

% building the problem and domain lists:

% buildLists(-ProblemList,-DomainList)
% reads the current input stream and builds up two lists, one of things that are relevant for the problem file, and one for the domain file

buildLists(ProblemList,DomainList) :-
	findLists([],[],ProblemList,DomainList).

% findLists(+ProbSoFar,+DomSoFar,-ProblemList,-DomainList)
% reads each line and checks which list to add it too

findLists(ProbSoFar,DomSoFar,ProblemList,DomainList) :-
	readLine(Line),!,
	checkWhichList(ProbSoFar,DomSoFar,ProblemList,DomainList,Line).

% checkWhichList(-ProblemList,-DomainList,+ProbSoFar,+DomSoFar)
% checks to see if the line is the last, and then checks whether it this line is relevant to either the problem list, the domain list, both or neither.  if it is the last line, it terminates, otherwise it recurses.

checkWhichList(ProblemList,DomainList,[Rest|ProblemList],DomainList,Line) :-
	extractLast(Line,end,Rest),
	checkRelevant(Rest,problem).

checkWhichList(ProblemList,DomainList,ProblemList,[Rest|DomainList],Line) :-
	extractLast(Line,end,Rest),
	checkRelevant(Rest,domain).

checkWhichList(ProblemList,DomainList,ProblemList,DomainList,Line) :-
	extractLast(Line,end,Rest),
	checkRelevant(Rest,none).


checkWhichList(ProbSoFar,DomSoFar,ProblemList,DomainList,Line) :-
	checkRelevant(Line,problem),
	checkRelevant(Line,domain),
	findLists([Line|ProbSoFar],[Line|DomSoFar],ProblemList,DomainList).
	
checkWhichList(ProbSoFar,DomSoFar,ProblemList,DomainList,Line) :-
	checkRelevant(Line,problem),
	findLists([Line|ProbSoFar],DomSoFar,ProblemList,DomainList).

checkWhichList(ProbSoFar,DomSoFar,ProblemList,DomainList,Line) :-
	checkRelevant(Line,domain),
	findLists(ProbSoFar,[Line|DomSoFar],ProblemList,DomainList).

checkWhichList(ProbSoFar,DomSoFar,ProblemList,DomainList,Line) :-
	checkRelevant(Line,none),
	findLists(ProbSoFar,DomSoFar,ProblemList,DomainList).


% checkRelevant(+Line,+RelevantList)
% determines what kind of ontological object the line is describing, and succeeds if this matches RelevantList

checkRelevant(Line,problem) :-
	(   name('(Define-Individual',DefInd),
	    matchExpression([],DefInd,_,Line)
	;
	    name('(Define-Frame',DefFrame),
	    matchExpression([],DefFrame,_,Line)
	;
	    name('(Define-Class',DefClass),
	    matchExpression([],DefClass,_,Line)
	).


checkRelevant(Line,domain) :-
	% check if this is a function
	(   name('(Define-Function',DefFunc),
	    matchExpression([],DefFunc,_,Line)
	;   
	    name('(Define-Relation',DefRel),
	    matchExpression([],DefRel,_,Line)
	;
	    name('(Define-Axiom',DefAx),
	    matchExpression([],DefAx,_,Line)
	;
	    name('(Define-Class',DefClass),
	    matchExpression([],DefClass,_,Line)
	).

checkRelevant(_,none).


% ************************************************************

% building the problem list:

% processProblem(ProblemList)

% this builds up a lists of all the ontological arguments that are necessary for the problem file, and then uses this information to write out the problem file

%processProblem(ProblemList,Goal) :-
%	buildProblemLists(ProblemList,IndivList,ClassList,FactList,[],[],[]),
%	buildProblemFile(IndivList,ClassList,FactList,Goal).


% buildProblemLists(+ProblemList,-IndivList,-ClassList,-FactList,+IndivListSoFar,+ClassListSoFar,+FactListSoFar)

% takes the problem list (which contains all the information relevant to the problem file) and builds into three different lists, containing, respectively, individuals, classes and facts.


buildProblemLists([],FinalIndivList,ClassList,FactList,IndivList,ClassList,FactList):-
	removeDuplicates(IndivList,[],GoodIndivList),
	reverse(GoodIndivList,FinalIndivList).

% if the object is an individual, then just the class needs to be extracted.  There are no attached facts.

buildProblemLists([FirstProblem|Rest],IndivList,ClassList,FactList,IndivListSoFar,ClassListSoFar,FactListSoFar) :-
	% if it is an individual, we want to extract the name and the class.  there are no facts.
	name('(Define-Individual ',DefIn),
	matchExpression([],DefIn,Body,FirstProblem),
	name(' (',ClassIndicator),
	matchExpression(Name,ClassIndicator,AfterName,Body),
	name(') ',ClassEndIndicator),
	matchExpression(Class,ClassEndIndicator,_,AfterName),
	buildProblemLists(Rest,IndivList,ClassList,FactList,[Name|IndivListSoFar],[[Name|Class]|ClassListSoFar],FactListSoFar).


% if the object is a frame, then there may be facts.  There will be a class and there may be single facts (with the name of the frame removed) and / or complex facts (listed as axioms).

buildProblemLists([FirstProblem|Rest],IndivList,ClassList,FactList,IndivListSoFar,ClassListSoFar,FactListSoFar) :-
	% if it is a frame (an individual with attached facts), we need to extract the name, the class (harder than above) and the facts.
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,Body,FirstProblem),
	name(' :',EndName),
	matchExpression(Name,EndName,AfterName,Body),
	name('Instance-Of ',InstID),
	name('Subclass-of ',SubcID),
	(   matchExpression(BeforeClass,InstID,ClassPlusRest,AfterName)
	;
	    matchExpression(BeforeClass,SubcID,ClassPlusRest,AfterName)
	),
	name('))',ClassEnd),
	matchExpression(ClassPlusSingleFacts,ClassEnd,_,ClassPlusRest),
	name(') (',FactIndicator),
	(   name('Own-Slots (',FactBegin),
	    matchExpression(_,FactBegin,EarlySingleFacts,BeforeClass),
	    matchExpression(Class,FactIndicator,SingleFacts,ClassPlusSingleFacts),
	    append(EarlySingleFacts,SingleFacts,AllSingleFacts),
	    stripFactSituations(AllSingleFacts,NoSitSingleFacts),
	    processSingleFacts(Name,NoSitSingleFacts,SingleFactList),
	    name(':Axioms (',AxID),
	    matchExpression(_,AxID,Axioms,ClassPlusRest),
	    stripFactSituations(Axioms,NoSitAxioms),
	    processComplexFacts(NoSitAxioms,AxiomList),
	    append(SingleFactList,AxiomList,FullFactList)
	;
	    matchExpression(Class,FactIndicator,SingleFacts,ClassPlusSingleFacts),
	    stripFactSituations(SingleFacts,NoSitSingleFacts),
	    processSingleFacts(Name,NoSitSingleFacts,FullFactList)
	;
	    name(')) :Axioms (',AxID),
	    matchExpression(Class,AxID,Axioms,ClassPlusRest),
	    stripFactSituations(Axioms,NoSitAxioms),
	    processComplexFacts(NoSitAxioms,FullFactList)
	;
	    FullFactList = [],
	    Class = ClassPlusSingleFacts
	),
	append(FullFactList,FactListSoFar,NewFactList),
	stripIndividuals(FullFactList,[],NewIndividuals),
	addToIndivList([Name|IndivListSoFar],NewIndividuals,NewIndivList),
	buildProblemLists(Rest,IndivList,ClassList,FactList,NewIndivList,[[Name|Class]|ClassListSoFar],NewFactList).




buildProblemLists([_|Rest],IndivList,ClassList,FactList,IndivListSoFar,ClassListSoFar,FactListSoFar) :-
	buildProblemLists(Rest,IndivList,ClassList,FactList,IndivListSoFar,ClassListSoFar,FactListSoFar).


buildProblemLists([_|Rest],IndivList,ClassList,FactList,NumPredList,ClassHierList,IndivListSoFar,ClassListSoFar,FactListSoFar,NumPredListSoFar,HierSoFar) :-
	buildProblemLists(Rest,IndivList,ClassList,FactList,NumPredList,ClassHierList,IndivListSoFar,ClassListSoFar,FactListSoFar,NumPredListSoFar,HierSoFar).



% processSingleFacts(+Name,+SingleFacts,-SingleFactList)
% this works out the facts connected to an individual if there is only one of them and returns it in an appropriate format

processSingleFacts(Name,SingleFacts,SingleFactList) :-
	processSingleFacts(Name,SingleFacts,[],NumberedSingleFacts),
	processForNumbers(NumberedSingleFacts,SingleFactList,[]).

% documentation appears as a fact, but we don't want it to be considered to be so in translation.  thus we search for it and discard it.

processSingleFacts(Name,SingleFacts,FactsSoFar,SingleFactList) :-
	name(') ',FactIndicator),
	matchExpression(Fact1,FactIndicator,MoreFacts,SingleFacts),
	name('Documentation',DocuIndicator),
	matchExpression(_,DocuIndicator,_,Fact1),
	processSingleFacts(Name,MoreFacts,FactsSoFar,SingleFactList).

processSingleFacts(Name,SingleFacts,FactsSoFar,SingleFactList) :-
	name(') ',FactIndicator),
	matchExpression(Fact1,FactIndicator,MoreFacts,SingleFacts),
	addName(Name,Fact1,PartFact1),
	name(')',Bracket),
	append(PartFact1,Bracket,FullFact1),
	processSingleFacts(Name,MoreFacts,[FullFact1|FactsSoFar],SingleFactList).

processSingleFacts(Name,SingleFacts,FactsSoFar,[FullFact|FactsSoFar]) :-
	addName(Name,SingleFacts,PartFact),
	name(')',Bracket),
	append(PartFact,Bracket,FullFact).

% addName(+Name,+Fact,-NamedFact)
% adds the name of the individual concerned into the relevant fact

addName(Name,Fact,NamedFact) :-
	name(' ',Space),
	matchExpression(PredName,Space,Args,Fact),
	append(Name,Space,FullName),
	append(FullName,Args,FullArgs),
	append(PredName,Space,FullPredName),
	append(FullPredName,FullArgs,NamedFact).

addName(Name,Fact,NamedFact) :-
	name(' ',Space),
	append(Fact,Space,FactBegin),
	append(FactBegin,Name,NamedFact).


% processComplexFacts(+Axioms,-AxiomList)
% this works out the facts connected to an individual if there are many of them and returns them in an appropriate format

processComplexFacts(Axioms,AxiomList) :-
	name('))',EndAx),
	matchExpression(ActAx,EndAx,_,Axioms),
	processComplexFacts(ActAx,[],NumberedAxioms),
	processForNumbers(NumberedAxioms,AxiomList,[]).

processComplexFacts(ComplexFacts,ComplexFactList) :-
	processComplexFacts(ComplexFacts,[],ComplexFactList).

processComplexFacts(ComplexFacts,FactsSoFar,ComplexFactList) :-
	name(') ',FactIndicator),
	matchExpression(Fact1,FactIndicator,MoreFacts,ComplexFacts),
	name(')',Bracket),
	append(Fact1,Bracket,FullFact1),
	processComplexFacts(MoreFacts,[FullFact1|FactsSoFar],ComplexFactList).

processComplexFacts(ComplexFacts,FactsSoFar,[FullFact|FactsSoFar]) :-
	name(')',Bracket),
	append(ComplexFacts,Bracket,FullFact).

% processForNumbers(+NumberedAxioms,-FinishedFacts,+FactsSoFar)
% facts that contain numbers must be treated differently in PDDL.  this checks to see if alteration is necessary and then calls the appropriate function

processForNumbers([],FinishedFacts,FinishedFacts).

processForNumbers([FirstFact|Rest],FinishedFacts,FactsSoFar) :-
	name(')',FinalBracket),
	matchExpression(ActualArgs,FinalBracket,_,FirstFact),
	(   numberArgs(ActualArgs,[],NewFact),
	    processForNumbers(Rest,FinishedFacts,[NewFact|FactsSoFar])
	;
	    processForNumbers(Rest,FinishedFacts,[FirstFact|FactsSoFar])
	).

% numberArgs(+ArgsList,+ArgsListSoFar,-NewFact)
% checks to see whether the first character is a number, if so, finding the end of the number and then alterting the fact appropriately.  if not, it recurses

numberArgs([H|T],PrevArgs,NewFact) :-
	numChar(H),
	findEndNumber(T,RestArgs,RestNumber,[]),
	append([H],RestNumber,WholeNumber),
	name('( = ',Declaration),
	name(' ',Space),
	matchExpression([],Space,GoodPrevArgs,PrevArgs),
	reverse(GoodPrevArgs,RevPrevArgs),
	append(RevPrevArgs,RestArgs,WholeArgs),
	name(')',LeftBracket),
	append(WholeArgs,LeftBracket,PredInfo),
	append(Declaration,PredInfo,PartFact),
	name(' ',Space),
	append(Space,WholeNumber,Value),
	append(PartFact,Value,WholeFact),
	name(')',LeftBracket),
	append(WholeFact,LeftBracket,NewFact).

numberArgs([H|T],PrevArgs,NewFact) :-
	numberArgs(T,[H|PrevArgs],NewFact).

% findEndNumber(+Args,-RestArgs,-Number,+NumSoFar)
% if the first element of Args is a number, it adds this to number so far and recurses.  if not, it returns everything that comes after the number and the whole number.

findEndNumber([H|T],RestArgs,RestNumber,NumSoFar) :-
	numChar(H),
	findEndNumber(T,RestArgs,RestNumber,[H|NumSoFar]).
	
findEndNumber(RestArgs,RestArgs,RestNumber,RevRestNumber) :-
	reverse(RestNumber,RevRestNumber).


stripIndividuals([],IndivList,IndivList).

stripIndividuals([FirstFact|OtherFacts],IndivListSoFar,IndivList) :-
	name('=',Equals),
	matchExpression(_,Equals,_,FirstFact),
	stripIndividuals(OtherFacts,IndivListSoFar,IndivList).

stripIndividuals([FirstFact|OtherFacts],IndivListSoFar,IndivList) :-
	name(' ',Space),
	matchExpression(_PredName,Space,FactIndivs,FirstFact),
	findIndivList(FactIndivs,[],FactIndivList),
	append(FactIndivList,IndivListSoFar,NewIndivsSoFar),
	stripIndividuals(OtherFacts,NewIndivsSoFar,IndivList).


addToIndivList(NewIndivList,[],NewIndivList).

addToIndivList(IndivListSoFar,[FirstNewIndiv|RestNewIndivs],NewIndivList) :-
	member(FirstNewIndiv,IndivListSoFar),
	addToIndivList(IndivListSoFar,RestNewIndivs,NewIndivList).

addToIndivList(IndivListSoFar,[FirstNewIndiv|RestNewIndivs],NewIndivList) :-
	addToIndivList([FirstNewIndiv|IndivListSoFar],RestNewIndivs,NewIndivList).


findIndivList(FactIndivs,IndivsSoFar,FullIndivs) :-
	name(' ',Space),
	matchExpression(FirstIndiv,Space,RestIndivs,FactIndivs),
	findIndivList(RestIndivs,[FirstIndiv|IndivsSoFar],FullIndivs).

findIndivList(LastIndiv,IndivsSoFar,[GoodLastIndiv|IndivsSoFar]) :-
	name(')',EndBracket),
	matchExpression(GoodLastIndiv,EndBracket,_,LastIndiv).


removeDuplicates([],NewList,NewList).

removeDuplicates([FirstMember|Rest],NewListSoFar,NewList) :-
	member(FirstMember,NewListSoFar),
	removeDuplicates(Rest,NewListSoFar,NewList).

removeDuplicates([FirstMember|Rest],NewListSoFar,NewList) :-
	removeDuplicates(Rest,[FirstMember|NewListSoFar],NewList).


% *******************************************************

% building the domain list:

% buildDomainLists(+DomainList,-PredicateList,-ClassList,-RuleList,-NumPredList,+PredicateListSoFar,+ClassListSoFar,+RuleListSoFar,+NumPredListSoFar).
% builds up and processes the information of each type of ontological object required in the domain file.  as well as building up a list of predicates, it also builds up a list of which of these are numerical, because these require special treatment.


buildDomainLists([],PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateList,ClassList,RuleList,NumPredList,ClassHierList).

% for a function or relation, the arguments need to be identified, and they need to be checked to see if they are numerical.

buildDomainLists([FirstDom|Rest],PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateSoFar,ClassSoFar,RuleSoFar,NumSoFar,HierSoFar) :-
	% first, we check for predicates:
	(   name('(Define-Function ',DefFunc),
	    matchExpression([],DefFunc,Body,FirstDom)
	;
	    name('(Define-Relation ',DefRel),
	    matchExpression([],DefRel,Body,FirstDom)
	),
	% now we need to find the name of the function:
	name(' (',NameEnd),
	matchExpression(Name,NameEnd,AfterName,Body),
	% now we need the arguments:
	name(':Def (And ',ArgBegin),
	matchExpression(_,ArgBegin,Arguments,AfterName),
	processArguments(Arguments,ArgumentList),
	(   checkNumerical(ArgumentList,Name,NewPred,[]),
	    buildDomainLists(Rest,PredicateList,ClassList,RuleList,NumPredList,ClassHierList,[[Name|ArgumentList]|PredicateSoFar],ClassSoFar,RuleSoFar,[NewPred|NumSoFar],HierSoFar)
	;
	    buildDomainLists(Rest,PredicateList,ClassList,RuleList,NumPredList,ClassHierList,[[Name|ArgumentList]|PredicateSoFar],ClassSoFar,RuleSoFar,NumSoFar,HierSoFar)
	).

% for a class, we need to find what it is a subclass of

buildDomainLists([FirstDom|Rest],PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateSoFar,ClassSoFar,RuleSoFar,NumSoFar,HierSoFar) :-
	% now we check for classes:
	name('(Define-Class ',DefClass),
	matchExpression([],DefClass,Body,FirstDom),
	% we need the name of the class:
	name(' (',NameEnd),
	matchExpression(Name,NameEnd,EndBody,Body),
	% now we look for the subclass
	name('(And (',ClassId),
	matchExpression(_,ClassId,EndClass,EndBody),
	name(' ',Space),
	matchExpression(SuperClass,Space,_,EndClass),
	buildDomainLists(Rest,PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateSoFar,[Name|ClassSoFar],RuleSoFar,NumSoFar,[[Name|SuperClass]|HierSoFar]).

% for an axiom, the preconditions and postconditions are found.  
	
buildDomainLists([FirstDom|Rest],PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateSoFar,ClassSoFar,RuleSoFar,NumSoFar,HierSoFar) :-
	% finally, we check for rules:
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,Body,FirstDom),
	% first, we find the name:
	name(' ',NameEnd),
	matchExpression(Name,NameEnd,AfterName,Body),
	% now, we need the preconditions:
	(   name('=> (And (',PrecondBegin),
	    matchExpression(_,PrecondBegin,PreAndPostConds,AfterName)
	;
	    name('=> (',PrecondBegin),
	    matchExpression(_,PrecondBegin,PreAndPostConds,AfterName)
	),
	(   name(' (And (',PostcondBegin),
	    matchExpression(Preconds,PostcondBegin,Postconds,PreAndPostConds)
	;
	    name(') (',PostcondBegin),
	    matchExpression(NearlyPreconds,PostcondBegin,NearlyPostconds,PreAndPostConds),
	    name('))',PrecondEnd),
	    append(NearlyPreconds,PrecondEnd,Preconds),
	    (	name(' ',Space),
		matchExpression(GoodPostconds,Space,[],NearlyPostconds),
		name(')',PostcondEnd),
		append(GoodPostconds,PostcondEnd,Postconds)
	    ;
		name(')',PostcondEnd),
		append(NearlyPostconds,PostcondEnd,Postconds)
	    )
	),
	processPreconds(Preconds,Preconds,ProcPreconds,PrecondVars,[]),
	processPostconds(Postconds,Postconds,ProcPostconds,PostcondVars,[]),
	PreAndPostcondsList = [ProcPreconds|ProcPostconds],
	append(PrecondVars,PostcondVars,FullVars),
	FullList = [FullVars|PreAndPostcondsList],
	buildDomainLists(Rest,PredicateList,ClassList,RuleList,NumPredList,ClassHierList,PredicateSoFar,ClassSoFar,[[Name|FullList]|RuleSoFar],NumSoFar,HierSoFar).




% processPreconds(+Preconds,+PrecondsSoFar,-ProcessedPreconds,-PrecondVars,+VarSoFar)
% processes the preconditions spliting the list of preconditions into the individual preconds.  it also returns the variables associated with the preconditions of the predicate, which are required as part of the action

processPreconds(Preconds,PrecondsSoFar,ProcPreconds,PrecondVars,VarsSoFar) :-
	% first, identify the first precond and find its vars
	name(') ',PrecondEnd),
	matchExpression(FirstPrecond,PrecondEnd,RestPreconds,PrecondsSoFar),
	findVars(FirstPrecond,FirstVars,[]),
	processPreconds(Preconds,RestPreconds,ProcPreconds,PrecondVars,[FirstVars|VarsSoFar]).

processPreconds(Preconds,_,ProcPreconds,PrecondVars,PrecondVars) :-
	name('(And (',PrecondsBegin),
	append(PrecondsBegin,Preconds,ProcPreconds).

% findVars(+Precond,-FirstVars,+VarsSoFar)
% returns the variables associated with each precondition

findVars(Precond,FirstVars,VarsSoFar) :-
	% is this a condition that begins with not?  if so, elimate it
	name('Not (',Not),
	(   matchExpression([],Not,RealPrecond,Precond)
	;
	    Precond = RealPrecond
	),
	name(' ',Space),
	matchExpression(_,Space,PrecondVars,RealPrecond),
	findVarsNoName(PrecondVars,FirstVars,VarsSoFar).

% findVarsNoName(+PrecondVars,-Vars,+VarsSoFar)
% processes the variables once the precond name has been removed

findVarsNoName(PrecondVars,Vars,VarsSoFar) :-
	name(' ',Space),
	matchExpression(FirstVar,Space,RestVars,PrecondVars),
	findVarsNoName(RestVars,Vars,[FirstVar|VarsSoFar]).

findVarsNoName(LastVar,[LastVar|VarsSoFar],VarsSoFar).


% processPostconds(+Postconds,+PostcondsSoFar,-ProcessedPostconds,-PostcondVar,+VarsSoFar)
% this is similar to processPreconds/5, but deals with the postconditions (which are stated slightly differently, hence the need for the two predicates)

processPostconds(Postconds,PostcondsSoFar,ProcPostconds,PostcondVars,VarsSoFar) :-
	% if it is not the last postcond, it will end with a bracket and space
	name(') (',PostcondEnd),
	matchExpression(FirstPostcond,PostcondEnd,RestPostconds,PostcondsSoFar),
	name('(',LeftBracket),
	append(LeftBracket,RestPostconds,GoodRestPostconds),
	findVars(FirstPostcond,FirstVars,[]),
	processPostconds(Postconds,GoodRestPostconds,ProcPostconds,PostcondVars,[FirstVars|VarsSoFar]).

processPostconds(Postconds,PostcondsSoFar,ProcPostconds,[LastVars|PostcondVars],PostcondVars) :-
	% first, we have to deal with the last postcond:
	% we strip off the last three brackets:
%	name(')))',EndBrackets),
	name('))',EndBrackets),
	matchExpression(LastPostcond,EndBrackets,_,PostcondsSoFar),
	findVars(LastPostcond,LastVars,[]),
	name('(And (',PostcondsBegin),
	append(PostcondsBegin,Postconds,SemiPostconds),
	name('))',PostcondsEnd),
	matchExpression(ProcPostconds,PostcondsEnd,End,SemiPostconds),
	name(' ',Space),
	(   End = Space
	;
	    End = []
	).


% processArguments(+Arguments,-ArgumentList)
% splits a long string of arguments into lists of individual args

processArguments(Arguments,ArgumentList) :-
	processArguments(Arguments,[],ArgumentList).

processArguments(Arguments,ArgsSoFar,ArgumentList) :-
	name(') ',ArgEnd),
	matchExpression(Arg,ArgEnd,RestOfArgs,Arguments),
	name(')',RightBracket),
	append(Arg,RightBracket,FullArg),
	processArguments(RestOfArgs,[FullArg|ArgsSoFar],ArgumentList).

processArguments(Arguments,ArgsSoFar,[FullLastArg|ArgsSoFar]) :-
	name(')))',ArgsEnd),
	matchExpression(LastArg,ArgsEnd,_,Arguments),
	name(')',LeftBracket),
	append(LastArg,LeftBracket,FullLastArg).

% checkNumerical(+ArgumentList,+Name,-NewPred,+NonNumArgsSoFar)
% this will succeed if the argument list contains a numerical argument.  it will then rearrange the arguments in an appropriate manner.

checkNumerical([],_,_,_) :-
	fail.

checkNumerical([FirstArg|Rest],Name,NewPred,NonNumArgsSoFar) :-
	% first, we must find if this arg is numerical; otherwise we ignore
	name('Number ',NumID),
	matchExpression(_,NumID,Arg,FirstArg),
	% if so, we rearrange
	name(')',LeftBracket),
	matchExpression(_,LeftBracket,_,Arg),
	% add Rest to the non-num args and reverse to get full non-num (what if there is more than one num arg?)
	append(Rest,NonNumArgsSoFar,FullArgs),
	reverse(FullArgs,NonNumArgs),
	% build up the predicate by replacing the bracket, the name and the args, and then adding a left bracket at the end.
	name('(',RightBracket),
	append(RightBracket,Name,PredStart),
	makeArgs(NonNumArgs,GoodArgs,[]),
	append(PredStart,GoodArgs,MostPred),
	append(MostPred,LeftBracket,NewPred).

checkNumerical([FirstArg|Rest],Name,NewPred,NonNumArgsSoFar) :-
	checkNumerical(Rest,Name,NewPred,[FirstArg|NonNumArgsSoFar]).

% makeArgs(+NonNumericalArguments,-CorrectArgs,+CorrectArgsSoFar)
% this reformats the non-numerical arguments into the correct format

makeArgs([],MostPred,MostPred).

makeArgs([FirstArg|Rest],MostPred,MostPredSoFar) :-
	name(' ',Space),
	matchExpression(_,Space,GoodArg,FirstArg),
	append(Space,GoodArg,FullFirstArg),
	name(')',LeftBracket),
	matchExpression(ArgLessBracket,LeftBracket,_,FullFirstArg),
	append(ArgLessBracket,MostPredSoFar,NewPreds),
	makeArgs(Rest,MostPred,NewPreds).


% striping the situations out because noone really cares about these ...

stripFactSituations(Facts,NoSitFacts) :-
	name(' [',SitStart),
	name(']',SitEnd),
	matchExpression(BeforeSit,SitStart,AfterSitStart,Facts),
	matchExpression(_Sit,SitEnd,AfterSit,AfterSitStart),
	append(BeforeSit,AfterSit,NewFacts),
	stripFactSituations(NewFacts,NoSitFacts).

stripFactSituations(Facts,Facts).

%stripFactSituations([],RevNoSitFactList,NoSitFactList) :-
%	reverse(RevNoSitFactList,NoSitFactList).

%stripFactSituations([FirstFact|RestFacts],NoSitFactListSoFar,NoSitFactList) :-
%	name('[',SitStart),
%	name(']',SitEnd),
%	matchExpression(BeforeSit,SitStart,AfterSitStart,FirstFact),
%	matchExpression(Sit,SitEnd,AfterSit,AfterSitStart),
%	append(BeforeSit,AfterSit,FullFirstFact),
%	stripFactSituations(RestFacts,[FullFirstFact|NoSitFactListSoFar],NoSitFactList).

%stripFactSituations([FirstFact|RestFacts],NoSitFactListSoFar,NoSitFactList) :-
%	name('=',[Equals]),
%	member(Equals,FirstFact),
%	reverse(FirstFact,RevFirstFact),
%	name(' ',Space),
%	matchExpression(Value,Space,MostFirstFact,RevFirstFact),
%	matchExpression(_Situation,Space,GoodFirstFact,MostFirstFact),
%	name(' )',EndSit),
%	append(EndSit,GoodFirstFact,BetterFirstFact),
%	append(Value,BetterFirstFact,RevFullFirstFact),
%	reverse(RevFullFirstFact,FullFirstFact),
%	stripFactSituations(RestFacts,[FullFirstFact|NoSitFactListSoFar],NoSitFactList).

%stripFactSituations([FirstFact|RestFacts],NoSitFactListSoFar,NoSitFactList) :-
%	reverse(FirstFact,RevFirstFact),
%	name(' ',Space),
%	matchExpression(_Situation,Space,NewRevFirstFact,RevFirstFact),
%	reverse(NewRevFirstFact,NewFirstFact),
%	name(')',EndBracket),
%	append(NewFirstFact,EndBracket,GoodFirstFact),
%	stripFactSituations(RestFacts,[GoodFirstFact|NoSitFactListSoFar],NoSitFactList).

stripPredicateSituations([],RevNoSitPredList,NoSitPredList) :-
	reverse(RevNoSitPredList,NoSitPredList).

stripPredicateSituations([FirstPredicate|RestPredicates],NoSitListSoFar,NoSitPredList) :-
	name('(Sit-Var',Situation),
	append(Situation,_,SitList),
	member(SitList,FirstPredicate),
	deleteEl(FirstPredicate,SitList,NoSitFirstPredicate),
	stripPredicateSituations(RestPredicates,[NoSitFirstPredicate|NoSitListSoFar],NoSitPredList).

stripPredicateSituations([FirstPredicate|RestPredicates],NoSitListSoFar,NoSitPredList) :-
	stripPredicateSituations(RestPredicates,[FirstPredicate|NoSitListSoFar],NoSitPredList).

stripRuleSituations([],NoSitRuleList,NoSitRuleList).

stripRuleSituations([[RuleName,RuleArgs|[Preconds|Postconds]]|RestRules],NoSitListSoFar,NoSitRuleList) :-
	removeArgsSit(RuleArgs,NoSitRuleArgs),
	removeCondsSit(Preconds,NoSitPreconds),
	removeCondsSit(Postconds,NoSitPostconds),
	stripRuleSituations(RestRules,[[RuleName,NoSitRuleArgs|[NoSitPreconds|NoSitPostconds]]|NoSitListSoFar],NoSitRuleList).


removeArgsSit([RuleArgs],[NoSitRuleArgs]) :-
	name('?Sit',Sit),
	append(Sit,_,SitList),
	member(SitList,RuleArgs),
	deleteEl(RuleArgs,SitList,NoSitRuleArgs).

removeArgsSit(RuleArgs,RuleArgs).

removeCondsSit(Conds,NoSitConds) :-
	name('(And (',CondsBegin),
	matchExpression([],CondsBegin,RestConds,Conds),
	removeAllCondsSit(RestConds,RemovedSitConds),
	append(CondsBegin,RemovedSitConds,NoSitConds).

removeAllCondsSit(Conds,Removed) :-
	name(' ?Sit',Sit),
	name(')',EndBracket),
	matchExpression(BeforeSit,Sit,AfterSit,Conds),
	matchExpression(_,EndBracket,GoodAfter,AfterSit),
	append(BeforeSit,EndBracket,NewCond),
	append(NewCond,GoodAfter,NewConds),
	removeAllCondsSit(NewConds,Removed).
	
removeAllCondsSit(Conds,Conds).

stripNumericalPredicateSituations([],NoSitNumPredList,NoSitNumPredList).

stripNumericalPredicateSituations([FirstNumPred|RestNumPreds],NoSitListSoFar,NoSitNumPredList) :-
	name(' ?Situation',Situation),
	matchExpression(BeforeSit,Situation,AfterSit,FirstNumPred),
	append(BeforeSit,AfterSit,NewFirstNumPred),
	stripNumericalPredicateSituations(RestNumPreds,[NewFirstNumPred|NoSitListSoFar],NoSitNumPredList).

stripNumericalPredicateSituations([FirstNumPred|RestNumPreds],NoSitListSoFar,NoSitNumPredList) :-
	stripNumericalPredicateSituations(RestNumPreds,[FirstNumPred|NoSitListSoFar],NoSitNumPredList).



% ********************************************************************

% buildMetaList(-MetaList)
% this process reads the meta ontology and returns a list of the relevant lines

buildMetaList(MetaList) :-
	buildMetaList(MetaList,[]).

buildMetaList(MetaList,MetaSoFar) :-
	readLine(Line),!,
	checkMetaList(MetaList,MetaSoFar,Line).

% checkMetaList(-MetaListSoFar,+MetaList,+Line)
% checks to see if the line is the last, and then checks to see if it is relevant.

checkMetaList([Line|MetaList],MetaList,Line) :-
	extractLast(Line,end,Rest),
	checkMetaRelevant(Rest).

checkMetaList(MetaList,MetaList,Line) :-
	extractLast(Line,end,_).

checkMetaList(MetaList,MetaSoFar,Line) :-
	checkMetaRelevant(Line),
	buildMetaList(MetaList,[Line|MetaSoFar]).

checkMetaList(MetaList,MetaSoFar,_) :-
	buildMetaList(MetaList,MetaSoFar).

% checkMetaRelevant(+Line)
% succeeds if the line containes the declaration 'define-frame'

checkMetaRelevant(Line) :-
	name('(Define-Frame',FrameID),
	matchExpression([],FrameID,_,Line),!.

checkMetaRelevant(Line) :-
	name('(Define-Class',ClassID),
	matchExpression([],ClassID,_,Line),!.





% ********************************************************************

% subsidiary predicates


% % readLine(-Text)
% % reads the current stream until it reaches the end of the line

% readLine(Text):-
%    get_code(C),
%    addtoline(C, Text).

% % addtoline(+character,-text)
% % checks if this is a new line or end of file character.  if not builds adds it to the text of the line and returns to readLine.
 
% addtoline(C, []):-
%     newline(C).

% addtoline(C,[end]) :-
% 	endoffile(C).
 
% addtoline(C, [C|More]):-
%     readLine(More).

% readLine(Stream,Text):-
% 	get_code(Stream, C),
% 	addtoline(Stream, C, Text).

% addtoline(_, C, []):-
% 	newline(C).

% addtoline(_, C, [end]):-
% 	endoffile(C).

% addtoline(Stream, C, [C|More]):-
% 	readLine(Stream, More).

% newline(10).
 
% endoffile(-1).

% % extractLast(+List,?Last,-Rest)
% % returns the last element of a list (which may or may not be specified: if specified, it will fail if the last is not the specified element), together with the rest of the list (with the last element removed).

% extractLast(List,Last,Rest) :-
% 	extractLast(List,Last,[],RevRest),
% 	reverse(RevRest,Rest).
	
% extractLast([H],H,Rest,Rest).

% extractLast([H|T],Last,RestSoFar,Rest) :-
% 	extractLast(T,Last,[H|RestSoFar],Rest).

% matchExpression(-Before,+Identifier,-After,+Whole)
% succeeds if the identifier is contained within whole, and returns what comes before the identifier and what comes after.

% matchExpression(Before,Identifier,After,Whole) :-
% 	findPosition(Identifier,Whole,Pos1,Pos2),
% 	findBefore(Whole,Pos1,[],Before),
% 	findAfter(Whole,Pos2,After).

%sublistMe(List, List).
%sublistMe(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

%sublist_(Sub, _, Sub).
%sublist_([Head|Tail], _, Sub) :- sublist_(Tail, Head, Sub).
%sublist_([Head|Tail], X, [X|Sub]) :- sublist_(Tail, Head, Sub).


% findPosition(+Identifier,+Whole,-BeginPos,-AfterPos)
% returns the start and end positions of the identifier within whole

% findPosition([FirstID|Rest],Whole,Pos1,Pos1,EndPos) :-
% 	nth1(Pos1,Whole,FirstID),
% 	findPosition([FirstID|Rest],Whole,Pos1,EndPos).

% findPosition([Last],Whole,EndPos,EndPos) :-
% 	nth1(EndPos,Whole,Last).

% findPosition([FirstID|Rest],Whole,CurrentPos,EndPos) :-
% 	nth1(CurrentPos,Whole,FirstID),
% 	NextPos is CurrentPos + 1,
% 	findPosition(Rest,Whole,NextPos,EndPos).

% % findBefore(+Whole,+Position,+BeforeSoFar,-Before)
% % returns what comes before position in whole

% findBefore(_,1,RevBefore,Before) :-
% 	reverse(RevBefore,Before).

% findBefore([FirstWhole|RestWhole],Counter,BeforeSoFar,Before) :-
% 	NewCounter is Counter - 1,
% 	findBefore(RestWhole,NewCounter,[FirstWhole|BeforeSoFar],Before).

% % findAfter(+Whole,+Position,-After)
% % returns what comes after position in whole

% findAfter(Whole,Pos2,After) :-
% 	length(Whole,End),
% 	Start is Pos2 +1,
% 	findAfter(Whole,Start,End,[],After).

% findAfter(_,Last,End,RevAfter,After) :-
% 	Last is End + 1,
% 	reverse(RevAfter,After).

% findAfter(Whole,Counter,End,AfterSoFar,After) :-
% 	nth1(Counter,Whole,Element),
% 	NewCounter is Counter + 1,
% 	findAfter(Whole,NewCounter,End,[Element|AfterSoFar],After).

% flatten(+List,-FlatList)
% turns a list of lists into a flat list

flatten(List,FlatList) :-
	flatten(List,FlatList,[]).

flatten([],FlatList,FlatSoFar) :-
	reverse(FlatSoFar,FlatList).

flatten([FirstEl|Rest],FlatList,FlatListSoFar) :-
	% either it's a list
	(   reverse(FirstEl,FirstList),
	    append(FirstList,FlatListSoFar,NewFlat),
	    flatten(Rest,FlatList,NewFlat)
	;
	    % or it's an element
	    flatten(Rest,FlatList,[FirstEl|FlatListSoFar])
	).


%   whitespace (character space or tab).

spaceChar(C):- C = 32; C = 9.

% numChar(+Char)
%   Succeeds if character value C is a numerical digit.

numChar(C):- C @>= 48, C @=< 57.

% whitespace(+CharList)
%   Succeeds if character list CharList contains only
%   whitespace characters.

whitespace([H|T]):-
        spaceChar(H), whitespace(T).
whitespace([]):- !.


removeSublist(Sublist,List,NewList) :-
	sublist(List,Sublist,_,_,_),
	matchExpression(Before,Sublist,After,List),
	append(Before,After,NewList).




names([]).

names([FirstName|Rest]) :-
	FirstName = [FirstEl|_],
	number(FirstEl),
	name(Name,FirstName),
	write(Name),write(','),
	names(Rest).


names([FirstName|Rest]) :-
	names(FirstName),
	names(Rest).


% deleteEl(List,El,Ans) :- 
% 	deleteEl(List,El,[],Ans). 

% deleteEl([H|T],El,SoFar,Ans) :-
% 	H == El,
% 	append(SoFar,T,Ans).

% deleteEl([H|T],El,Ans,X) :- 
% 	append(Ans,[H],SoFar), 
% 	deleteEl(T,El,SoFar,X). 




