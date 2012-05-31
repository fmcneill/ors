:- use_module(library(lists)),use_module(library(random)).


% updateOnt(+Individual,+Fact)
% reads 'ont.in' line by line, finds the appropriate line to alter,
% and alters it accordingly


updateKIFOnt(Scenario,Assertions,Situation) :-
%	removeDoubles(Assertions,[],NoDupsAssertions),
	reverse(Assertions,RevAssertions),
	translateSituation(Situation,[],TransSituation),
	updateKIFOntRev(Scenario,RevAssertions,TransSituation).


updateKIFOntRev(Scenario,[],Situation) :-
	updateAllSituations(Scenario,Situation),
	write('The KIF ontology has been updated'),nl,nl.

updateKIFOntRev(Scenario,[[start]|OtherAssertions],Situation) :-
	updateKIFOntRev(Scenario,OtherAssertions,Situation).

updateKIFOntRev(Scenario,[FirstState|OtherAssertions],Situation) :-
	FirstState = [_ActionName,FirstAssertionList],
	processAssertions(Scenario,FirstAssertionList,Situation),
	updateKIFOntRev(Scenario,OtherAssertions,Situation).


processAssertions(_,[],_).

processAssertions(Scenario,[FirstAssertion|OtherAssertions],Situation) :-
	FirstAssertion =.. [PredName|_],
	name(PredName,PredNameChars),
	name('class',Class),
	matchExpression(_,Class,_,PredNameChars),
	processAssertions(Scenario,OtherAssertions,Situation).

processAssertions(Scenario,[FirstAssertion|OtherAssertions],Situation) :-
	translateFact(FirstAssertion,TransFirstAssertion),
	name(')',EndFact),
	name(' ',Space),
	reverse(TransFirstAssertion,RevFirst),
	matchExpression(_,EndFact,RestFirst,RevFirst),
	reverse(RestFirst,MostFirst),
	append(MostFirst,Space,MostFirstAssertion),
	append(MostFirstAssertion,Situation,NearlyFirstAssertion),
	append(NearlyFirstAssertion,EndFact,FullTransFirstAssertion),
	name(TransName,FullTransFirstAssertion),
	addFact(Scenario,TransName),
	processAssertions(Scenario,OtherAssertions,Situation).

addFact(Scenario,Fact) :-
	% sets 'ont.in' as the input stream and 'ont.out' as the output stream
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	% calls predicate 'performUpdate'
	name(Fact,FactChars),
	name(' ',Space),
	matchExpression(_PredicateName,Space,Individuals,FactChars),
	(   matchExpression(FirstIndividual,Space,_OtherIndividuals,Individuals),
	    name(FirstIndividualName,FirstIndividual)
	;
	    name(FirstIndividualName,Individuals)
	),
	performUpdate(FirstIndividualName,Fact),
	told,seen,
	copyFiles(Scenario).



% checkIndivsPresent(+Fact)
% sometimes the individuals involved in a fact are not mentioned in the ontology, having been learned from another agent.  we need to check that they are there, and, if not, add them.

checkIndivsPresent(Scenario,Fact) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	name(Fact,FactChars),
	name(' ',Space),
	matchExpression(_PredicateName,Space,Individuals,FactChars),
	% we need to find out all the individuals
	makeIndivList(Individuals,[],IndivList),
	checkAllIndivsPresent(IndivList,NewIndivList),
	addNewIndivs(NewIndivList),
	told,seen,
	copyFiles(Scenario).

makeIndivList(Individuals,IndivList,IndivsSoFar) :-
	name(' ',Space),
	matchExpression(FirstIndiv,Space,RestIndivs,Individuals),
	makeIndivList(RestIndivs,[FirstIndiv|IndivList],IndivsSoFar).

makeIndivList(LastIndiv,IndivList,[LastIndiv|IndivList]).


checkAllIndivsPresent(IndivList,FinalIndivList) :-
	% reads the first line of the current input stream
	readLine(Line),
	    % checks to see if this is the last line: if so, removes the 'end' identifier
	(   extractLast(Line,end,Rest),nl,
	    % checks what to do on the line (without the end identifier) and then terminates
	    checkThisIndivPresent(IndivList,Rest,FinalIndivList)
	;
	    % if it is not the last line, it checks the line and then recurses
	    checkThisIndivPresent(IndivList,Line,NewIndivList),
	    checkAllIndivsPresent(NewIndivList,FinalIndivList)
	).


checkThisIndivPresent(IndivList,Line,NewIndivList) :-
	name('Define-Frame ',DefFrame),
	name('Define-Individual ',DefIndiv),
	(   matchExpression(_,DefFrame,WholeIndiv,Line)
        ;
	    matchExpression(_,DefIndiv,WholeIndiv,Line)
	),
	name(' ',Space),
	matchExpression(IndivName,Space,_,WholeIndiv),
	deleteEl(IndivList,IndivName,NewIndivList),
	copyLine(Line).

checkThisIndivPresent(IndivList,Line,IndivList) :-
	copyLine(Line).


addNewIndivs([]).

addNewIndivs([FirstIndiv|Rest]) :-
	write('new indiv is '),write(FirstIndiv),
	addNewIndivs(Rest).

	
	





copyFiles(Scenario) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntOut),tell(OntIn),
	copyFile,
	told,seen.

copyFile :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyThisLine(Rest)
	;
	    copyThisLine(Line),
	    copyFile
	).


% performUpdate(+Individual,+Fact)
% reads a line, checks whether it is the one we are looking for, and processes it accordingly

performUpdate(Individual,Fact) :-
	% reads the first line of the current input stream
	readLine(Line),
	    % checks to see if this is the last line: if so, removes the 'end' identifier
	(   extractLast(Line,end,Rest),nl,
	    % checks what to do on the line (without the end identifier) and then terminates
	    checkForIndiv(Individual,Rest,Fact)
	;
	    % if it is not the last line, it checks the line and then recurses
	    checkForIndiv(Individual,Line,Fact),
	    performUpdate(Individual,Fact)
	).


% checkLine(+Individual,+Line,+Fact)
% checks to see whether the line is the definition of the correct agent and, if so, alters it.  Otherwise, it simply copies the line as it is.

checkForIndiv(Individual,Line,Fact) :-
	name('(Define-Frame ',Def),
	name(Individual,Indiv),
	append(Def,Indiv,DefID),
	matchExpression(_,DefID,After,Line),
	\+ checkAxPresent(Individual,Fact,Line),
	\+ checkSinglePresent(Individual,Fact,Line),
	updateFrame(Individual,DefID,After,Fact).

checkForIndiv(Individual,Line,Fact) :-
	name('(Define-Individual ',Def),
	name(Individual,Indiv),
	append(Def,Indiv,DefID),
	matchExpression(_,DefID,After,Line),
	\+ checkAxPresent(Individual,Fact,Line),
	\+ checkSinglePresent(Individual,Fact,Line),
	changeToFrame(Indiv,After,Fact).

checkForIndiv(_,Line,_) :-
	copyThisLine(Line).


updateFrame(Individual,DefID,MostLine,Fact) :-
	name(Fact,FactChars),
	name(' ',Space),
	matchExpression(PredName,Space,_Args,FactChars),
	name('(Money',PredName),
	matchExpression(_,PredName,_,MostLine),
	alterFact(Individual,PredName,Fact,MostLine,NewMost),
	append(DefID,NewMost,Line),
	name(LineName,Line),
	write(LineName),nl.


updateFrame(Individual,DefID,MostLine,Fact) :-
	name(') :Axioms',FactsEnd),
	matchExpression(Facts,FactsEnd,AfterFacts,MostLine),
	name(Fact,FactChars),
	name(Individual,IndivChars),
	matchExpression(BeforeIndiv,IndivChars,AfterIndiv,FactChars),
	name(' ',Space),
	matchExpression([],Space,NewAfterIndiv,AfterIndiv),
	append(BeforeIndiv,NewAfterIndiv,NonAxFact),
	append(Space,NonAxFact,NewFact),
	append(Facts,NewFact,NewFacts),
	append(NewFacts,FactsEnd,FullFacts),
	append(FullFacts,AfterFacts,NewAfter),
	append(DefID,NewAfter,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName),nl.



changeToFrame(Indiv,MostLine,Fact) :-
	name(' (',BeginClass),
	matchExpression(_,BeginClass,ClassPlusRest,MostLine),
	name(')',EndClass),
	matchExpression(Class,EndClass,_,ClassPlusRest),
	name(Fact,FactChars),
	name('(Define-Frame ',DefBeg),
	append(DefBeg,Indiv,DefID),
	name(' :Own-Slots ((Instance-Of ',FactsBegin),
	append(DefID,FactsBegin,PartLine1),
	append(PartLine1,Class,PartLine2),
	append(PartLine2,EndClass,PartLine3),
	append(PartLine3,EndClass,PartLine4),
	name(' :Axioms (',BeginAx),
	append(PartLine4,BeginAx,PartLine5),
	append(PartLine5,FactChars,PartLine6),
	append(PartLine6,EndClass,PartLine7),
	append(PartLine7,EndClass,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName),nl.




alterFact(Individual,PredName,Fact,MostLine,NewMost) :-
	name(Individual,IndivChars),
	name(' ',Space),
	append(PredName,Space,PredBegin),
	append(PredBegin,IndivChars,AxBegin),
	matchExpression(BeforeAxiom,AxBegin,AfterAxBegin,MostLine),
	name('])',RightBracket),
	matchExpression(_,RightBracket,AfterAxiom,AfterAxBegin),
	name(Fact,FactChars),
	append(BeforeAxiom,FactChars,NewStart),
	append(NewStart,AfterAxiom,NewMost).


alterFact(Individual,PredName,Fact,MostLine,NewMost) :-	
	matchExpression(BeforePred,PredName,AfterPredName,MostLine),
	name('])',RightBracket),
	matchExpression(_,RightBracket,AfterPred,AfterPredName),
	name(' ',Space),
	name(Individual,IndivChars),
	name(Fact,FactChars),
	append(IndivChars,Space,FullIndiv),
	matchExpression(BeforeIndiv,FullIndiv,AfterIndiv,FactChars),
	append(BeforeIndiv,AfterIndiv,GoodFact),
	append(BeforePred,GoodFact,GoodBegin),
	append(GoodBegin,AfterPred,NewMost).
	


translateFact(Fact,TransFact) :-
	Fact =.. [PredName|Args],
	name(PredName,PredNameChars),
	translateArg(PredNameChars,TransPred),
	translateAllArgs(Args,TransArgs,[]),
	createTransFact(TransPred,TransArgs,TransFact).

translateSituation([],RevTransSit,TransSit) :-
	insertCommas(RevTransSit,RevCommas),
	reverse(RevCommas,TransSitNoBrackets),
	name('[',LeftList),
	name(']',RightList),
	append(LeftList,TransSitNoBrackets,FirstTransSit),
	append(FirstTransSit,RightList,TransSit).

translateSituation([FirstSit|Rest],TransSitSoFar,TransSit) :-
	translateFact(FirstSit,TransFirstSit),
	reverse(TransFirstSit,RevFirstSit),
	append(RevFirstSit,TransSitSoFar,NewSitSoFar),
	translateSituation(Rest,NewSitSoFar,TransSit).

insertCommas(RevTransSit,RevCommas) :-
	name('(,)',FactEnd),
	name('( )',GoodFactEnd),
	matchExpression(BeforeFactEnd,FactEnd,AfterFactEnd,RevTransSit),
	append(BeforeFactEnd,GoodFactEnd,Fact1),
	append(Fact1,AfterFactEnd,NewTransSit),
	insertCommas(NewTransSit,RevCommas).

insertCommas(RevCommas,RevCommas).


translateAllArgs([],TransArgs,RevTransArgs) :-
	reverse(TransArgs,RevTransArgs).

translateAllArgs([FirstArg|Others],TransArgs,TransSoFar) :-
	name(FirstArg,FirstArgChars),
	translateArg(FirstArgChars,TransFirstArg),
	reverse(TransFirstArg,RevFirst),
	translateAllArgs(Others,TransArgs,[RevFirst|TransSoFar]).


createTransFact(TransPred,ArgsList,TransFact) :-
	name('(',RightBracket),
	append(RightBracket,TransPred,Begin),
	reverse(Begin,RevBegin),
	addArgs(RevBegin,ArgsList,FullPred),
	name(')',LeftBracket),
	append(FullPred,LeftBracket,TransFact).

addArgs(FullPred,[],RevFullPred) :-
	reverse(FullPred,RevFullPred).

addArgs(Begin,[FirstArg|RestArgsList],FullPred) :-
	name(' ',Space),
	append(Space,Begin,FullBegin),
	append(FirstArg,FullBegin,NewSoFar),
	addArgs(NewSoFar,RestArgsList,FullPred).


updateAllSituations(Scenario,Situation) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	checkForSituation(Situation),
	seen,told,
	copyFiles(Scenario).

checkForSituation(Situation) :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    updateThisSituation(Situation,Rest)
	;
	    updateThisSituation(Situation,Line),
	    checkForSituation(Situation)
	).


checkAxPresent(_,Fact,Line) :-
	name(Fact,FactName),
	name(' [',BeginSit),
	matchExpression(BeforeSit,BeginSit,_,FactName),!,
	matchExpression(_,BeforeSit,_,Line).

checkSinglePresent(Individual,Fact,Line) :-
	name(Fact,FactName),
	name(' [',BeginSit),
	matchExpression(BeforeSit,BeginSit,_,FactName),
	name(Individual,IndivName),
	matchExpression(BeforeIndiv,IndivName,AfterIndiv,BeforeSit),
	append(BeforeIndiv,AfterIndiv,NewFact),
	matchExpression(_,NewFact,_,Line).

updateThisSituation(Situation,Line) :-
	checkForAllSits(Situation,Line,[],NewLine),
	name(NewLineName,NewLine),
	write(NewLineName),nl.


checkForAllSits(Situation,Line,NewLineSoFar,NewLine) :-
	name('[',SitStart),
	name(']',SitEnd),
	matchExpression(BeforeSit,SitStart,AfterSitStart,Line),
	matchExpression(_Sit,SitEnd,AfterSit,AfterSitStart),
	append(BeforeSit,Situation,NewBefore),
	append(NewLineSoFar,NewBefore,NewNewLine),
	checkForAllSits(Situation,AfterSit,NewNewLine,NewLine).

checkForAllSits(_,Line,NewLineSoFar,NewLine) :-
	append(NewLineSoFar,Line,NewLine).



%removeDoubles([_],NoDups,NoDups).

%removeDoubles([FirstSit|Rest],NoDupsSoFar,NoDupAssertions) :-
%	write('first sit is '),write(FirstSit),nl,
%	compareAllFacts(FirstSit,Rest,[],NoDupsFirstSit),
%	append(NoDupsFirstSit,NoDupsSoFar,BetterSoFar),
%	removeDoubles(Rest,BetterSoFar,NoDupAssertions).

%compareAllFacts(FirstSit,Rest,NoDupsSoFar,NoDups) :-
%	name(']',FactEnd),
%	matchExpression(FirstFact,FactEnd,RestFacts,FirstSit),
%	name(' [',SitBegin),
%	matchExpression(GoodFact,SitBegin,_Sit,FirstFact),
%	removeThisFact(GoodFact,Rest,GoodRest),
%	compareAllFacts(RestFacts,GoodRest,NoDups)


	






