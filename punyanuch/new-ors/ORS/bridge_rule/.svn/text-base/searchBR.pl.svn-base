checkBridge(Unknown,Known,Agent,Action) :-
	get_current_scenario_path(CurrentScenarioPath),
	atom_concat(CurrentScenarioPath,'/bridgerule.pl',BridgeRule),
	[BridgeRule],
	nl,nl,write('Looking up a bridge rule...'),nl,nl,
	searchBridge(Unknown,Known,Agent,Action).
	

searchBridge(Unknown,Known,Agent,Action) :-
	Unknown =.. [UnknownPred_name|UnknownArgs],
	checkPredicate(Unknown,Known,UnknownPred_name,UnknownArgs,RepairInfoList,LHSArgsList,Outcome,Agent,Action).

searchBridge(Unknown,Known,Agent,Action) :-
	Known= no,
	write('Not found in a bridge rule'),nl,nl.

%Check whether the unknown predicate exists in a bridge rule.
%Check arity that whether it matches in a bridge rule

checkPredicate(Unknown,Known,UnknownPred_name,UnknownArgs,RepairInfoList,LHSArgsClassList,Outcome,Agent,Action) :-
	findall(RepairType, bridgeRule(_,[RepairType,[UnknownPred_name|_]]), RepairTypeList),!,
	checkEachBridgeRule(Unknown,Known,UnknownPred_name,UnknownArgs,RepairTypeList,LHSArgsClassList,Outcome,Agent,Action).
	

checkEachBridgeRule(Unknown,Known,UnknownPred_name,UnknownArgs,[FirstRepairType|_Rest],LHSArgsClassList,Outcome,Agent,Action):-
	findall(RepairInfo, bridgeRule(_,[FirstRepairType,[UnknownPred_name|RepairInfo]]), RepairInfoList),
	checkEachRepairInfo(Unknown,Known,UnknownPred_name,UnknownArgs,RepairInfoList,LHSArgsClassList,Outcome,Agent,Action,FirstRepairType).
	

checkEachBridgeRule(Unknown,Known,UnknownPred_name,UnknownArgs,[_FirstRepairType|Rest],LHSArgsClassList,Outcome,Agent,Action):-
	checkEachBridgeRule(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action).

checkEachRepairInfo(Unknown,Known,UnknownPred_name,UnknownArgs,[FirstRepairInfo|_Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType):-
	%findall(RepairInfo, bridgeRule(_,[FirstRepairType,[UnknownPred_name|RepairInfo]]), RepairInfoList),!,	
	%nl,nl,write(FirstRepairInfo),nl,
	%FirstRepairInfo = [Repair|_],
	FirstRepairInfo = [Arity,LHSArgsClassList|Rest],
	length(UnknownArgs,UnknownArity),
	Arity = UnknownArity,
	nl,write('Predicate found in a bridge rule : '),write(UnknownPred_name),nl,nl,
	write('An unknown term has the same arity as in a bridge rule : '),write(UnknownArity),nl,nl, 
	findall(Rule, bridgeRule(Rule,[RepairType,[UnknownPred_name|FirstRepairInfo]]), RuleList),!,
	checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,RuleList,LHSArgsClassList,Outcome,Agent,Action,RepairType,Rest).	


checkEachRepairInfo(Unknown,Known,UnknownPred_name,UnknownArgs,[_FirstRepairInfo|Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType):-
	checkEachRepairInfo(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action,RepairType).

checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,[FirstRule|_Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType,Rest) :-
	FirstRule = [LHS,RHS],
	checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,[RepairType],Rest,Agent,Known,Action).


checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,[_FirstRule|Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType) :-
	checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action,RepairType).


%If not exists return notfound
checkPredicate(Unknown,Known,UnknownPred_name,UnknownArgs,_,_,Outcome,Agent) :-
	Known= no,
	write('Not found in a bridge rule'),nl,nl.

%find RepairType which store corespond to an unknown predicate.
checkRepairType(UnknownPred_name,RepairType):-
	%write(Pred_name),nl,nl,
	findall(X, bridgeRule(_,[X,[UnknownPred_name|_]]),RepairType),
	write('Repair Type: '),write(RepairType),nl,nl.
	%RepairInfoList = [Repair|_],
	%write(RepairTypeList).

%check arguments class for precondAA
checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,_,_,[precondAA],Rest,Agent,Known,Action) :-
	Unknown=..[_Class|UnknownArgsClassList],
	UnknownArgsClassList = [X,Class|_],
	LHS = Class,
	write('RHS term in a bridge rule : '),write(RHS),nl,nl,
	write('Checking the PA''s ontology...'),nl,nl,
	checkOntology(RHS,Unknown,[precondAA],Rest,Known,UnknownArgsClassList,Agent,Action).
	

%check arguments class of unknown predicate whether it contains the same class as in a bridge rule.
checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action) :-
	findArgsClassHis(pa,Agent,Unknown,UnknownPArgsClassList),!,
	reverse(UnknownPArgsClassList,UnknownArgsClassList),
	LHSArgsClassList = UnknownArgsClassList,
	write('An unknown term has the same argument classes: '),
	write(UnknownArgsClassList),nl,nl,
	checkUninstantiated(Unknown,LHS,UnknownArgs,InstantiatedUnknown),
	write('RHS term in a bridge rule : '),write(RHS),nl,nl,
	%write(InstantiatedUnknown),nl,nl,
	write('Checking the PA''s ontology...'),nl,nl,
	checkOntology(RHS,InstantiatedUnknown,RepairType,Rest,Known,UnknownArgsClassList,Agent,_Action).

checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,_,_,[precondAA],Rest,Agent,Known,Action) :-
	write('Class Precondition did not match with a bridge rule'),nl,nl,
	Known = no.





checkUninstantiated(Unknown,LHS,UnknownArgs,InstantiatedUnknown) :-
	LHS =..[Pred_name|LHSInstArgs],
	findPos(UnknownArgs,uninstantiated,RevPos,1),!,
	remove_at(_,UnknownArgs,RevPos,RevList),
	findIndividual(RevPos,LHS,Arg),
	insert_at(Arg,RevList,RevPos,UninsList),
	append([Pred_name],UninsList,InstList),
	InstantiatedUnknown =..InstList.

checkUninstantiated(Unknown,LHS,UnknownArgs,InstantiatedUnknown) :-
	InstantiatedUnknown = Unknown.



%checkOntology : look up the PA ontology whether it contains the RHS term of the bridge rule and some repair information depend on repair type

% PredicateAA
%check RHS term in ontology and check whether they contain same class.
%LHS and RHS term need to have the same class argument for predicateAA.
checkOntology(RHS,Unknown,[predicateAA],_,Known,UnknownArgsClassList,_,_Action) :-
	checkFullPredExists(RHS),!,
	findArgsClassMine(RHS,ExPArgsClassList),!,
	reverse(ExPArgsClassList,ArgsClassList),
	UnknownArgsClassList = ArgsClassList,
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

%PredicateA
%check RHS term in ontology and check whether they contain same class.
%LHS and RHS term need to have the same class argument for predicateAA.
checkOntology(RHS,Unknown,[predicateA],_,Known,UnknownArgsClassList,_,_Action) :-
	checkFullPredExists(RHS),!,
	findArgsClassMine(RHS,ExPArgsClassList),!,
	reverse(ExPArgsClassList,ArgsClassList),
	UnknownArgsClassList = ArgsClassList,
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

%DomainA
%check RHS term in ontology and check whether they contain same class.
%repair information subclass(X,Y) need to be found in the PA's ontology.

checkOntology(RHS,Unknown,[domainA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	checkFullPredExists(RHS),!,
	Repair = [_,SubType|_],
	SubType =..[_,Class1,Class2|_],
	checkSubClass(pa,Agent,Class1,Class2),
	RHS =..[RHSPred_name|RHSArgs],
	Unknown =..[UnknownPred_name|UnknownArgs],
	findArgsClassMine(RHS,ExArgsClassList),!,
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

%DomainAA
%check RHS term in ontology and check whether they contain same class.
%repair information subclass(X,Y) need to be found in the PA's ontology.

checkOntology(RHS,Unknown,[domainAA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	checkFullPredExists(RHS),!,
	Repair = [_,SubType|_],
	SubType =..[_,Class1,Class2|_],
	checkSubClass(pa,Agent,Class1,Class2),
	RHS =..[RHSPred_name|RHSArgs],
	Unknown =..[UnknownPred_name|UnknownArgs],
	findArgsClassMine(RHS,ExArgsClassList),!,
	%reverse(ExPArgsClassList,ArgsClassList),
	%ArgsList = ArgsClassList,
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

%propositionalAA
%check RHS term in ontology and check whether they contain same class.
%repair information : added argument class  need to be found in the PA's ontology 
checkOntology(RHS,Unknown,[propositionalAA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	Repair = [number|_],
	checkFullPredExists(RHS),!,
	findArgsClassMine(RHS,ExPArgsClassList),!,
	%subclass(Subclass,Class),
	reverse(ExPArgsClassList,ArgsClassList),
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.


checkOntology(RHS,Unknown,[propositionalAA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	Repair = [Subclass|_],	
	checkFullPredExists(RHS),!,
	findArgsClassMine(RHS,ExPArgsClassList),!,
	subclass(Subclass,Class),
	reverse(ExPArgsClassList,ArgsClassList),
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.



%propositionalA
%check RHS term in ontology and check whether they contain same class.
%repair information : added argument class  need to be found in the PA's ontology 
checkOntology(RHS,Unknown,[propositionalA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	checkFullPredExists(RHS),	
	Repair = [number|_],
	findArgsClassMine(RHS,ExPArgsClassList),!,
	%subclass(Subclass,Class),
	reverse(ExPArgsClassList,ArgsClassList),
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

checkOntology(RHS,Unknown,[propositionalA],Repair,Known,UnknownArgsClassList,Agent,_Action) :-
	checkFullPredExists(RHS),
	Repair = [Subclass|_],
	findArgsClassMine(RHS,ExPArgsClassList),!,
	subclass(Subclass,Class),
	reverse(ExPArgsClassList,ArgsClassList),
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.


%switchArgs
%check RHS term in ontology and check whether they contain same class.
%repair information : SwitchList
% SwitchList need to match both RHS and LHS
checkOntology(RHS,Unknown,[switchArgs],[SwitchList|_],Known,UnknownArgsClassList,Agent,_Action) :-
	checkFullPredExists(RHS),!,
	SwitchList = [RHS_args,LHS_args|_],
	findArgsClassMine(RHS,ExPArgsClassList),!,
	reverse(ExPArgsClassList,ArgsClassList),
	RHS_args = ArgsClassList,
	LHS_args = UnknownArgsClassList,
	Known =Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.

%PrecondAA : Class
%We decided not to search for Precondtion refinement because most cases will fail. 
%We cannot find the extra predition in the ontology. 
checkOntology(RHS,Unknown,[precondAA],Rest,Known,UnknownArgsClassList,Agent,Action):-
	nl,write('RHS term is not found in the ontology'),nl,nl.
%	UnknownArgsClassList = [X,UnknownClass|_],
%	class(X,CurrentClass),
%	Class = CurrentClass,
%	RHS_Class =..[class,X,RHS],
%	subclass(UnknownClass,RHS),
%	matchesPrecond(RHS_Class,_,_,Action,_,_),
%	Known = Unknown,
%	nl,write('Found an unknown term in a bridge rule :'),
%	write(Known),nl,nl.
	

%not found
checkOntology(RHS,Unknown,_,Repair,Known,UnknownArgsClassList,Agent,_Action) :-	
	write('RHS term is not found in the ontology'),nl,nl,!,
	
	checkLinkBridgeRule(RHS,Unknown,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action),!.	
	%Known= no,
	%write('RHS term is not found in the ontology'),nl,nl.


%not found
checkOntology(RHS,Unknown,_,Repair,Known,UnknownArgsClassList,Agent,_Action) :-	
	Known= no,
	write('RHS term is not found in the ontology'),nl,nl.

% for checking the link between each rules : useful for detecting compound mismatch.
checkLinkBridgeRule(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action) :-
	Flag = 1,
	write('Checking the link between each bridge rule...'),nl,nl,
	findall(PossibleLink, bridgeRule([RHS,PossibleLink],[_,[_|_]]),PossibleLinkList),
	checkEachLink(PossibleLinkList,RHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action).
	
checkLinkBridgeRule(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action) :-
	write('No link was found'),nl,nl,
	Known =no.

checkEachLink([First|_Rest],LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action):-
	LHS =..[Pred_name|Args],
	findall(RepairType, bridgeRule(_,[RepairType,[Pred_name|_]]), RepairTypeList),!,
	checkEachBridgeRule(LHS,Known,Pred_name,Args,RepairTypeList,LHSArgsClassList,Outcome,Agent,Action,Unknown,First).

checkEachLink([_First|Rest],LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action):-
	checkEachLink(Rest,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action).





checkEachBridgeRule(LHS,Known,UnknownPred_name,UnknownArgs,[FirstRepairType|_Rest],LHSArgsClassList,Outcome,Agent,Action,Unknown,RHS):-
	findall(RepairInfo, bridgeRule(_,[FirstRepairType,[UnknownPred_name|RepairInfo]]), RepairInfoList),
	checkEachRepairInfo(LHS,Known,UnknownPred_name,UnknownArgs,RepairInfoList,LHSArgsClassList,Outcome,Agent,Action,FirstRepairType,Unknown,RHS).
	

checkEachBridgeRule(LHS,Known,UnknownPred_name,UnknownArgs,[_FirstRepairType|Rest],LHSArgsClassList,Outcome,Agent,Action,Unknown,RHS):-
	checkEachBridgeRule(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action,Unknown,RHS).

checkEachRepairInfo(LHS,Known,UnknownPred_name,UnknownArgs,[FirstRepairInfo|_Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType,Unknown,RHS):-
	%findall(RepairInfo, bridgeRule(_,[FirstRepairType,[UnknownPred_name|RepairInfo]]), RepairInfoList),!,	
	%nl,nl,write(FirstRepairInfo),nl,
	%FirstRepairInfo = [Repair|_],
	write('Found the related links:  '),nl,
	write(Unknown), write(' --> '), write(LHS),nl,
	write(LHS), write('-->'), write(RHS),nl,nl,
	write('Then it is possible to have: '), nl,
	write(Unknown), write(' --> '),write(RHS),nl,nl,
	FirstRepairInfo = [Arity,LHSArgsClassList|Rest],
	length(UnknownArgs,UnknownArity),
	Arity = UnknownArity,
	write('Perform checking this link... '),nl,nl,
	%write('An unknown term has the same arity as in a bridge rule : '),write(UnknownArity),nl,nl, 
	findall(Rule, bridgeRule(Rule,[RepairType,[UnknownPred_name|FirstRepairInfo]]), RuleList),!,
	checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,RuleList,LHSArgsClassList,Outcome,Agent,Action,RepairType,Unknown,Rest).	


checkEachRepairInfo(LHS,Known,UnknownPred_name,UnknownArgs,[_FirstRepairInfo|Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType,Unknown):-
	checkEachRepairInfo(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action,RepairType,Unknown).

checkEachRule(LHS,Known,UnknownPred_name,UnknownArgs,[FirstRule|_Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType,Unknown,Rest) :-
	FirstRule = [NLHS,NRHS],!,
	%write('RHS : '),write(NRHS),nl,nl,
	checkArgsClass(NRHS,NLHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,[RepairType],Rest,Agent,Known,Action).


checkEachRule(LHS,Known,UnknownPred_name,UnknownArgs,[_FirstRule|Rest],LHSArgsClassList,Outcome,Agent,Action,RepairType) :-
	checkEachRule(Unknown,Known,UnknownPred_name,UnknownArgs,Rest,LHSArgsClassList,Outcome,Agent,Action,RepairType).
