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
	findall(RepairInfo, bridgeRule(_,[_,[UnknownPred_name|RepairInfo]]), RepairInfoList),!,
	RepairInfoList = [Repair|_],
	Repair = [Arity,LHSArgsClassList|Rest],
	length(UnknownArgs,UnknownArity),
	Arity = UnknownArity,
	nl,write('Predicate found in a bridge rule : '),write(UnknownPred_name),nl,nl,
	write('An unknown term has the same arity as in a bridge rule : '),write(UnknownArity),nl,nl, 
	bridgeRule([LHS,RHS],[_,[UnknownPred_name|Repair]]),
	write('RHS : '),write(RHS),nl,nl,
	checkRepairType(UnknownPred_name,RepairType),
	checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,Action).
	%checkOntology(RHS,Unknown,RepairType,Rest,Known,UnknownArgsClassList,Agent).
	%write(Arity),nl,nl,write(Arity_Update).

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
checkArgsClass(RHS,_LHS,Unknown,UnknownPred_name,UnknownArgs,_,_,[precondAA],Rest,Agent,Known,Action) :-
	Unknown=..[_Class|UnknownArgsClassList],
	checkOntology(RHS,Unknown,[precondAA],Rest,Known,UnknownArgsClassList,Agent,Action).
	

%check arguments class of unknown predicate whether it contains the same class as in a bridge rule.
checkArgsClass(RHS,LHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action) :-
	findArgsClassHis(pa,Agent,Unknown,UnknownPArgsClassList),!,
	reverse(UnknownPArgsClassList,UnknownArgsClassList),
	LHSArgsClassList = UnknownArgsClassList,
	write('An unknown term has the same argument classes: '),
	write(UnknownArgsClassList),nl,nl,
	checkUninstantiated(Unknown,LHS,UnknownArgs,InstantiatedUnknown),
	%write(InstantiatedUnknown),nl,nl,
	checkOntology(RHS,InstantiatedUnknown,RepairType,Rest,Known,UnknownArgsClassList,Agent,_Action).

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


%class not match return not found
checkArgsClass(RHS,Unknown,UnknownPred_name,UnknownArgs,LHSArgsClassList,UnknownArgsClassList,RepairType,Rest,Agent,Known,_Action) :-
	Known= no,	
	write('Not found in a bridge rule'),nl,nl.

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

%ProcondAA : Class
checkOntology(RHS,Unknown,[precondAA],Rest,Known,UnknownArgsClassList,Agent,Action):-
	UnknownArgsClassList = [X|Class],
	RHS_Class =..[class,X,RHS],
	matchesPrecond(RHS_Class,_,_,Action,_,_),
	Known = Unknown,
	nl,write('Found an unknown term in a bridge rule :'),
	write(Known),nl,nl.
	

%not found
checkOntology(RHS,RSQ,_,Repair,Known,UnknownArgsClassList,Agent,_Action) :-	
	Known= no,
	write('Not found in a bridge rule'),nl,nl.

