:- use_module(library(lists)),use_module(library(random)).
:- consult('~/trunk/andriana/new-ors/ORS/negotiation.pl').


%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%

% consultMeta(+Scenario,+RefineType,+OntType,+RefineInfo,-Outcome)
% called in translation, 
% consults metaOnt.in to read data about protection and translateToProlog to use convertName function

consultMeta(Scenario,RepairType,OntType,RepairInfo,Outcome) :-
        nl,nl,write('* I must check if the repair can be performed.'),nl,nl,
        find_ontology_path(Scenario,ScenarioPath),
        atom_concat(ScenarioPath,'/metaOnt',MetaPlPath),
        consult(MetaPlPath),
        get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translateToProlog', TranslatePath),
        consult(TranslatePath),
        checkProtection(Scenario,RepairType,OntType,RepairInfo,Outcome,Utility).

        
% consultMetaAgent(+Scenario,+RefineType,+OntType,+RefineInfo,-Outcome,+Agent,-Utility)
% called by spa
% consults metaOnt.in to read data about protection and translateToProlog to use convertName function  

consultMetaAgent(Scenario,RepairType,OntType,RepairInfo,Outcome,Agent,Utility) :-
	nl,nl,write('* I must check if the repair can be performed.'),nl,nl,
	
		%spa's prolog ontology

	%( 
        %get_absolute_path('/agent_environment/scenarios/', ScenPath),
        %atom_concat(ScenPath, Scenario, ScPath),
        %atom_concat(ScPath, '/SPA/', ScPath2),
        %spa_name(SpaName),
        %atom_concat(ScPath2, SpaName, ScPath3),
        %atom_concat(ScPath3, '/', ScPath4),
        %atom_concat(ScPath4, 'AgentThy', ThyPath),
	%atom_concat(ScPath4, 'AgentSig', SigPath),
	%get_absolute_path('/agent_environment/SPA/spa', SpaAbsolPath),
	%reconsult(ThyPath),
	%reconsult(SigPath),
	%reconsult(SpaAbsolPath),

	%write('-------------consulted!')
	%);

	%spa's kif ontology	
	%(
	find_ontology_path_agent(Scenario,Agent,ScPath),
	
	atom_concat(ScPath,'/metaOnt',MetaPlPath),
        consult(MetaPlPath),
        get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translateToProlog', TranslatePath),
        consult(TranslatePath),
        checkProtectionAgent(Scenario,RepairType,OntType,RepairInfo,Outcome,Agent,Utility).
	%).

        
                
% checkProtection(+Scenario,+RefineType,+OntType,+RefineInfo)
% checks protection according to the type of refinement, performs repair if it is possible  

% PRECONDITION REFINEMENT
checkProtection(Scenario,precondAA,onto,[Rule,Precond],[protected,precondAA,[Rule,Precond],Level],Utility) :-
	protectPredicate(PredicateList),
	lowerName(Rule,LowRuleChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
checkProtection(Scenario,precondAA,onto,[Rule,Precond],[_protected,precondAA,[Rule,Precond],Level],Utility) :-

	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is.'),write(Utility),nl,nl,
	setInitiatorUtility(Utility).


% PRECONDITION DOMAIN REFINEMENT 
checkProtection(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[protected,precondAA,[Rule,RightClass,WrongClass],Level],Utility) :-
  	protectPredicate(PredicateList),
  	protectArgument(ArgumentList),
        lowerName(Rule,LowRuleChars),
        lowerName(WrongClass,LowWrongClassChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(NewRule,NewWrongClass,ArgumentType,_),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
checkProtection(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[_protected,precondAA,[Rule,RightClass,WrongClass],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl.
	

%% PROPOSITIONAL REFINEMENT
checkProtection(Scenario,propositionalAA,onto,[Pred,ArgType,ArgValue,Position,Arity],[protected,propositionalAA,[Pred,ArgType,ArgValue,Position,Arity],Level],Utility) :-
        protectPredicate(PredicateList),
        lowerName(Pred,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateArity,Comma,HighProtection,RightBracket,ArityHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateArity,Comma,LowProtection,RightBracket,ArityLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        %% predicate-arity
        member(ArityHighName,PredicateList),
        =(Level,HighProtectionName),
	setInitiatorUtility(Utility),	
	Utility=0,
        write('* Protection used: '),write(ArityHighName),nl;
        member(ArityLowName,PredicateList),
        =(Level,LowProtectionName),
	setInitiatorUtility(Utility),	
	Utility=1,
        write('* Protection used: '),write(ArityLowName),nl
        ),!.

checkProtection(Scenario,propositionalAA,onto,[Pred,ArgType,ArgValue,Position,Arity],[_protected,propositionalAA,[Pred,ArgType,ArgValue,Position,Arity],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).


%% PROPOSITIONAL ABSTRACTION
checkProtection(Scenario,propositionalA,onto,[Pred,ArgPosition],[protected,propositionalA,[Pred,ArgPosition],Level],Utility) :-
        protectPredicate(PredicateList),
        protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateArity,Comma,HighProtection,RightBracket,ArityHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateArity,Comma,LowProtection,RightBracket,ArityLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),        
	write('* Protection used: '),write(AllLowName),nl;
        %% predicate-arity
        member(ArityHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(ArityHighName),nl;
        member(ArityLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(ArityLowName),nl;
        %% argument-all
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl
        ),!.



checkProtection(Scenario,propositionalA,onto,[Pred,ArgPosition],[_protected,propositionalA,[Pred,ArgPosition],Level],Utility) :-
	Level=0,
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).



% DOMAIN ABSTRACTION
checkProtection(Scenario,domainA,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[protected,domainA,[Pred,OldType,NewType,NewTypeSuperType,Position],Level],Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,HighProtection,RightBracket,TypeHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,LowProtection,RightBracket,TypeLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        %% argument-all
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl;
        %% argumet-class
        member(TypeHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeHighName),nl;
        member(TypeLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeLowName),nl
        ),!.
checkProtection(Scenario,domainA,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[_protected,domainA,[Pred,OldType,NewType,NewTypeSuperType,Position],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NEW ADDED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% CHANGE INSTANCE DEFINITION
checkProtection(Scenario,changeInstDef,onto,[Item,WrongClass,Class],[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,HighProtection,RightBracket,TypeHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,LowProtection,RightBracket,TypeLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        %% argument-all
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl;
        %% argumet-class
        member(TypeHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeHighName),nl;
        member(TypeLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeLowName),nl
        ),!.

checkProtection(Scenario,changeInstDef,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[_protected,no,Level],Agent,Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% PREDICATE AA
checkProtection(Scenario,predicateAA,onto,[OldPred,NewPred,Rule],[protected,predicateAA,[OldPred,NewPred,Rule],Level],Utility) :-
	protectPredicate(PredicateList),
        lowerName(OldPred,LowPredChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
	(
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
checkProtection(Scenario,predicateAA,onto,[OldPred,NewPred,Rule],[_protected,predicateAA,[OldPred,NewPred,Rule],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).





 %PREDICATE A       
checkProtection(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[protected,predicateA,[OldPred,NewPred,Rule],Level],Utility) :-
	protectPredicate(PredicateList),
        lowerName(OldPred,LowPredChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
	(
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
checkProtection(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[_protected,predicateA,[OldPred,NewPred,Rule],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).


   

checkProtectionAgent(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[protected,no,Level],Agent, Utility) :-
	protectPredicate(PredicateList),
        lowerName(NewPred,LowPredChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
	(
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllLowName),nl       
	),!.

checkProtectionAgent(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[_protected,no,Level],Agent, Utility) :-
	Level=0,
write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).





% SWITCH ARGUMENTS
checkProtection(Scenario,switchArgs,onto,PredName,[protected,switchArgs,PredName,Level],Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(PredName,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        name('1',Position1),
        name('2',Position2),
        %(protect(NewPred,predicateAll,_)
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,Position1,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
	joinArgument(LeftBracket,LowPredChars,Comma,Position1,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,Position2,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHigh2Name),	
        joinArgument(LeftBracket,LowPredChars,Comma,Position2,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLow2Name),
        (
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl;
        member(AllArgHigh2Name,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHigh2Name),nl;
        member(AllArgLow2Name,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLow2Name),nl     
        ),!.
checkProtection(Scenario,switchArgs,onto,PredName,[_protected,switchArgs,PredName,Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).




% REMOVE PRECONDITION
checkProtection(problemPrecond,[Precond,_,FirstIndividual],Line,[protected,problemPrecond,[Precond,_,FirstIndividual],Level],Utility) :-
	protectPredicate(PredicateList),
	lowerName(Predcond,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.

checkProtection(problemPrecond,[Precond,_,FirstIndividual],Line,[_protected,problemPrecond,[Precond,_,FirstIndividual],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).



check(problemPrecond,[_,Precond,FirstIndividual],Line,[protected,problemPrecond,[Precond,_,FirstIndividual],Level],Utility) :-
	protectPredicate(PredicateList),
	lowerName(Predcond,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
check(problemPrecond,[_,Precond,FirstIndividual],Line,[_protected,problemPrecond,[Precond,_,FirstIndividual],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).



% REMOVE POSTCONDITION
checkprotection(removePostcond,[PredName,RuleName],Line,[protected,removePostcond,[PredName,RuleName],Level],Utility) :-
	protectPredicate(PredicateList),
	lowerName(PredName,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(NewRule,NewWrongClass,ArgumentType,_),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.

checkprotection(removePostcond,[PredName,RuleName],Line,[_protected,removePostcond,[PredName,RuleName],Level],Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).



% NEGATE POSTCONDITION
checkProtection(negatePrecond,[RuleName,PrecondName,negToPos],Line,[protected,negatePrecond,[RuleName,PrecondName,negToPos],Level], Utility) :-
	protectPredicate(PredicateList),
	lowerName(PrecondName,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(NewRule,NewWrongClass,ArgumentType,_),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.

checkProtection(negatePrecond,[RuleName,PrecondName,negToPos],Line,[_protected,negatePrecond,[RuleName,PrecondName,negToPos],Level], Utility) :-
	Level=0,	
	write('**I have no protection '),nl,nl,	
	Utility=2,
	write('** My Utility for this repair is '),write(Utility),nl,nl,
	setInitiatorUtility(Utility).

% ELSE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NO PROTECTION --> Utility valuation cost =2 
%%% mental note: otan mpainei edo den girnaei sto oras ekei pou ginetai to negotiation, ara prepei na ksekinisei to negotiation apo edo.
%checkProtection(Scenario,RefineType,OntType,RefineInfo,_success, Utility) :-
%	write('**I have no protection '),nl,nl,	
%	Utility=2,
%	write('** My Utility for this repair is '),write(Utility),nl,nl,
%	setInitiatorUtility(Utility).
%	findInverseRepair(RefineType,InverseRepairType),
%	nl,write('* I am informing the SPA bout the repair the ORS suggested, so he can check his Utility Functor. The repair I am sending his is '),write(InverseRepairType),nl,
 %       out(query(pa,Agent,repair,[Scenario,InverseRepairType,RefineInfo])),
%	in(reply(pa,Agent,question,SpaUtility)),
%	write('I received a response for SPA Utility, which is'), write(Utility),nl,nl,
%	write('the repair for the negotiation is '),write(RepairType),nl,nl,
%	read(Bull),
%	domain(initiator,RefineType,Agent,SpaUtility,yes),!,
%	negotiationIs(over),    
%	negototiationOntologyPath(NegoOntoPath),
	
%	out(query(pa,Agent,negotiation,NegoOntoPath)),
%	write('I am sending to the SPA the negotiation ontology path:  '),write(NegoOntoPath),nl,nl,
	%startNegotiation(NegoOntoPath),
%       write('* I am performing the repair'),nl,nl,
%        refine(Scenario,RefineType,OntType,RefineInfo),!.

	








	

% formUtility(+Level, -Utility)
% called when the repair cannot be performed due to protection
%forms the Utility valuation functor depending on the level of protection
formUtility(Level) :-
	(member( lowProtection,Level),
	write('my Utility is 1'));	
	(member(highProtection,Level),
	write('my Utility is 0')).	
	


% checkProtectionAgent(+Scenario,+RefineType,+OntType,+RefineInfo,-Result,-Utility)
% checks protection of SPA's ontology according to the type of refinement, performs repair if it is possible  


checkProtectionAgent(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[protected,no,Level],Agent,Utility) :-
  	protectPredicate(PredicateList),
  	lowerName(Rule,LowRuleChars),
        lowerName(WrongClass,LowWrongClassChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(NewRule,NewWrongClass,ArgumentType,_),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl,
	formUtility(Level);
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllLowName),nl
	
        ),!.
%% No protection --> Utility = 2  
checkProtectionAgent(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[protected,ok,noProtection],Agent,Utility) :-       
Utility=2,!.	
%refineAgent(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],Agent),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    ADDED    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRECONDITION DOMAIN REFINEMENT 
checkProtectionAgent(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[protected,no,Level],Agent,Utility) :-
  	protectPredicate(PredicateList),
  	protectArgument(ArgumentList),
        lowerName(Rule,LowRuleChars),
        lowerName(WrongClass,LowWrongClassChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowRuleChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(NewRule,NewWrongClass,ArgumentType,_),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl
        ),!.

checkProtectionAgent(Scenario,precondAA,onto,[Rule,RightClass,WrongClass],[protected,no,noProtection],Agent,Utility) :-
Utility=2,!.


% DOMAIN ABSTRACTION
checkProtectionAgent(Scenario,domainA,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,HighProtection,RightBracket,TypeHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,LowProtection,RightBracket,TypeLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        %% argument-all
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl;
        %% argumet-class
        member(TypeHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeHighName),nl;
        member(TypeLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeLowName),nl
        ),!.

checkProtectionAgent(Scenario,domainA,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[protected,no,noProtection],Agent,Utility) :-
Utility=2,!.







% CHANGE INSTANCE DEFINITION
checkProtectionAgent(Scenario,changeInstDef,onto,[Item,WrongClass,Class],[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,HighProtection,RightBracket,TypeHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentType,Comma,LowProtection,RightBracket,TypeLowName),
        (
        %% predicate-all
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllLowName),nl;
        %% argument-all
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(AllArgLowName),nl;
        %% argumet-class
        member(TypeHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeHighName),nl;
        member(TypeLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
	setInitiatorUtility(Utility),
        write('* Protection used: '),write(TypeLowName),nl
        ),!.

checkProtectionAgent(Scenario,changeInstDef,onto,[Pred,OldType,NewType,NewTypeSuperType,Position],[protected,no,noProtection],Agent,Utility) :-
Utility=2,!.












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%









checkProtectionAgent(Scenario,propositionalA,onto,[Pred,ArgType,ArgValue,ArgPosition,Arity],[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
        protectArgument(ArgumentList),
        lowerName(Pred,LowPredChars),
        name(ArgPosition,ArgPositionChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        % protect(Pred,argumentPosition,argumentAll,_)
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,HighProtection,RightBracket,ValueHighName),
        joinArgument(LeftBracket,LowPredChars,Comma,ArgPositionChars,Comma,ArgumentAll,Comma,LowProtection,RightBracket,ValueLowName),
        (
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl,
		formUtility(Level);
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllLowName),nl,
	formUtility(Level);
        member(ValueHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(ValueHighName),nl,
	formUtility(Level);
        member(ValueLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(ValueLowName),nl
	
        ),!.
 %%%%% No protection Utility=2     
checkProtectionAgent(Scenario,propositionalA,onto,[Pred,ArgType,ArgValue,Position,Arity],[protected,ok,noProtection],Agent,Utility) :-
	Utility=2,!.	
	%refineAgent(Scenario,propositionalA,onto,[Pred,Position],Agent),!.   


%% Current hack, since there is no check for propositionalAA
checkProtectionAgent(Scenario,propositionalAA,onto,_,[protected,no,highProtection],Agent,Utility):- Utility=1.
	
checkProtectionAgent(Scenario,predicateAA,onto,[OldPred,NewPred,Rule],[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
        lowerName(NewPred,LowPredChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
	(
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl,
	formUtility(Level);
        member(AllLowName,PredicateList),
	Utility=1,
        =(Level,LowProtectionName),
        write('* Protection used: '),write(AllLowName),nl
        ),!.
        
checkProtectionAgent(Scenario,predicateAA,onto,[OldPred,NewPred,Rule],[protected,ok,noProtection],Agent,Utility) :-        
       	Utility=2,!.
	%refineAgent(Scenario,predicateAA,onto,[OldPred,NewPred,Rule],Agent),!.
        
checkProtectionAgent(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[protected,no,Level],Agent, Utility) :-
	protectPredicate(PredicateList),
        lowerName(NewPred,LowPredChars),
	getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
	joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
	(
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl,
	formUtility(Level);
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllLowName),nl       
	),!.
	
checkProtectionAgent(Scenario,predicateA,onto,[OldPred,NewPred,Rule],[protected,ok,noProtection],Agent,Utility) :-        
	Utility=2,!.       
	%refineAgent(Scenario,predicateAA,onto,[NewPred,OldPred,Rule],Agent),!.	
	
checkProtectionAgent(Scenario,switchArgs,onto,PredName,[protected,no,Level],Agent,Utility) :-
	protectPredicate(PredicateList),
	protectArgument(ArgumentList),
        lowerName(PredName,LowPredChars),
        getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType),
	getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName),
        name('1',Position1),
        name('2',Position2),
        %(protect(NewPred,predicateAll,_)
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,HighProtection,RightBracket,AllHighName),
        joinPredicate(LeftBracket,LowPredChars,Comma,PredicateAll,Comma,LowProtection,RightBracket,AllLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,Position1,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHighName),
	joinArgument(LeftBracket,LowPredChars,Comma,Position1,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLowName),
        joinArgument(LeftBracket,LowPredChars,Comma,Position2,Comma,ArgumentAll,Comma,HighProtection,RightBracket,AllArgHigh2Name),	
        joinArgument(LeftBracket,LowPredChars,Comma,Position2,Comma,ArgumentAll,Comma,LowProtection,RightBracket,AllArgLow2Name),
        (
        member(AllHighName,PredicateList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllHighName),nl;
        member(AllLowName,PredicateList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllLowName),nl;
        member(AllArgHighName,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllArgHighName),nl;
        member(AllArgLowName,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllArgLowName),nl;
        member(AllArgHigh2Name,ArgumentList),
        =(Level,HighProtectionName),
	Utility=0,
        write('* Protection used: '),write(AllArgHigh2Name),nl;
        member(AllArgLow2Name,ArgumentList),
        =(Level,LowProtectionName),
	Utility=1,
        write('* Protection used: '),write(AllArgLow2Name),nl 
        ),!.
      
       
checkProtectionAgent(Scenario,switchArgs,onto,PredName,[protected,ok,noProtection],Agent,Utility) :-
	Utility=2,!.	
	%refineAgent(Scenario,switchArgs,onto,PredName,Agent),!.

	
%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%

% refine(+Type,+Info)
% sets 'ont.in' as the current stream, sets 'ont.out' as the stream to write to, performs the refinement and closes both of these streams.

%refine(classAdd,Info) :-
%	tell('PlanningAgent/ontologies/updated/ont.out'),
%	addClass(Info),
%	told.

refine(Scenario,Type,onto,Info) :-
	checkTypes(Scenario),
	assert(newTypes([])),
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	performRef(Type,Info),
	newTypes(TypesList),
	addClasses(TypesList),
	told,seen,
	copyFilesRef(Scenario).


%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%

% refineAgent(+Type,+Info,+Agent)
% sets 'ont.in' as the current stream, sets 'ont.out' as the stream to write to, performs the refinement and closes both of these streams.

refineAgent(Scenario,Type,onto,Info,Agent) :-
	checkTypesAgent(Scenario,Agent),
	assert(newTypes([])),
	find_ontology_path_agent(Scenario,Agent,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	performRef(Type,Info),
	%write('I perfomrd the refinement '), write(Type),nl,nl,
	newTypes(TypesList),
	addClasses(TypesList),
	told,seen,
	copyFilesRefAgent(Scenario,Agent).
	     
	     
% lowerName(+Name,-LowNameChars)
% converts Name into lower case

lowerName(Name,LowNameChars) :-!,
        name(Name,NameChars),
        convertName(NameChars,LowNameChars,[]).


% joinPredicate(+El1-El7,-Result)
% joins the elements of the protectPredicate predicate

joinPredicate(El1,El2,El3,El4,El5,El6,El7,ResName) :-
        append(El1,El2,Res1),
        append(El3,El4,Res2),
        append(Res1,Res2,Res3),
        append(El5,El6,Res4),
        append(Res3,Res4,Res5),
        append(Res5,El7,Res6),
        name(ResName,Res6).


% joinArgument(+El1-El9,-Result)
% joins the elements of protectArgument predicate

joinArgument(El1,El2,El3,El4,El5,El6,El7,El8,El9,ResName) :-
        append(El1,El2,Res1),
        append(El3,El4,Res2),
        append(Res1,Res2,Res3),
        append(El5,El6,Res4),
        append(El7,El8,Res5),
        append(Res4,Res5,Res6),
        append(Res3,Res6,Res7),
        append(Res7,El9,Res8),
        name(ResName,Res8).        


% getElements1(-El1-8)
% provides necessary components to check protection 

getElements1(LeftBracket,RightBracket,Comma,PredicateAll,PredicateArity,ArgumentAll,ArgumentValue,ArgumentType) :-	
	name('(',LeftBracket),
        name(')',RightBracket),
        name(',',Comma),
        name('predicateAll',PredicateAll),
        name('predicateArity',PredicateArity),
        name('argumentAll',ArgumentAll),
        name('argumentValue',ArgumentValue),
        name('argumentType',ArgumentType).

        
        
% getElements2(-El1-4)      
% provides necessary components to check protection level  
        
getElements2(HighProtection,LowProtection,HighProtectionName,LowProtectionName) :-        
        name('highProtection',HighProtection),  
        name('lowProtection',LowProtection),
        name(HighProtectionName,HighProtection),
        name(LowProtectionName,LowProtection).   
        
             
%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%


% another way of refining the ontology is simply to add a new class:
% addClass(+Class,?SuperClass)

addClass(Scenario,Class,SuperClass) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	copyOnt,
	addClasses([[Class,SuperClass]]),
	seen,told,
	copyFilesRef(Scenario).
	
addClassAgent(Scenario,Class,SuperClass,Agent) :-
	find_ontology_path_agent(Scenario,Agent,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	copyOnt,
	addClasses([[Class,SuperClass]]),
	seen,told,
	copyFilesRefAgent(Scenario).	

% sometimes, we need to add individuals:

addIndividual(Individual,Class) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	copyOnt,
	addIndiv(Individual,Class),
	seen,told,
	copyFilesRef(Scenario).

addIndividualAgent(Individual,Class,Agent) :-
	find_ontology_path_agent(Scenario,Agent,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntIn),tell(OntOut),
	copyOnt,
	addIndiv(Individual,Class),
	seen,told,
	copyFilesRefAgent(Scenario).

% copyOnt
% this copies the ontology line for line until it reaches the end.

copyOnt :-
	readLineRef(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyLine(Rest)
	;
	    copyLine(Line),
	    copyOnt
	).

% checkTypes() opens the ontology, builds up a list of the types it contains, asserts this list and then closes the ontology

checkTypes(Scenario) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	see(OntIn),
	buildTypesList(List),
	assert(typesList(List)),
	seen.

checkTypesAgent(Scenario,Agent) :-
	find_ontology_path_agent(Scenario,Agent,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	see(OntIn),
	buildTypesList(List),
	assert(typesList(List)),
	seen.

% buildTypesList(-List)
% returns a list of the types in the ontology

buildTypesList(List) :-
	typesList(List,[]).

% typesList(-List,+ListSoFar)
% reads each line, checks if it is the last line, processes the line, terminates if it was the last, otherwise recurses

typesList(List,ListSoFar) :-
	readLineTypes(Line),
	(   extractLast(Line,end,Rest),nl,
	    % if this is the last line then process the rest of the line
	    typeLine(Rest,ListSoFar,List)
	;
	    % else process the whole line and keep going
	    typeLine(Line,ListSoFar,NewList),
	    typesList(List,NewList)
	).

% typeLine(+Line,+ListSoFar,-NewList)
% if the line is a class definition, appends the class name to ListSoFar, else sets NewList to ListSoFar

typeLine(Line,ListSoFar,NewList) :-
	% first, we need to check if this is a 'define-class' line.  if not, we ignore it
	name('(Define-Class ',DefClass),
	(   matchExpression([],DefClass,RestOfLine,Line),
	    % now we need to extract the name of the class
	    name(' (',EndofClassName),
	    matchExpression(ClassNameChars,EndofClassName,_,RestOfLine),
	    name(ClassName,ClassNameChars),
	    NewList = [ClassName|ListSoFar]
	;
	    NewList = ListSoFar
	).
	    

% performRef(+Type,+Info)
% goes through the current stream line by line, checks if it is the last line (in which case, it terminates after processing that line), processes the line and recurses.

performRef(Type,Info) :-
	readLineRef(Line),
	(   extractLast(Line,end,Rest),nl,
	    % if this is the last line then process the rest of the line
	    checkLine(Type,Info,Rest)
	;
	    % else process the whole line and keep going
	    checkLine(Type,Info,Line),
	    performRef(Type,Info)
	).


% checkLine(+Type,+Info,+Line)
% attempts to process the line, which will succeed iff Line is the line that needs to be refined.  If processLine fails, this line is not the appropriate line, so it is copied to the out stream.

checkLine(Type,Info,Line) :-
	(   processLine(Type,Info,Line)
	    % if this is the line to process, then process it
	;   
	    copyLine(Line)
	    % else just copy the line into the new ontology
	).



% processLine(+Type,+Info,+Line)
% this is a different process for each refinement type.  This predicate succeeds if refinement of type Type can be performed on Line using the information contained in the list Info (which contains different info for each refinement).


% 1. Precond Refinement

processLine(precondAA,[Rule,Precond],Line):-
	% first, check if the line is a rule
	name('(Define-Axiom ', DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	% if so, find the ASCII name of the rule, append a space and see if this matches the name of this particular rule
	name(Rule,RuleChars),
	name(' ',Space),
	append(RuleChars,Space,WholeRuleChars),
	matchExpression([],WholeRuleChars,Def,AxNameAndDef),
	% if it does, we must find where the preconditions start
	name('=> (And ',PostImp),
	(   matchExpression(Comment,PostImp,PrecondsAndRest,Def)
	;
	    name('=> ',SinglePostImp),
	    matchExpression(Comment,SinglePostImp,PrecondsAndRest,Def)
	),
	append(Space,PrecondsAndRest,PrecondsAndSpace),
	% then we convert the precond into ASCII and append it
	name(Precond,PrecondChars),
	name('?Confirmation-Number',ConfNo),
	name('Pseudo-Var',PseudoVar),
	(   matchExpression(BeforeConf,ConfNo,AfterConf,PrecondChars),
	    append(BeforeConf,PseudoVar,GoodBegin),
	    append(GoodBegin,AfterConf,GoodPrecond)
	;
	    PrecondChars = GoodPrecond
	),
	append(GoodPrecond,PrecondsAndSpace,NewPrecondsAndRest),
	% then we rebuild what we have taken apart
	append(PostImp,NewPrecondsAndRest,NewDef),
	append(Comment,NewDef,NewDef2),
	append(WholeRuleChars,NewDef2,NewAxNameAndDef),
	append(DefAx,NewAxNameAndDef,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).



% 1a). Precond Domain Refinement

processLine(precondAA,[Rule,RightClass,WrongClass],Line) :-
	% first, check if the line is a rule
	name('(Define-Axiom ', DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	% if so, find the ASCII name of the rule, append a space and see if this matches the name of this particular rule
	name(Rule,RuleChars),
	name(' ',Space),
	append(RuleChars,Space,WholeRuleChars),
	matchExpression([],WholeRuleChars,_Def,AxNameAndDef),
	% now we must prepare the new precondition and add it where necessary.
	name('(',RightBracket),
	name(')',LeftBracket),
	name('?',Que),
	name(' ',Space),
	name(RightClass,RightClassChars),
	name(WrongClass,WrongClassChars),
	append(Space,RightBracket,BeginPred),
	append(BeginPred,RightClassChars,BeginClass),
	append(BeginClass,Space,Class1),
	append(Class1,Que,Class2),
	append(Class2,WrongClassChars,Class3),
	append(Class3,LeftBracket,ClassDef),
	name(') (And',PrecondsEnd),
	matchExpression(PrecondsAndBefore,PrecondsEnd,AfterPreconds,Line),
	append(PrecondsAndBefore,ClassDef,NewPreconds),
	append(NewPreconds,PrecondsEnd,GoodPreconds),
	append(GoodPreconds,AfterPreconds,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).
	
%	append(RightBracket,WrongClassChars,BeginClassID),
%	append(BeginClassID,Space,ClassID),
%	matchExpression(BeforeClass,ClassID,AfterClass,Line),
%	append(RightBracket,RightClassChars,NewBeginID),
%	append(NewBeginID,Space,NewID),
%	append(BeforeClass,NewID,NewBegin),
%	append(NewBegin,AfterClass,NewLineChars),
%	name(NewLine,NewLineChars),
%	write(NewLine).


% 1b). Precond Domain Refinement
% if there is already class information present.

processLine(precondAA,[Rule,RightClass,WrongClass],Line) :-
	% first, check if the line is a rule
	name('(Define-Axiom ', DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	% if so, find the ASCII name of the rule, append a space and see if this matches the name of this particular rule
	name(Rule,RuleChars),
	name(' ',Space),
	append(RuleChars,Space,WholeRuleChars),
	matchExpression([],WholeRuleChars,_Def,AxNameAndDef),
	% now we must see where the occurrence of wrongclass is and replace it with rightclass
	name('(',RightBracket),
	name(' ',Space),
	name(WrongClass,WrongClassChars),
	name(RightClass,RightClassChars),
	append(RightBracket,WrongClassChars,BeginClassID),
	append(BeginClassID,Space,ClassID),
	matchExpression(BeforeClass,ClassID,AfterClass,Line),
	append(RightBracket,RightClassChars,NewBeginID),
	append(NewBeginID,Space,NewID),
	append(BeforeClass,NewID,NewBegin),
	append(NewBegin,AfterClass,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% 2. Propositional Refinement

processLine(propositionalAA,[Pred,ArgType,ArgValue,Position,Arity],Line):-
	(   name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	name(Pred,PredChars),
	name(' (',SpaceAndBracket),
	append(PredChars,SpaceAndBracket,WholePredChars),
	matchExpression([],WholePredChars,Def,AxNameAndDef),
	% first of all, build up the argument type to add at the beginning.
	% if the argument type is a number, this is not needed.  else, this is a question mark, followed by the name of the arg type, followed by a space
	name(ArgValue,ArgValueChars),
	name('?',QueMark),
	name(' ',Space),
	name(Arity,ArityName),
	append(ArgValueChars,ArityName,NewArgValueChars),
	(   ArgType = 'Number',
	    NewPredChars = WholePredChars
	;
	    append(QueMark,NewArgValueChars,Var),
	    % now, we add this to the list of variables that comes after the pred name:
	    name(')',EndVars),
	    matchExpression(Vars,EndVars,AfterVars,Def),
	    buildNewVars(Vars,Var,Position,Def,NewVars),
	    append(NewVars,EndVars,NewPredChars)
	),
	% next, build up the extra argument we will want to add:
	name(ArgType,ArgTypeChars),
	name('(',LeftBracket),
	name(')',RightBracket),
	append(LeftBracket,ArgTypeChars,Arg1),
	append(Arg1,Space,Arg2),
	append(Arg2,QueMark,Arg3),
	append(Arg3,NewArgValueChars,Arg4),
	append(Arg4,RightBracket,Arg5),
	append(Arg5,Space,WholeArg),
	% find out where the definition of the pred begins:
	name(':Def (And ',DefHeader),
	matchExpression(MiddleStuff,DefHeader,Arguments,AfterVars),
	% find the right place for this argument to go:
	buildNewArgs(WholeArg,Arguments,Position,NewArguments),
	append(DefHeader,NewArguments,NewActualArgs),
	append(DefAx,PredChars,LineBegin),
	name(' (',SpRB),
	append(LineBegin,SpRB,LineBegin6),
	append(LineBegin6,NewVars,LineBegin1),
	append(LineBegin1,RightBracket,LineBegin7),
	append(LineBegin7,MiddleStuff,LineBegin2),
	append(LineBegin2,NewActualArgs,NewWholeChars),
	% convert this back to text and write to the file
	name(NewWhole,NewWholeChars),
	write(NewWhole). 


% now we have to refine all the instances of this predicate by adding a meta-variable.
processLine(propositionalAA,[Pred,_,_,Position,_],Line) :-
	% these are going to be attached to frames or axioms:
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,_,Line),
	changeAllVars(Pred,Position,Line,[],NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).


processLine(propositionalAA,[Pred,_,_,Position,_],Line) :-
	name('(Define-Frame ',DefAx),
	matchExpression([],DefAx,_,Line),
	name(Pred,PredChars),
	matchExpression(BeforePred,PredChars,AfterPred,Line),
	(   name('))',EndVars),
	    matchExpression(Vars,EndVars,RestLine,AfterPred)
	;
	    name(')',EndVars),
	    matchExpression(Vars,EndVars,RestLine,AfterPred)
	),
	VarPosition is Position - 1,
	insertNewVar([],Vars,VarPosition,NewVars),
	append(NewVars,EndVars,FullVars),
	append(FullVars,RestLine,NewAfter),
	append(BeforePred,PredChars,BeforeVars),
	append(BeforeVars,NewAfter,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).
	


processLine(propositionalAANewType,[OldPred,NewPred,_,_,_],Line) :-
	% check if the line is marked off by comment semi-colons:
	name(';;; ',CommentInitial),
	name(OldPred,OldPredChars),
	matchExpression([],CommentInitial,OldPredChars,Line),
	% now, create the new header by replacing the old name with the new one:
	name(NewPred,NewPredChars),
	append(CommentInitial,NewPredChars,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).



% A and AA the same

% ??? what is this doing here?  is this important?  is this redundant?  must find out if this is useful.  is so, move to sensible place.  if not, delete

processLine(predicateAANewType,[OldPred,NewPred,_,_,_],Line) :-
	% first, check if the line is a function or a relation
	(   name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	% if so, find the ASCII name of the pred, append a space and bracket (so it matches) and see if this matches the name of this particular pred
	name(OldPred,OldPredChars),
	matchExpression([],OldPredChars,Def,AxNameAndDef),
	name(NewPred,NewPredChars),
	append(NewPredChars,Def,NewAxNameAndDef),
	append(DefAx,NewAxNameAndDef,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName),
	% now, we need to replace the original version:
	name(LineName,Line),
	nl,nl,write(';;; '),write(OldPred),nl,nl,
	write(LineName),nl,nl.


processLine(propositionalAANewType,[_,Pred,ArgType,ArgValue,_],Line):-
	% first, check if the line is a function or a relation
	(   name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	% if so, find the ASCII name of the pred, append a space and bracket (so it matches) and see if this matches the name of this particular pred
	name(Pred,PredChars),
	name(' (',SpaceAndBracket),
	append(PredChars,SpaceAndBracket,WholePredChars),
	matchExpression([],WholePredChars,Def,AxNameAndDef),
	% if it does, we now need to look at the rest of the line, the definition of this pred, and alter it accordingly
	% first of all, build up the argument type to add at the beginning.
	% if the argument type is a number, this is not needed.  else, this is a question mark, followed by the name of the arg type, followed by a space
	name(ArgValue,ArgValueChars),
	name('?',QueMark),
	name(' ',Space),
	% now we need to add a random number, to make sure this argument is not confused with another of the same name:
	random(Number),
	name(Number,RanNoName),
	append(ArgValueChars,RanNoName,NewArgValueChars),
	(   ArgType = 'Number',
	    NewPredChars = WholePredChars
	;
	    append(QueMark,NewArgValueChars,Var),
	    append(Var,Space,VarAndSpace),
	    % now, we add this to the list of variables that comes after the pred name:
	    append(WholePredChars,VarAndSpace,NewPredChars)
	),
	% next, build up the extra argument we will want to add:
	name(ArgType,ArgTypeChars),
	name('(',LeftBracket),
	name(')',RightBracket),
	append(LeftBracket,ArgTypeChars,Arg1),
	append(Arg1,Space,Arg2),
	append(Arg2,QueMark,Arg3),
	append(Arg3,NewArgValueChars,Arg4),
	append(Arg4,RightBracket,Arg5),
	append(Arg5,Space,WholeArg),
	% find out where the definition of the pred begins:
	name(':Def (And ',DefHeader),
	matchExpression(FirstParts,DefHeader,Arguments,Def),
	% now stick this new argument on at the beginning of the other args:
	append(WholeArg,Arguments,NewArguments),
	append(DefHeader,NewArguments,NewActualArgs),
	% now rebuild what we have taken apart:
	append(FirstParts,NewActualArgs,NewDef),
	append(NewPredChars,NewDef,NewAxNameAndDef),
	append(DefAx,NewAxNameAndDef,NewWholeChars),
	% convert this back to text and write to the file
	name(NewWhole,NewWholeChars),
	write(NewWhole).



% propositional abstraction:
% this can apply to three different objects: function or relation (original defination); axioms (where it appears in the preconds); frames (where it is a fact)

% first search for the original definition:

processLine(propositionalA,[PredName,ArgPosition],Line) :-
	(   name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	name(PredName,NameChars),
	matchExpression([],NameChars,Def,AxNameAndDef),
	test,
	name(') ',EndArgList),
	matchExpression(ArgList,EndArgList,ArgDefs,Def),
	removeArg(ArgList,ArgPosition,[],NewArgList),
	name('(And ',BeginArgDefs),
	name(ArgName,ArgList),
	name(NewArgName,NewArgList),
	test,
	matchExpression(BeforeArgDefs,BeginArgDefs,FullArgDefs,ArgDefs),
	removeArgDef(FullArgDefs,ArgPosition,[],NewArgDefs),
	append(DefAx,NameChars,Begin1),
	append(Begin1,NewArgList,Begin2),
	append(Begin2,EndArgList,Begin3),
	append(Begin3,BeforeArgDefs,Begin4),
	append(Begin4,BeginArgDefs,Begin5),
	append(Begin5,NewArgDefs,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% next, search for all appearances as pre- and post-conds in axioms.

processLine(propositionalA,[PredName,ArgPosition],Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	name('(=> (And ',BeginPreconds),
	name('(And ',BeginPostconds),
	matchExpression(BeforeConds,BeginPreconds,PrecondsAndPost,AxNameAndDef),
	matchExpression(Preconds,BeginPostconds,Postconds,PrecondsAndPost),
	checkAllConds(PredName,ArgPosition,Preconds,[],NewPreconds),
	checkAllConds(PredName,ArgPosition,Postconds,[],NewPostconds),
	append(DefAx,BeforeConds,Line1),
	append(Line1,BeginPreconds,Line2),
	append(Line2,NewPreconds,Line3),
	append(Line3,BeginPostconds,Line4),
	append(Line4,NewPostconds,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% now, search for all appearances as facts, which are found within frames.

processLine(propositionalA,[PredName,ArgPosition],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,_FrameNameAndDef,Line),
	% the position of the argument to be removed depends on whether it is an axiom or not (whether the name of the individual is listed in it). thus we treat these situations differently.
	name(':Axioms',AxBegin),
	matchExpression(BeforeAxiom,AxBegin,AfterAxiom,Line),
	rewriteAllFacts(PredName,ArgPosition,BeforeAxiom,[],NewBefore),
	AxiomPos is ArgPosition + 1,
	rewriteAllFacts(PredName,AxiomPos,AfterAxiom,[],NewAfter),
	append(NewBefore,AxBegin,BeginLine),
	append(BeginLine,NewAfter,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).

test.


% 3. Domain Refinement

% we don't want to actually replace the arg in the def of the func because this specific situation may only refer to one specific case.  it is usually better just to update this in the specific axiom.  the first of these - domain - does this.  the second - funcDomain - does the former.
	
processLine(domainA,[Pred,OldType,NewType,NewTypeSuperType,Position],Line) :-
	% first, check if the line is a function or a relation
	(   name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	% if so, find the ASCII name of the pred and see if this matches the name of this particular pred
	name(Pred,PredChars),
	matchExpression([],PredChars,Def,AxNameAndDef),
	% now that we are in the right place, we must check that the new type exists already in the type hierarchy.  if not, we must add it to the class hierarchy:
	typesList(TypesList),!,
	(   member(NewType,TypesList)
	;   
	    newTypes(Types),
	    append([[NewType,NewTypeSuperType]],Types,NewTypes),
	    assert(newTypes(NewTypes)),
	    retract(newTypes(Types)),
	    NewTypesList = [NewType|TypesList],
	    retract(typesList(TypesList)),
	    assert(typesList(NewTypesList))
	),
	% if so, we can go ahead with the refinement.  First, we isolate the list of arguments:
	name(':Def (And ',DefHeader),
	matchExpression(FirstParts,DefHeader,Arguments,Def),
	% position tells us which of these args we need to replace, so we search for this, keeping track of the unchanged args that go before this one and after this one:
	locateArg(Arguments,Position,OldPrevArgs,OldPostArgs,ChangingArg),!,
	% now we have the correct argument, we need to extract the old type:
	name(NewType,NewTypeChars),
	name(' ?',CentralChars),
	name(OldType,OldTypeChars),
	name('(',RightBracket),
	append(RightBracket,OldTypeChars,FullOldType),
	(   matchExpression(FullOldType,CentralChars,OldValue,ChangingArg)
	;
	    write('WARNING: the refinement has failed'),nl,
	    fail
	),
	% now we replace it with the new type and build up again:
	append(RightBracket,NewTypeChars,FullNewType),
	append(FullNewType,CentralChars,NewChangeArgs1),
	append(NewChangeArgs1,OldValue,NewChangingArg),
	sortReverse(OldPrevArgs,NewPrevArgs),
	append(NewPrevArgs,NewChangingArg,FullEarlyArgs),
	append(FullEarlyArgs,OldPostArgs,FullArgs),
	append(DefHeader,FullArgs,NewDef),
	append(FirstParts,NewDef,NewFullDef),
	append(PredChars,NewFullDef,NewAxNameAndDef),
	append(DefAx,NewAxNameAndDef,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).


processLine(domainA,[Pred,_OldType,_NewType,_NewTypeSuperType,_Position],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,_Frame,Line),
	name(Pred,PredChars),
	matchExpression(BeforePred,PredChars,AfterPredName,Line),
	name('])',EndArgs),
	matchExpression(RestArgs,EndArgs,AfterPred,AfterPredName),
	% is the example of pred considered to be an axiom or a single fact?
	name(' :Axioms',AxBegin),
	matchExpression(_,AxBegin,_,AfterPred),
	% this succeeds if it is a single fact - this means it is a binary predicate, and the relevant arg must be in position 1 or 2.
	replaceArgs(single,RestArgs,NewArgs),
	append(BeforePred,PredChars,LineBegin),
	append(LineBegin,NewArgs,LineBegin2),
	append(LineBegin2,EndArgs,LineBegin3),
	append(LineBegin3,AfterPred,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).

processLine(domainA,[Pred,_OldType,_NewType,_NewTypeSuperType,_Position],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,_Frame,Line),
	name(Pred,PredChars),
	matchExpression(BeforePred,PredChars,AfterPredName,Line),
	name('])',EndArgs),
	matchExpression(RestArgs,EndArgs,AfterPred,AfterPredName),
	replaceArgs(axiom,PredChars,RestArgs,NewArgs),
	append(BeforePred,PredChars,LineBegin),
	append(LineBegin,NewArgs,LineBegin2),
	append(LineBegin2,EndArgs,LineBegin3),
	append(LineBegin3,AfterPred,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).

% 4. Predicate Refinement

% for predicate refinement, we want to refine two different lines: the definition of the predicate, and also the line that gives the name of the predicate.  The latter is not important as it is only altering a comment, but it is done to ensure the ontology remains readable.

% first of all, we copy the original lines, as we want these to remain in the ontology.



processLine(predicateAA,[OldPred,NewPred,Rule],Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,Axiom,Line),
	name(Rule,RuleChars),
	matchExpression([],RuleChars,RestofAxiom,Axiom),
	name(OldPred,OldPredChars),
	name(NewPred,NewPredChars),
	replaceNames(OldPredChars,NewPredChars,RestofAxiom,NewRestofAxiom),
	append(RuleChars,NewRestofAxiom,NewAxiom),
	append(DefAx,NewAxiom,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).


processLine(predicateAA,[OldPred,NewPred,Rule],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,Axiom,Line),
	name(OldPred,OldPredChars),
	matchExpression(BeforeOldPred,OldPredChars,AfterOldPred,Axiom),
	name(NewPred,NewPredChars),
	append(BeforeOldPred,NewPredChars,Line1),
	append(Line1,AfterOldPred,Line2),
	append(DefFrame,Line2,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).



processLine(predicateAANewType,[OldPredName,NewPredName,_],Line) :-
	% check if the line is marked off by comment semi-colons:
	name(';;; ',CommentInitial),
	name(OldPredName,OldPredChars),
	matchExpression([],CommentInitial,OldPredChars,Line),
	% now, create the new header by replacing the old name with the new one:
	name(NewPredName,NewPredChars),
	append(CommentInitial,NewPredChars,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).


processLine(predicateAANewType,[OldPred,NewPred,_],Line) :-
	% first, check if the line is a function or a relation
	(   name('(Define-Function ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	;
	    name('(Define-Relation ',DefAx),
	    matchExpression([],DefAx,AxNameAndDef,Line)
	),
	% if so, find the ASCII name of the pred, append a space and bracket (so it matches) and see if this matches the name of this particular pred
	name(OldPred,OldPredChars),
	matchExpression([],OldPredChars,Def,AxNameAndDef),
	name(NewPred,NewPredChars),
	append(NewPredChars,Def,NewAxNameAndDef),
	append(DefAx,NewAxNameAndDef,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName),
	% now, we need to replace the original version:
	name(LineName,Line),
	nl,nl,write(';;; '),write(OldPred),nl,nl,
	write(LineName),nl,nl.
	
processLine(predicateAA,[OldPred,NewPred,Rule],Line) :-
       name('(Define-Frame ',DefFrame),
       matchExpression([],DefFrame,Axiom,Line),
       name(OldPred,OldPredChars),
       matchExpression(BeforeOldPred,OldPredChars,AfterOldPred,Axiom),
       name(NewPred,NewPredChars),
       append(BeforeOldPred,NewPredChars,Line1),
       append(Line1,AfterOldPred,Line2),
       append(DefFrame,Line2,NewLine),
       name(NewLineName,NewLine),
       write(NewLineName).


processLine(predicateAANewType,[OldPred,NewPred,Rule],Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,Axiom,Line),
	name(Rule,RuleChars),
	matchExpression([],RuleChars,RestofAxiom,Axiom),
	name(OldPred,OldPredChars),
	name(NewPred,NewPredChars),
	replaceNames(OldPredChars,NewPredChars,RestofAxiom,NewRestofAxiom),
	append(RuleChars,NewRestofAxiom,NewAxiom),
	append(DefAx,NewAxiom,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).





%  removing problem preconds (i.e. incorrect facts):
% this precond will either be a single fact or an axiom; thus we have to search for two different ways of expressing the fact (these are both developed during the diagnostic process)

processLine(problemPrecond,[Precond,_,FirstIndividual],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,Frame,Line),
	name(FirstIndividual,IndivChars),
	matchExpression([],IndivChars,_RestOfFrame,Frame),
	name(Precond,PrecondChars),
	name(')',EndBracket),
	matchExpression(GoodPrecond,EndBracket,[],PrecondChars),
	matchExpression(BeforePrecond,GoodPrecond,AfterPrecond,Line),
	name('])',EndSit),
	matchExpression(_Sit,EndSit,GoodAfter,AfterPrecond),
	append(BeforePrecond,GoodAfter,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).

processLine(problemPrecond,[_,Precond,FirstIndividual],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,Frame,Line),
	name(FirstIndividual,IndivChars),
	matchExpression([],IndivChars,_RestOfFrame,Frame),
	name(Precond,PrecondChars),
	name(')',EndBracket),
	matchExpression(GoodPrecond,EndBracket,[],PrecondChars),
	matchExpression(BeforePrecond,GoodPrecond,AfterPrecond,Line),
	name('])',EndSit),
	matchExpression(_Sit,EndSit,GoodAfter,AfterPrecond),
	name(' ',Space),
	matchExpression(GoodBefore,Space,[],BeforePrecond),
	append(GoodBefore,GoodAfter,NewLine),
	name(NewLineName,NewLine),
	write(NewLineName).


testx.


% 5. switching arguments
% NOTE: this only works with 2-arity predicates at the moment.

% first, search for where the predicate is defined:

processLine(switchArgs,PredName,Line) :-
	name('(Define-Relation ',DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	name(PredName,PredNameChars),
	matchExpression(_,PredNameChars,AfterName,AxNameAndDef),
	testx,
	name(' (',ArgIdBegin),
	name(')',ArgIdEnd),
	matchExpression([],ArgIdBegin,ArgsAndRest,AfterName),
	matchExpression(Args,ArgIdEnd,AfterArgIds,ArgsAndRest),
	switchArgIds(Args,NewArgs),
	name('(And ',ArgDefsBegin),
	name('))',ArgDefsEnd),
	matchExpression(BeforeArgDefs,ArgDefsBegin,ArgDefsAndRest,AfterArgIds),
	matchExpression(ArgDefs,ArgDefsEnd,[],ArgDefsAndRest),
	switchArgDefs(ArgDefs,NewArgDefs),
	append(DefAx,PredNameChars,Line1),
	append(Line1,ArgIdBegin,Line2),
	append(Line2,NewArgs,Line3),
	append(Line3,ArgIdEnd,Line4),
	append(Line4,BeforeArgDefs,Line5),
	append(Line5,ArgDefsBegin,Line6),
	append(Line6,NewArgDefs,Line7),
	append(Line7,ArgDefsEnd,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% next, search for when it appears in axioms:

processLine(switchArgs,PredName,Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	switchAllExamples(PredName,AxNameAndDef,[],NewRestOfLine),
	append(DefAx,NewRestOfLine,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% finally, search for all the appearances in frames.  this is more difficult: if it appears under axioms then we can proceed as above, but if it is in the single facts, we must remove altogether from the relevant frame, take a note of what frame it was attached to, and then attach it to the frame belonging to the other argument, with the first frame as the argument.

% first, the simpler case where the problem predicate is part of the axioms

processLine(switchArgs,PredName,Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,FrameNameAndDef,Line),
	name(' ',Space),
	matchExpression(FrameName,Space,Def,FrameNameAndDef),
	name(PredName,PredNameChars),
	matchExpression(BeforePred,PredNameChars,AfterPred,Def),
	name(' ',Space),
	name(')',EndBracket),
	name(':Axioms',AxBegin),
	matchExpression(_,AxBegin,_,BeforePred),
	matchExpression([],Space,ArgsAndAfter,AfterPred),
	matchExpression(Args,EndBracket,After,ArgsAndAfter),
	switchArgIds(Args,NewArgs),
	append(DefFrame,FrameName,NewLine1),
	append(NewLine1,Space,NewLine2),
	append(NewLine2,BeforePred,NewLine3),
	append(NewLine3,PredNameChars,NewLine4),
	append(NewLine4,Space,NewLine5),
	append(NewLine5,NewArgs,NewLine6),
	append(NewLine6,EndBracket,NewLine7),
	append(NewLine7,After,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).
	

% now the more complex case where the problem predicate is a single fact

processLine(switchArgs,PredName,Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,FrameNameAndDef,Line),
	name(' ',Space),
	matchExpression(FrameName,Space,Def,FrameNameAndDef),
	name(PredName,PredNameChars),
	matchExpression(BeforePred,PredNameChars,AfterPred,Def),
	name(' (',SpaceAndRightBracket),
	name('])',LeftBracket),
	matchExpression([],Space,ArgsAndAfter,AfterPred),
	matchExpression(Arg,LeftBracket,After,ArgsAndAfter),
	name(':Axioms (',AxBegin),
	matchExpression(BeforeAxiom,AxBegin,AfterAxiom,After),
	matchExpression(GoodBeforePred,SpaceAndRightBracket,[],BeforePred),
	buildNewSwitchedArg(Arg,PredNameChars,FrameName,NewFact),
	append(DefFrame,FrameName,NewLine1),
	append(NewLine1,Space,NewLine2),
	append(NewLine2,GoodBeforePred,NewLine3),
	append(NewLine3,BeforeAxiom,NewLine4),
	append(NewLine4,AxBegin,NewLine5),
	append(NewLine5,NewFact,NewLine6),
	append(NewLine6,Space,NewLine7),
	append(NewLine7,AfterAxiom,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).




% removing postconds from action rules when this is diagnosed by the shapiro alogirthm:


processLine(removePostcond,[PredName,RuleName],Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	name(RuleName,RuleNameChars),
	matchExpression([],RuleNameChars,Def,AxNameAndDef),
	name('(And ',CondsBegin),
	matchExpression(BeforePreconds,CondsBegin,PrecondsAndAfter,Def),
	matchExpression(BeforePostconds,CondsBegin,PostcondsAndAfter,PrecondsAndAfter),
	name(PredName,PredNameChars),
	matchExpression(EarlyPostconds,PredNameChars,LatePostconds,PostcondsAndAfter),
	name(' (',PredBegin),
	matchExpression(GoodEarlyPostconds,PredBegin,[],EarlyPostconds),
	name(')',EndBracket),
	matchExpression(_Args,EndBracket,GoodLatePostconds,LatePostconds),
	append(DefAx,RuleNameChars,Line1),
	append(Line1,BeforePreconds,Line2),
	append(Line2,CondsBegin,Line3),
	append(Line3,BeforePostconds,Line4),
	append(Line4,CondsBegin,Line5),
	append(Line5,GoodEarlyPostconds,Line6),
	append(Line6,GoodLatePostconds,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).



% 6.negate preconds of an action: this occurs after failure with unsurprising questions

%processLine(negatePrecond,[RuleName,PrecondName,posToNeg],Line) :-

processLine(negatePrecond,[RuleName,PrecondName,negToPos],Line) :-
	name('(Define-Axiom ',DefAx),
	matchExpression([],DefAx,AxNameAndDef,Line),
	name(RuleName,RuleNameChars),
	matchExpression([],RuleNameChars,Def,AxNameAndDef),
	name(PrecondName,PrecondChars),
	matchExpression(BeforePrecond,PrecondChars,AfterPrecond,Def),
	name('(Not ',Neg),
	matchExpression(GoodBeforePrecond,Neg,AfterNeg,BeforePrecond),
	name(')',EndBracket),
	matchExpression(PrecondArgs,EndBracket,AfterPrecondArgs,AfterPrecond),
	append(DefAx,RuleNameChars,Line1),
	append(Line1,GoodBeforePrecond,Line2),
	append(Line2,AfterNeg,Line3),
	append(Line3,PrecondChars,Line4),
	append(Line4,PrecondArgs,Line5),
	append(Line5,AfterPrecondArgs,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).


% 7. changing an instance definition: an instance is assigned to a different class.

% first, we look at when the instance is defined as an individual:


processLine(changeInstDef,[Item,WrongClass,Class],Line) :-
%	write(Item),write(WrongClass),write(Class),
	name('(Define-Individual ',DefInd),
	matchExpression([],DefInd,IndNameAndDef,Line),
	name(Item,ItemChars),
	matchExpression([],ItemChars,Def,IndNameAndDef),
	name(WrongClass,WrongClassChars),
	name(' (',ClassBegin),
	matchExpression(ClassBegin,WrongClassChars,EndStuff,Def),
	append(DefInd,ItemChars,Line1),
	append(Line1,ClassBegin,Line2),
	name(Class,ClassChars),
	append(Line2,ClassChars,Line3),
	append(Line3,EndStuff,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).

% then we have to look at the case when the individual is defined as a frame.

processLine(changeInstDef,[Item,WrongClass,Class],Line) :-
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,FrameNameAndDef,Line),
	name(Item,ItemChars),
	matchExpression([],ItemChars,Def,FrameNameAndDef),
	name(WrongClass,WrongClassChars),
	name('Instance-Of ',ClassBegin),
	matchExpression(PreClass,ClassBegin,ClassAndAfter,Def),
	matchExpression([],WrongClassChars,End,ClassAndAfter),
	name(Class,ClassChars),
	append(DefFrame,ItemChars,Line1),
	append(Line1,PreClass,Line2),
	append(Line2,ClassBegin,Line3),
	append(Line3,ClassChars,Line4),
	append(Line4,End,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).
	

% processLine(changeInstDef,[Item,WrongClass,Class],Line) :-
% 	name('(Define-Individual ',DefInd),
% 	matchExpression([],DefInd,IndNameAndDef,Line),
% 	name(Item,ItemChars),
% 	matchExpression([],ItemChars,Def,IndNameAndDef),
% 	name(DefName,Def),
% 	write(Def).


	
	
% find_ontology_path_agent(+Scenario,+Agent,+ScenarioPath)
% path for SPA's ontology  

find_ontology_path_agent(Scenario,Agent,UpdatedPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/SPA/',SPAPath),
	atom_concat(SPAPath,Agent,AgentPath),
	atom_concat(AgentPath,'/updated',UpdatedPath).


replaceArgs(single,RestArgs,NewArgs) :-
	name(' ',Space),
	matchExpression([],Space,OtherArgs,RestArgs),
	matchExpression(_OldArg,Space,GoodOtherArgs,OtherArgs),
	name('Meta-Var',MetaVar),
	append(Space,MetaVar,BetterArgs),
	append(BetterArgs,Space,SpacedArgs),
	append(SpacedArgs,GoodOtherArgs,NewArgs).

replaceArgs(axiom,PredName,RestArgs,NewArgs) :-
	name(' ',Space),
	matchExpression([],Space,OtherArgs,RestArgs),
	matchExpression(_OldArg,Space,GoodOtherArgs,OtherArgs),
	name('Meta-Var',MetaVar),
	append(Space,MetaVar,BetterArgs),
	append(BetterArgs,Space,SemiSpacedArgs),
	append(SemiSpacedArgs,PredName,BetterSpaced),
	append(BetterSpaced,Space,FullySpacedArgs),
	append(FullySpacedArgs,GoodOtherArgs,NewArgs).





replaceNames(OldPred,NewPred,Axiom,NewAxiom) :-
	matchExpression(Before,OldPred,After,Axiom),
	append(Before,NewPred,BetterStart),
	append(BetterStart,After,BetterAxiom),
	replaceNames(OldPred,NewPred,BetterAxiom,NewAxiom).

replaceNames(_,_,NewAxiom,NewAxiom).



buildNewArgs(WholeArg,Arguments,Position,NewArguments) :-
	buildNewArgs(WholeArg,[],Arguments,Position,NewArguments).


buildNewArgs(WholeArg,BeforeArg,_,1,NewArguments) :-
	name(')))',EndArgs),
	matchExpression(GoodBefore,EndArgs,_,BeforeArg),
	name(')',SingleBracket),
	name('))',DoubleBracket),
	append(GoodBefore,SingleBracket,BestBefore),
	append(BestBefore,WholeArg,MostArgs),
	append(MostArgs,DoubleBracket,NewArguments).

buildNewArgs(WholeArg,BeforeArg,AfterArg,1,NewArguments) :-
	append(BeforeArg,WholeArg,NewBefore),
	append(NewBefore,AfterArg,NewArguments).
	

buildNewArgs(WholeArg,BeforeArg,AfterArg,Position,NewArguments) :-
	name(') ',Divider),
	matchExpression(FirstArg,Divider,RestArgs,AfterArg),
	NewPosition is Position - 1,
	append(FirstArg,Divider,FullFirstArg),
	append(BeforeArg,FullFirstArg,NewBefore),
	buildNewArgs(WholeArg,NewBefore,RestArgs,NewPosition,NewArguments).


buildNewVars(Vars,VarAndSpace,Position,_Def,NewVars) :-
%	findPosition(Position,Def,NewPosition),
	buildGoodVars(VarAndSpace,[],Vars,Position,NewVars).


buildGoodVars(Var,BeforeVar,AfterVar,1,NewVars) :-
	append(BeforeVar,Var,FullBefore),
	append(FullBefore,AfterVar,FullVars),
	addSpaces(FullVars,[],GoodVars),
	name(' ',FalseBegin),
	matchExpression([],FalseBegin,NewVars,GoodVars).

buildGoodVars(Var,BeforeVar,AfterVar,Position,NewVars) :-
	name(' ',Space),
	matchExpression(FirstVar,Space,RestVars,AfterVar),
	NewPosition is Position - 1,
	append(BeforeVar,FirstVar,NewBefore),
	buildGoodVars(Var,NewBefore,RestVars,NewPosition,NewVars).

buildGoodVars(Var,BeforeVar,AfterVar,2,NewVars) :-
	append(BeforeVar,AfterVar,MostVars),
	append(MostVars,Var,FullVars),
	addSpaces(FullVars,[],GoodVars),
	name(' ',FalseBegin),
	matchExpression([],FalseBegin,NewVars,GoodVars).


changeAllVars(Pred,Position,Line,NewLineSoFar,NewLine) :-
	name(Pred,PredChars),
	name(' ',Space),
	name('(',RightBracket),
	append(RightBracket,PredChars,SemiPredChars),
	append(SemiPredChars,Space,GoodPredChars),
	matchExpression(BeforePred,GoodPredChars,AfterPred,Line),
	name(')',EndVars),
	matchExpression(Vars,EndVars,RestLine,AfterPred),
	name('?MetaVar',Meta),
	append(Space,Vars,GoodVars),
	buildAxVars(Meta,[],GoodVars,Position,NewVars),
	append(NewVars,EndVars,GoodNewVars),
	append(BeforePred,SemiPredChars,BeforeVars),
	append(BeforeVars,GoodNewVars,NewBeginLine),
	append(NewLineSoFar,NewBeginLine,NewStartLine),
	changeAllVars(Pred,Position,RestLine,NewStartLine,NewLine).

changeAllVars(_,_,EndLine,BeginLine,NewLine) :-
	append(BeginLine,EndLine,NewLine).



buildAxVars(Var,BeforeVar,AfterVar,0,NewVars) :-
	append(BeforeVar,Var,FullBefore),
	addSpaces(FullBefore,[],GoodBefore),
	name(' ',Space),
	append(Space,AfterVar,GoodAfter),
	append(GoodBefore,GoodAfter,NewVars).

buildAxVars(Var,BeforeVar,AfterVar,Position,NewVars) :-
	name(' ',Space),
	matchExpression(FirstVar,Space,RestVars,AfterVar),
	NewPosition is Position - 1,
	append(BeforeVar,FirstVar,NewBefore),
	buildAxVars(Var,NewBefore,RestVars,NewPosition,NewVars).

buildAxVars(Var,BeforeVar,AfterVar,1,NewVars) :-
	append(BeforeVar,AfterVar,FullBefore),
	append(FullBefore,Var,FullVars),
	addSpaces(FullVars,[],NewVars).


findPosition(Position,Def,NewPosition) :-
	numberPred(Def),
	NewPosition is Position - 1.

findPosition(Position,_,Position).

numberPred(Line) :-
	name(':->',NumID),
	matchExpression(_,NumID,_,Line),!.



addSpaces(Vars,GoodSoFar,GoodVars) :-
	name('?',Que),
	matchExpression(FirstVar,Que,RestVars,Vars),
	name(' *',Marker),
	append(FirstVar,Marker,NewFirst),
	append(GoodSoFar,NewFirst,BetterVars),
	addSpaces(RestVars,BetterVars,GoodVars).

addSpaces(Vars,GoodSoFar,GoodVars) :-
	name('Pseudo',Que),
	matchExpression(FirstVar,Que,RestVars,Vars),
	name(' &',Marker),
	append(FirstVar,Marker,NewFirst),
	append(GoodSoFar,NewFirst,BetterVars),
	addSpaces(RestVars,BetterVars,GoodVars).

addSpaces(LastVar,GoodSoFar,GoodVars) :-
	append(GoodSoFar,LastVar,NearlyGood),
	removeMarkers(NearlyGood,GoodVars).


removeMarkers(Vars,GoodVars) :-
	name('&',Marker),
	name('Pseudo',Que),
	matchExpression(Before,Marker,After,Vars),
	append(Before,Que,Better),
	append(Better,After,BetterVars),
	removeMarkers(BetterVars,GoodVars).

removeMarkers(Vars,GoodVars) :-
	name('*',Marker),
	name('?',Que),
	matchExpression(Before,Marker,After,Vars),
	append(Before,Que,Better),
	append(Better,After,BetterVars),
	removeMarkers(BetterVars,GoodVars).

removeMarkers(GoodVars,GoodVars).


insertNewVar(BeforeVars,AfterVars,0,NewVars) :-
	name(' ',Space),
	(   matchExpression(ImmedBefore,Space,RestVars,AfterVars),
	    BeforeVars = GoodBefore
	;   
	    (	name(')',RightBracket),
		matchExpression(ImmedBefore,RightBracket,RestVars,AfterVars),
		append(BeforeVars,RightBracket,GoodBefore)
	    ;
		ImmedBefore = AfterVars,
		GoodBefore = BeforeVars,
		RestVars = []
	    )
	),
	append(GoodBefore,ImmedBefore,FullBefore),
	name('Meta-Var',Meta),
	append(FullBefore,Meta,All),
	returnSpaces(All,[],SpacedVars),
	append(SpacedVars,Space,FullySpacedVars),
	append(FullySpacedVars,RestVars,NewVars).


insertNewVar(BeforeVars,AfterVars,Position,NewVars) :-
	name(' ',Space),
	matchExpression(BeforeThis,Space,AfterThis,AfterVars),
	NewPosition is Position - 1,
	append(BeforeVars,BeforeThis,NewBefore),
	insertNewVar(NewBefore,AfterThis,NewPosition,NewVars).



returnSpaces([],VarsSoFar,NewVars) :-
	reverse(VarsSoFar,NewVars).

returnSpaces([Dash|[H|RestVars]],NewSoFar,NewVars) :-
	name('-',[Dash]),
	returnSpaces(RestVars,[H|[Dash|NewSoFar]],NewVars).

returnSpaces([H|RestVars],NewVarsSoFar,NewVars) :-
	H > 64,
	H < 91,
	name(' ',Space),
	append([H],Space,NewH),
	append(NewH,NewVarsSoFar,BetterVars),
	returnSpaces(RestVars,BetterVars,NewVars).

returnSpaces([H|RestVars],NewVarsSoFar,NewVars) :-
	H > 47,
	H < 58,
	findRestNumber(RestVars,[],RestNumber,AfterNumber),
	append(RestNumber,[H],WholeNumber),
	name(' ',Space),
	append(WholeNumber,Space,FullNumber),
	append(FullNumber,NewVarsSoFar,BetterVars),
	returnSpaces(AfterNumber,BetterVars,NewVars).

returnSpaces([H|RestVars],NewVarsSoFar,NewVars) :-
	returnSpaces(RestVars,[H|NewVarsSoFar],NewVars).


findRestNumber([H|RestVars],NumberSoFar,RestNumber,AfterNumber) :-
	H > 47,
	H < 58,
	findRestNumber(RestVars,[H|NumberSoFar],RestNumber,AfterNumber).

findRestNumber(RestVars,RestNumber,RestNumber,RestVars).




buildNewSwitchedArg(Arg,PredNameChars,FrameName,NewFact) :-
	name('(',RightBracket),
	name('])',LeftBracket),
	name(' ',Space),
	matchExpression(RealArg,Space,SitArg,Arg),
	append(RightBracket,PredNameChars,Fact1),
	append(Fact1,Space,Fact2),
	append(Fact2,RealArg,Fact3),
	append(Fact3,Space,Fact4),
	append(Fact4,FrameName,Fact5),
	append(Fact5,Space,Fact6),
	append(Fact6,SitArg,Fact7),
	append(Fact7,LeftBracket,NewFact).
	


switchAllExamples(PredName,Line,NewLineSoFar,NewLine) :-
	name(PredName,PredNameChars),
	matchExpression(BeforePred,PredNameChars,AfterPred,Line),
	name(' ',Space),
	name(')',EndBracket),
	matchExpression([],Space,ArgsAndRest,AfterPred),
	matchExpression(Args,EndBracket,AfterArgs,ArgsAndRest),
	switchArgIds(Args,NewArgs),
	append(BeforePred,PredNameChars,NewLine1),
	append(NewLine1,Space,NewLine2),
	append(NewLine2,NewArgs,NewLine3),
	append(NewLine3,EndBracket,NewPartOfLine),
	append(NewLineSoFar,NewPartOfLine,NewSoFar),
	switchAllExamples(PredName,AfterArgs,NewSoFar,NewLine).

switchAllExamples(_,Line,NewSoFar,NewLine) :-
	append(NewSoFar,Line,NewLine).

	

switchArgIds(Args,NewArgs) :-
	name(' ',Space),
	matchExpression(FirstArg,Space,RestArgs,Args),
	matchExpression(SecondArg,Space,SitArg,RestArgs),
	append(SecondArg,Space,NewArgsBegin),
	append(NewArgsBegin,FirstArg,NewArgsNext),
	append(NewArgsNext,Space,NewArgsNearly),
	append(NewArgsNearly,SitArg,NewArgs).

switchArgDefs(Args,NewArgs) :-
	name(') (',BetweenArgs),
	name('(',RightBracket),
	name(')',LeftBracket),
	name(' ',Space),
	matchExpression(FirstArg,BetweenArgs,RestArg,Args),
	matchExpression(SecondArg,BetweenArgs,SitArg,RestArg),
	append(RightBracket,SecondArg,GoodSecondArg),
	append(GoodSecondArg,LeftBracket,BetterSecondArg),
	append(BetterSecondArg,Space,NewArgsBegin),
	append(NewArgsBegin,FirstArg,NewArgs1),
	append(NewArgs1,LeftBracket,NewArgs2),
	append(NewArgs2,Space,NewArgs3),
	append(NewArgs3,RightBracket,NewArgs4),
	append(NewArgs4,SitArg,NewArgs).
	
	

% Sub predicates:

% locateArg(+ArgsList,+Pos,-Prev,-Rest,-ChangingArg)
% finds the argument in ArgsList which is in position Pos.  Returns this as ChangingArg, along with a list of all the arguments before ChangingArg in ArgsList (Prev) and after it (Rest).

locateArg(ArgsList,Pos,Prev,Rest,ChangingArg) :-
	locateArg(ArgsList,Pos,Prev,[],Rest,ChangingArg).

locateArg(ArgsList,1,Prev,Prev,Rest,WholeChangingArg) :-
	(   name(') ',CentralSpace),
	    matchExpression(ChangingArg,CentralSpace,Rest,ArgsList),
	    append(ChangingArg,CentralSpace,WholeChangingArg)
	;   
	    name('))',EndChars),
	    matchExpression(ChangingArg,EndChars,_,ArgsList),
	    append(ChangingArg,EndChars,WholeChangingArg),
	    Rest = []
	).

locateArg(ArgsList,Position,Prev,PrevSoFar,Rest,ChangingArg) :-
	name(' (',CentralSpace),
	matchExpression(FirstArg,CentralSpace,RestofArgsList,ArgsList),
	append(CentralSpace,RestofArgsList,FullRestofArgsList),
	name(' ',Space),
	append(Space,UsefulRestofArgsList,FullRestofArgsList),
	NewPosition is Position - 1,
	append(Space,FirstArg,FirstArgSpace),
	append(FirstArgSpace,PrevSoFar,NewPrevSoFar),
	locateArg(UsefulRestofArgsList,NewPosition,Prev,NewPrevSoFar,Rest,ChangingArg).




% sortReverse(+OldPrevArgs,-NewPrevArgs)
% reverses the order of the arguments in OldPrevArgs and returns them as NewPrevArgs

sortReverse(OldPrevArgs,NewPrevArgs) :-
	sortReverse(OldPrevArgs,NewPrevArgs,[]).

sortReverse(OldPrevArgs,NewPrevArgs,ArgsSoFar) :-
	name(' (',BeginArgs),
	matchExpression(FirstArg,BeginArgs,RestOfArgs,OldPrevArgs),
	name('(',RightBracket),
	append(RightBracket,RestOfArgs,OtherArgs),
	name(' ',Space),
	append(Space,FirstArg,FirstArgSpace),
	append(FirstArgSpace,ArgsSoFar,NewArgsSoFar),
	sortReverse(OtherArgs,NewPrevArgs,NewArgsSoFar).

sortReverse(FinalArg,NewPrevArgs,ArgsSoFar) :-
	append(FinalArg,ArgsSoFar,NewPrevArgs).
	


% copyLine(+Line)
% write Line as is to the out stream

%copyLine([]) :-
%	nl.

copyLine(Line) :-
	name(LineName,Line),
	write(LineName).



% addClasses(+ListofClassesAndSuperClasses)
% creates an entry for a new class.  since order is not important in the ontology, this is added in the next available space.  

addClasses([]).

addClasses([[FirstClass,FirstClassSuperClass]|Others]) :-
	% first we build up the line declaring the beginning of the class entry
	nl,
	name(';;; ',Comment),
	name(FirstClass,ClassChars),
	append(Comment,ClassChars,CommentLine),
	name(CommentLineName,CommentLine),
	write(CommentLineName),
	nl,nl,
	% next we build up the actual class line
	name('(Define-Class ',DefClass),
	name(' (?X) :Def (And (',DefMiddle),
	% if FirstClassSuperClass is uninstantiated or if it is instantiated to 'Thing', we add that name at the end
	(   FirstClassSuperClass = 'Thing',
	    name('Thing (?X)))',DefEnd)
	;
	    % otherwise, we add the name of the superclass
	    name(FirstClassSuperClass,SuperClass),
	    name(' (?X)))',RestOfDefEnd),
	    append(SuperClass,RestOfDefEnd,DefEnd)
	),
	append(DefMiddle,DefEnd,SomeDef),
	append(ClassChars,SomeDef,MostDef),
	append(DefClass,MostDef,FullDef),
	name(FullDefName,FullDef),
	write(FullDefName),nl,
	addClasses(Others).


addIndiv(Individual,Class) :-
	% declare the name:
	name(';;; ',Comment),
	name(Individual,IndivChars),
	append(Comment,IndivChars,CommentLine),
	name(CommentLineName,CommentLine),
	write(CommentLineName),
	nl,nl,
	% now, the definition line:
	name('(Define-Individual ',DefIndiv),
	append(DefIndiv,IndivChars,Line1),
	name(' ',Space),
	append(Line1,Space,Line2),
	name('(',OpenBracket),
	append(Line2,OpenBracket,Line3),
	name(Class,ClassChars),
	append(Line3,ClassChars,Line4),
	name(') "Not supplied yet.") ',AfterClass),
	append(Line4,AfterClass,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).
	



removeArg(OldArgAndAfter,0,ArgsSoFar,Args) :-
	name(' ',Space),
	matchExpression(_OldArg,Space,After,OldArgAndAfter),
	append(Space,After,GoodAfter),
	append(ArgsSoFar,GoodAfter,Args).

removeArg(_,0,Args,Args).
	
removeArg(Args,Pos,NewArgsSoFar,NewArgs) :-
	name(' ',Space),
	(
	    matchExpression([],Space,RestArgs,Args),
	    append(NewArgsSoFar,[],NewSoFar),
	    NewPos is Pos - 1
	;
	    matchExpression(FirstArg,Space,RestArgs,Args),
	    append(Space,FirstArg,GoodFirst),
	    append(NewArgsSoFar,GoodFirst,NewSoFar),
	    NewPos is Pos - 1
	),
	removeArg(RestArgs,NewPos,NewSoFar,NewArgs).





removeArgDef(OldArgDefAndAfter,1,ArgDefs,NewDefs) :-
	name(') (',BetweenArgs),
	matchExpression(_OldDef,BetweenArgs,AfterDef,OldArgDefAndAfter),
	append(ArgDefs,AfterDef,NewDefs).

removeArgDef(_,1,ArgDefs,NewDefs) :-
	name(' (',FalseEnd),
	name('))',GoodEnd),
	matchExpression(GoodArgs,FalseEnd,[],ArgDefs),
	append(GoodArgs,GoodEnd,NewDefs).

removeArgDef(ArgDefs,Pos,NewDefsSoFar,NewDefs) :-
	name(') (',BetweenArgs),
	matchExpression(FirstDef,BetweenArgs,RestDefs,ArgDefs),
	append(FirstDef,BetweenArgs,GoodFirst),
	append(NewDefsSoFar,GoodFirst,NewSoFar),
	NewPos is Pos - 1,
	removeArgDef(RestDefs,NewPos,NewSoFar,NewDefs).



checkAllConds(PredName,ArgPosition,Conds,CondsSoFar,NewConds) :-
	name(PredName,NameChars),
	matchExpression(BeforePred,NameChars,PredAndAfter,Conds),
	name(')',EndBracket),
	matchExpression(PredArgs,EndBracket,AfterPred,PredAndAfter),
	removeRightArg(PredArgs,ArgPosition,[],NewArgs),
	append(NameChars,NewArgs,NewPred),
	append(NewPred,EndBracket,GoodPred),
	append(BeforePred,GoodPred,NewBefore),
%	append(NewBefore,CondsSoFar,NewSoFar),
	append(CondsSoFar,NewBefore,NewSoFar),
	checkAllConds(PredName,ArgPosition,AfterPred,NewSoFar,NewConds).
	
checkAllConds(_,_,Conds,CondsSoFar,NewConds) :-
	append(CondsSoFar,Conds,NewConds).



removeRightArg(PredArgs,1,NewArgSoFar,NewArg) :-
	name('?',VariableID),
	matchExpression(BeforeVarID,VariableID,AfterVarID,PredArgs),
	% if the argument is not the last
	name(' ',Space),
	matchExpression(_UnwantedVariable,Space,AfterUnwanted,AfterVarID),
	append(BeforeVarID,AfterUnwanted,NewEnd),
	append(NewArgSoFar,NewEnd,NewArg).


removeRightArg(PredArgs,1,NewArgSoFar,NewArg) :-
	name(' ?',VariableID),
	matchExpression(BeforeVarID,VariableID,_AfterVarID,PredArgs),
	append(NewArgSoFar,BeforeVarID,NewArg).

removeRightArg(PredArgs,Position,NewArgSoFar,NewArg) :-
	name('?',VariableID),
	matchExpression(BeforeVarID,VariableID,AfterVarID,PredArgs),
	append(BeforeVarID,VariableID,WholeBefore),
	append(NewArgSoFar,WholeBefore,NewSoFar),
	NewPos is Position - 1,
	removeRightArg(AfterVarID,NewPos,NewSoFar,NewArg).



rewriteAllFacts(PredName,ArgPosition,Line,NewLineSoFar,NewLine) :-
	name(PredName,NameChars),
	matchExpression(BeforePred,NameChars,ArgsAndAfter,Line),
	name('])',EndBracket),
	matchExpression(Args,EndBracket,AfterArgs,ArgsAndAfter),
	rewriteThisFact(Args,ArgPosition,[],NewArgs),
	append(NewArgs,EndBracket,GoodNewArgs),
	append(NameChars,GoodNewArgs,NewPred),
	append(BeforePred,NewPred,NewBefore),
	append(NewLineSoFar,NewBefore,NewSoFar),
	rewriteAllFacts(PredName,ArgPosition,AfterArgs,NewSoFar,NewLine).

rewriteAllFacts(_,_,Line,NewLineSoFar,NewLine) :-
	append(NewLineSoFar,Line,NewLine).

%% new stuff added in an attempt to debug

rewriteAllFacts(_PredName,_ArgPosition,Line,_NewLineSoFar,Line).


rewriteThisFact(Args,1,ArgsSoFar,NewArgs) :-
	name(' ',Space),
	matchExpression(_UnwantedArg,Space,RestArgs,Args),
	matchExpression(GoodSoFar,Space,[],ArgsSoFar),
	append(Space,RestArgs,GoodRest),
	append(GoodSoFar,GoodRest,NewArgs).

rewriteThisFact(_,1,ArgsSoFar,NewArgs) :-
	name(' ',Space),
	matchExpression(NewArgs,Space,[],ArgsSoFar).
	

rewriteThisFact(Args,Position,ArgsSoFar,NewArgs) :-
	name(' ',Space),
	matchExpression(FirstArg,Space,RestArgs,Args),
	append(FirstArg,Space,GoodFirstArg),
	append(ArgsSoFar,GoodFirstArg,NewSoFar),
	NewPos is Position - 1,
	rewriteThisFact(RestArgs,NewPos,NewSoFar,NewArgs).

% readLine(-Text)
% reads the current stream until it reaches the end of the line

readLineRef(Text):-
   get_code(C),
   addtolineRef(C, Text).
 
addtolineRef(C, []):-
    newlineRef(C),
    nl.

addtolineRef(C,[end]) :-
	endoffileRef(C).
 
addtolineRef(C, [C|More]):-
    readLineRef(More).

newlineRef(10).
 
endoffileRef(-1).


% readLineTypes(-Text)
% this is identical to readLine, except that it does not add a newline after the newline charachter is discovered.  It is used when reading the ontology to build up the types list, and does not need to add the newline because the ontology is not being written to a new stream.

readLineTypes(Text):-
   get_code(C),
   addtolineTypes(C, Text).
 
addtolineTypes(C, []):-
    newlineTypes(C).

addtolineTypes(C,[end]) :-
	endoffileTypes(C).
 
addtolineTypes(C, [C|More]):-
    readLineTypes(More).

newlineTypes(10).
 
endoffileTypes(-1).


copyFilesRef(Scenario) :-
	find_ontology_path(Scenario,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntOut),tell(OntIn),
	copyFileRef,
	told,seen.

copyFileRef :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyThisLine(Rest)
	;
	    copyThisLine(Line),
	    copyFile
	).

copyFilesRefAgent(Scenario,Agent) :-
	find_ontology_path_agent(Scenario,Agent,ScenarioPath),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	atom_concat(ScenarioPath,'/ont.out',OntOut),
	see(OntOut),tell(OntIn),
	copyFileRefAgent,
	told,seen.

copyFileRefAgent :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyThisLine(Rest)
	;
	    copyThisLine(Line),
	    copyFileRefAgent
	).




