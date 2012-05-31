
%need to create RepairInfo for each repair type.
%need to use the value from ORS.

%storeBridgerule(store,Original,Update,Diagnose) :-
 storeBridgerule(store,Action,Repaired,[RSQ|_]) :-
	Repaired = [Original,Update|Diagnose],
	Diagnose = [Rest|_],
	Rest = [RepairType|RestInfo],
	%write(RepairType),nl, 
	get_current_scenario_path(CurrentScenarioPath),
	atom_concat(CurrentScenarioPath,'/bridgerule.pl',BridgeRule),
	%get_absolute_path('/ORS/bridge_rule/bridgerule.pl',BridgeRule),
	open(BridgeRule,append,Stream),
	%open('bridgerule.pl',append,Stream),
	buildRepairInfo(RestInfo,RepairType,Action,Original,Update,RepairInfo,IsMatch),
	atom_concat(CurrentScenarioPath, '/PA-onts/updated/', UpdatedPath),
	atom_concat(UpdatedPath,'/centralSig',CentralSigPath),
	atom_concat(UpdatedPath,'/centralThy',CentralThyPath),
	atom_concat(UpdatedPath,'/metaOnt',MetaOntPath),
	consult(CentralSigPath),
	consult(CentralThyPath),
	consult(MetaOntPath),
	write('Saving a bridge rule...'),nl,nl,
	checkUninstantiateArgs(Update,IntPred),!,
	storeRepair(Stream,Original,IntPred,RepairInfo,IsMatch),
	flush_output(Stream),
	close(Stream).

storeBridgerule(_,_,_,_):-
	!.	



checkUninstantiateArgs(UnIntPred,InPred):-
	UnIntPred =..[Pred_name|UnknownArgsList],
	findPos(UnknownArgsList,uninstantiated,RevPos,1),!,
	remove_at(_,UnknownArgsList,RevPos,RevList),
	%write(RevPos),nl,nl,
	findIndividual(RevPos,UnIntPred,Arg),
	insert_at(X,RevList,RevPos,UninsList),
	append([Pred_name],UninsList,InstList),
	%UninsList = [UninList|_],
	InPred =..InstList,
	instantiatePreconds([InPred]).
	

checkUninstantiateArgs(UnIntPred,IntPred):-
	IntPred = UnIntPred.


%propositional abstraction repaired information
buildRepairInfo(RestInfo,propositionalA,_,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Update =.. [Pred_name_Update|ArgsList_Update],
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 length(ArgsList_Update,Arity_Update),
	 length(ArgsList_Original,Arity_Original),
	 RestInfo = [ExPArgsClassList,Rest|_],
	 reverse(ExPArgsClassList,ArgsClassList),
	 %nl,write(ArgsClassList),nl,
	 Rest = [Arg_name,Position|_],
	 RepairInfo = [propositionalA,[Pred_name_Update,Arity_Original,ArgsClassList,Arg_name,Position]].

%propositional anti abstraction repaired information

buildRepairInfo(RestInfo,propositionalAA,_,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Update =.. [Pred_name_Update|ArgsList_Update],
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 length(ArgsList_Update,Arity_Update),
	 length(ArgsList_Original,Arity_Original),
	 RestInfo = [ExPArgsClassList,Rest|_],
	 reverse(ExPArgsClassList,ArgsClassList),
	 %nl,write(ArgsClassList),nl,
	 Rest = [Arg_name,Position|_],
	 %write(ExPArgsClassList),nl,
	 RepairInfo = [propositionalAA,[Pred_name_Update,Arity_Original,ArgsClassList,Arg_name,Position]].


%predicate anti abstraction repaired information
buildRepairInfo(RestInfo,predicateAA,_,Original,Update,RepairInfo,IsMatch):-
	% write('..........'),nl,
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 length(ArgsList_Original,Arity_Original),
	 %write(Pred_name_Original),nl,
	 findArgsClassMine(Original,ExPArgsClassList),
	 reverse(ExPArgsClassList,ArgsClassList),
	 RepairInfo = [predicateAA,[Pred_name_Original,Arity_Original,ArgsClassList]].

%predicate abstraction
buildRepairInfo(RestInfo,predicateA,_,Original,Update,RepairInfo,IsMatch):-
	% write('..........'),nl,
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 length(ArgsList_Original,Arity_Original),
	 %write(Pred_name_Original),nl,
	 findArgsClassMine(Original,ExPArgsClassList),
	 reverse(ExPArgsClassList,ArgsClassList),
	 RepairInfo = [predicateA,[Pred_name_Original,Arity_Original,ArgsClassList]].

%switch arguments
% [switchArgs,[Pred_name,Arity,SwitchList]]
buildRepairInfo(RestInfo,switchArgs,_,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 RestInfo = [ExPArgsClassList,_,SwitchList|_],
	 reverse(ExPArgsClassList,ArgsClassList),
	 length(ArgsList_Original,Arity_Original),
	 RepairInfo = [switchArgs,[Pred_name_Original,Arity_Original,ArgsClassList,SwitchList]].

%Domain Abstratction
buildRepairInfo(RestInfo,domainA,_,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 RestInfo = [Rest|_],
	Rest = [ExPArgsClassList,SubClass,SuperClass|_],
	% Rest = [ExPArgsClassList,SubClass,SuperClass,Position|_],
	 reverse(ExPArgsClassList,ArgsClassList),
	 length(ArgsList_Original,Arity_Original),
	RepairInfo = [domainA,[Pred_name_Original,Arity_Original,ArgsClassList,SubClass,subclass(SubClass,SuperClass)]].
	% RepairInfo = [domainA,[Pred_name_Original,Arity_Original,ArgsClassList,SubClass,subclass(SubClass,SuperClass),Position]].


%Domain Abstratction
buildRepairInfo(RestInfo,domainAA,_,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 RestInfo = [Rest|_],
	Rest = [ExPArgsClassList,SubClass,SuperClass|_],
	 %Rest = [ExPArgsClassList,SuperClass,SubClass,Position|_],
	 reverse(ExPArgsClassList,ArgsClassList),
	 length(ArgsList_Original,Arity_Original),
	RepairInfo = [domainAA,[Pred_name_Original,Arity_Original,ArgsClassList,SuperClass,subclass(SubClass,SuperClass)]].
	 %RepairInfo = [domainAA,[Pred_name_Original,Arity_Original,ArgsClassList,SuperClass,subclass(SubClass,SuperClass),Position]].

%Precondition Anti-abstraction
%It is possible to store Precondition but it may not be useful to store.
%When searching, most cases will fail.

buildRepairInfo([class|_],precondAA,Action,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Original =.. [Pred_name_Original|ArgsList_Original],
	 Action =..[Rule_name|_],
	 RepairInfo = [precondAA,[class,2,Action]].

buildRepairInfo(RestInfo,precondAA,Action,Original,Update,RepairInfo,IsMatch):-
	 IsMatch = yes,
	 Update =.. [Pred_name_Update|ArgsList_Update],
	 length(ArgsList_Update,Arity_Update),
	 RepairInfo = [precondAA,[Pred_name_Update,Arity_Update,Action]].


buildRepairInfo(_,_,_,_,_,_,IsMatch):-
	IsMatch = no,	
	write('Wrong format'),!.




storeRepair(Stream,Original,Update,RepairInfo,yes) :-
	
	nl(Stream),write(Stream,'bridgeRule(['),
	write(Stream,Original),write(Stream,','),write(Stream,Update),write(Stream,'],'),write(Stream,RepairInfo),
	write(Stream,').'),nl(Stream),
	write('successfully saved a brigde rule.'),nl,nl.

storeRepair(_,_,_,_,no):-
	!.


insert_at(X,L,K,R) :- remove_at(X,R,K,L).

remove_at(X,[X|Xs],1,Xs).
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, 
   K1 is K - 1, remove_at(X,Xs,K1,Ys).

