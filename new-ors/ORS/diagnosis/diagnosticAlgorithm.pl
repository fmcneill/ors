% This file contains the code that performs a diagnosis of the problem
% after plan execution failure occurs.  The top level function,
% diagnoseFailure/8 is called by the central agent when necessary.

:- dynamic planSoFar/1.



diagnose(Justification, Messages, Diagnosis,OntType,Scenario) :-
        findLast(Messages,in(reply(Agent,Me,Action,problem))),
        findQueries(Messages,Agent,Me,Action,Justification,ListOfQueries,ListOfSurprisingQuestions),
%       write('SQs are '),write(ListOfSurprisingQuestions),nl,nl,
        diagnoseFailure(Me,OntType,Scenario,Action,Agent,ListOfQueries,Justification,ListOfSurprisingQuestions,_Outcome,Diagnosis).




%diagnoseFailure(Me,onto,_Scenario,Action,Agent,_ListOfQueries,_Just,_SurprisingQues,Experiences,Outcome,experience) :-
 %       consumeExperiences(Agent,Action,Experiences,Repairs),
  %      saveExps(CorrectAgent,Action,Experiences),!.


diagnoseFailure(Me,OntType,Scenario,Action,Agent,ListOfQueries,Just,SurprisingQues,Experiences,Outcome,Repair) :-
        diagnoseFailure(Me,OntType,Scenario,Action,Agent,ListOfQueries,Just,SurprisingQues,Outcome,Repair).

% this is a top-level diagnosis clause which checks whether a word is unknown and, if not, uses Theodosia's semantic matching.  This process needs to be more closely integrated with the diangosis.

diagnoseFailure(Me,sumo,Scenario,_Action,Agent,_ListOfQueries,_Just,[class(Object,Class)|SurprisingQues],Outcome,semantic) :-
        % note: this is a temporary cheat!  These preds should be in the ontology.  They are not in Theodosia's current implementation ...
        get_current_scenario_path(ScPath), % defined in the scenario-based Planning Agent
        atom_concat(ScPath, '/PA-onts/updated/preds', PredsPath),
        consult(PredsPath),
        \+ Object = uninstantest,
        \+ class(Object,AnyClass),
        semanticMismatch(Me,sumo,Agent,Object,individual).

diagnoseFailure(Me,sumo,Scenario,_Action,Agent,_ListOfQueries,_Just,[class(Object,Class)|SurprisingQues],Outcome,semantic) :-
        \+ Class = uninstantest,
        \+ subclass(Class,AnyClass),
        semanticMismatch(Me,sumo,Agent,Class,class).


% if problem is not a class, need to find out if pred is known and, if so, whether the individuals are known.
%diagnoseFailure(Me,OntType,_Action,Agent,_ListOfQueries,_Just,[RSQ|SurprisingQues],Outcome,semantic) :-

diagnoseFailure(Me,sumo,Scenario,_Action,Agent,_ListOfQueries,_Just,[RSQ|SurprisingQues],Outcome,semantic) :-
        % note: this is a temporary cheat!  These preds should be in the ontology.  They are not in Theodosia's current implementation ...
        get_current_scenario_path(ScPath), % defined in the scenario-based Planning Agent
        atom_concat(ScPath, '/PA-onts/updated/preds', PredsPath),
        consult(PredsPath),
        RSQ =.. [Pred|Args],
        \+ checkPredExists(Pred),!,
        semanticMismatch(Me,sumo,Agent,Pred,pred).

diagnoseFailure(Me,sumo,Scenario,_Action,Agent,_ListOfQueries,_Just,[RSQ|SurprisingQues],Outcome,semantic) :-
        RSQ =.. [Pred|Args],
        unknownArg(Args,UnknownArg),
        semanticMismatch(Me,sumo,Agent,UnknownArg,individual).





% diagnoseFailure(+PlanningAgent,OntType,+FailedAction,+RelevantServiceProvidingAgent,+ListOfQueries,+Justification,+ListOfSurprisingQuestions,-Outcome,-RepairPerfomed)
% This predicate is called by the planning agent when plan execution occurs, and passes all the relevant information to the diagnostic system.  The appropriate repair is automatially performed within the diagnostic system; the Repair variable is merely to report this to the planning agent, which is compiling a list of repairs performed.  The Outcome variable indicates whether repair has been successful or not.

diagnoseFailure(Me,OntType,Scenario,Action,Agent,[],Just,_,Outcome,Repair) :-
        write('This plan failed immediately after a request was made to perform '),write(Action),nl,
        out(query(Me,Agent,question,performTask(Action,Agent))),
        sleep(1),
        in_noblock(reply(Agent,Me,question,Reply)),
        interpretReply_noQuestions(Me,Action,Agent,Reply,Just,OntType,Scenario,Outcome,Repair).


diagnoseFailure(_,OntType,Scenario,Action,Agent,[],_,_,_,_) :-
        write('This plan failed immediately after a request was made to perform '),write(Action),nl,
        write(Agent),write(' won\'t bloody talk to me now. '),nl.


diagnoseFailure(_Me,OntType,Scenario,Action,_Agent,[MostRecentQuery|_],_Just,[],_Outcome,negated_precondition) :-
        write('There seems to be a problem with '),write(MostRecentQuery),write('although I knew that I would be asked about it.'),nl,nl,
        checkFullyInstantiated(MostRecentQuery),
        write('This is fully instantiated: therefore, I must negate my expectations about '),write(MostRecentQuery),nl,
        rule(Action,[_,PrecondList,_]),
        Action =.. [ActionName|_ActionArgs],
        MostRecentQuery =.. [QueryName|_QueryArgs],
        findRightNegation(MostRecentQuery,PrecondList,NegationOutcome),
        translatePred(ActionName,TransAction),
        translatePred(QueryName,TransQuery),
        refine(Scenario,negatePrecond,OntType,[TransAction,TransQuery,NegationOutcome]).


diagnoseFailure(_Me,OntType,Scenario,_Action,_Agent,[MostRecentQuery|_],_Just,[],_Outcome,incorrect_instantiation) :-
        write('There seems to be a problem with '),write(MostRecentQuery),write('although I knew that I would be asked about it.'),nl,nl,
        write('I must have instantiated it incorrectly.  I\'m not sure what to do about this!'),nl.


diagnoseFailure(Me,OntType,Scenario,Action,Agent,[_|_],Just,[RelevantSurprisingQuestion|_],Outcome,Repair) :-
        write('I received a query about '),write(RelevantSurprisingQuestion),write(', which I was not expecting to be asked about.'),nl,
        matchesPrecond(RelevantSurprisingQuestion,NameRSQ,ArityRSQ,Action,Precond,ArityPrecond),
        instantiatePreconds([Precond]),!,
        write(RelevantSurprisingQuestion),write(' has the same name as the precondition '),write(Precond),nl,
        compareArity(Me,Agent,Just,RelevantSurprisingQuestion,NameRSQ,ArityRSQ,Action,Precond,ArityPrecond,OntType,Scenario,Outcome,Repair).


diagnoseFailure(Me,OntType,Scenario,Action,Agent,[_|_],Just,[RelevantSurprisingQuestion|_],Outcome,Repair) :-
        predicateRepair(Me,Agent,Action,Just,RelevantSurprisingQuestion,OntType,Scenario,Outcome,Repair).


diagnoseFailure(Me,OntType,Scenario,Action,Agent,[_|_],_,[RelevantSurprisingQuestion|_],Outcome,Repair) :-
        nl,write('DIAGNOSIS: precondition anti-abstraction '),nl,
        write(Action),write(' requires an extra precondition: '),write(RelevantSurprisingQuestion),nl,
        write('WARNING: This is a guess'),nl,nl,
        % ** add in link to new repair here (morike's).
        precondTranslate(Me,Agent,Action,RelevantSurprisingQuestion,TransInfo),
        attemptRefine(Scenario,precondAA,OntType,TransInfo,Outcome),
        Repair = [none,RelevantSurprisingQuestion,[precondAA,_]].      



semanticMismatch(_Me,onto,_Agent,Object,Type) :-
        write('the '),write(Type),write(' '),write(Object),write(' is unknown.  A semantic mismatch is diagnosed.  Unfortuntely, ORS only supports semantic matching with SUMO ontologies, not with Ontolingua ontologies.').

semanticMismatch(Me,sumo,Agent,Class,class) :-
        nl,nl,write('I don\'t know the class '),write(Class),nl,
        out(query(Me,Agent,question,uniqueIdentifier(Class,URI))),
        (   in_noblock(reply(Agent,Me,question,uniqueIdentifier(Class,URI)))
        ;
            sleep(1),
            in_noblock(reply(Agent,Me,question,uniqueIdentifier(Class,URI)))
        ),
        createMatchingCommand(Class,URI,'class-indiv',MatchCommand),
        process_create('/bin/sh', ['-c', MatchCommand]),
        find_candsPath(CandsPath),
        wordAsString(Class,ClassString),
        candidates([ClassString,[FirstCand|Rest]]),
        nl,nl,write('I am replacing '),write(FirstCand),write(' with '),write(Class),nl,nl,
        createRepairCommand(Class,FirstCand,'class-indiv',RefCommand),
        process_create('/bin/sh', ['-c', RefCommand]),
        %sleep(30),
        sleep(5).


semanticMismatch(Me,sumo,Agent,Predicate,pred) :-
        nl,nl,write('I don\'t know the predicate '),write(Predicate),nl,
        out(query(Me,Agent,question,uniqueIdentifier(Predicate,URI))),
        (   in_noblock(reply(Agent,Me,question,uniqueIdentifier(Predicate,URI)))
        ;
            sleep(1),
            in_noblock(reply(Agent,Me,question,uniqueIdentifier(Predicate,URI)))
        ),
        createMatchingCommand(Predicate,URI,relation,MatchCommand),
        process_create('/bin/sh', ['-c', MatchCommand]),
        find_candsPath(CandsPath),
        candidates([Predicate,[FirstCand|Rest]]),
        nl,nl,write('I am replacing '),write(FirstCand),write(' with '),write(Predicate),nl,nl,
        createRepairCommand(Predicate,FirstCand,'relation',RefCommand),
        process_create('/bin/sh', ['-c', RefCommand]),
        sleep(5).

find_candsPath(CandsPath) :-
        get_current_scenario_path(ScPath), % defined in the scenario-based Planning Agent
        atom_concat(ScPath, '/PA-onts/original/matches/candidateLexemes.pl', CandsPath),
        file_exists(CandsPath),
        reconsult(CandsPath),
        delete_file(CandsPath).

find_candsPath(CandsPath) :-
        sleep(1),
        find_candsPath(CandsPath).


createFileName(Class,CorrectFile) :-
        atom_codes(Class,[FirstLetter|Rest]),
        UCFirst is FirstLetter - 32,
        append([UCFirst],Rest,NewClass),
        atom_codes(CorrectClass,NewClass),
        get_current_scenario_path(ScPath),
        atom_concat(ScPath, '/PA-onts/original/matches/', MatchesAbsPath),
        atom_concat(MatchesAbsPath,CorrectClass,CorrectFile).


createMatchingCommand(Word,URI,Type,Command) :-
        atom_concat('python $ORS_HOME/ORS/sem-matching/semantic_matcher.py --url ',URI,Command1),
        atom_concat(Command1,' --name ',Command2),
        atom_concat(Command2,Word,Command3),
        atom_concat(Command3,' --kind ',Command4),
        atom_concat(Command4,Type,Command).


createRepairCommand(New,Old,relation,Command) :-
        atom_concat('python $ORS_HOME/ORS/refinement/SUMO_refinement/sumo_refine.py --old ',Old,Command1),
        atom_concat(Command1,' --new ',Command2),
        atom_concat(Command2,New,Command3),
        atom_concat(Command3,' --kind relation --ont ont.in',Command4),
        atom_concat(Command4,' --sce ', Command5),
        scenario_name(ScN), % unifies with a term which was asserted automatically after loading the scenario-based planning agent (in the run.sh file)
        atom_concat(Command5, ScN, Command).

createRepairCommand(New,UCOld,Type,Command) :-
        atom_codes(UCOld,[FirstLetter|Rest]),
        LCFirst is FirstLetter + 32,
        append([LCFirst],Rest,LCChars),
        atom_codes(LCOld,LCChars),
        atom_concat('python $ORS_HOME/ORS/refinement/SUMO_refinement/sumo_refine.py --old ',LCOld,Command1),
        atom_concat(Command1,' --new ',Command2),
        atom_concat(Command2,New,Command3),
        atom_concat(Command3,' --kind ',Command4),
        atom_concat(Command4,Type,Command5),
        atom_concat(Command5,' --ont ont.in',Command6),
        atom_concat(Command6,' --sce ',Command7),
        scenario_name(ScN),
        atom_concat(Command7, ScN, Command).

                       
wordAsString(Word,String) :-
        atom_codes(Word,[FirstChar|Rest]),
        NewFirst is FirstChar - 32,
        atom_codes(String,[NewFirst|Rest]).



       
checkPredExists(Pred) :-
        setof(X,predicate(X),FullPreds),
        checkEachPred(Pred,FullPreds).

checkEachPred(RSQ,[FirstPred|Rest]):-
        FirstPred =.. [RSQ|_Args].

checkEachPred(RSQ,[_FirstPred|Rest]) :-
        checkEachPred(RSQ,Rest).




% interpretReply_noQuestions(+PlanningAgent,+FailedAction,+ServiceProvdingAgent,-Response,+Justification,-Outcome,-Repair)
% This predicates is called in the situation where no questions have been asked prior to failure, and it interprets the service providing agent's response as to whether it can perform the action or not.  If it should be able to, the preconditions are examined.

interpretReply_noQuestions(Me,Action,Agent,yes,Just,OntType,Scenario,Outcome,Repair) :-
        write(Agent),write(' says that he can perform this task'),nl,
        problemPreconds(Me,Agent,Just,Action,ProblemPrecond),
        interpretProblemPreconds(Me,Agent,Just,Action,ProblemPrecond,OntType,Scenario,Outcome,Repair).


interpretReply_noQuestions(_,Action,Agent,no,_,OntType,Scenario,Outcome,Repair) :-
        write('DIAGNOSIS: '),write(Agent),write(' can\'t perform '),write(Action),nl,
         translatePred(Agent,TransAgent),
         attemptRefineMeta(Scenario,agent,TransAgent,Outcome),
         Repair = [none,meta,[incorrectAgent,_]].


interpretReply_noQuestions(_,_,_,Reply,_,_,_,failure,[]) :-
        write('The reply is '),write(Reply),nl.




% interpretProblemPreconds(+PlanningAgent,+ServiceProvidingAgent,+Justification,+FailedAction,+ProblemPrecond,-Outcome,-Repair)
% Diagnoses problem based on whether there is a problem precondition or not.

interpretProblemPreconds(Me,Agent,_,_,class(Item,_WrongClass),OntType,Scenario,Outcome,change_instance_definition) :-
        write('DIAGNOSIS: Change instance definition: '),
        class(Item,WrongClass),
        out(query(Me,Agent,question,class(Item,Class))),
        in_noblock(reply(Agent,Me,question,class(Item,Class))),
        WrongClass \= Class,
        write('According to '),write(Agent),
        write(' the class of '),write(Item),
        write(' should be '), write(Class), nl,
        write('I thought that '),write(Item),
        write(' was of class '),write(WrongClass),
        write('I will update my ontology to align with the '),
        write(Agent),write(' ontology '), nl,
        translatePred(Item,TransItem),
        translatePred(WrongClass,TransWrongClass),
        translatePred(Class,TransClass),
        attemptRefine(Scenario,changeInstDef,OntType,[TransItem,TransWrongClass,TransClass],Outcome).

interpretProblemPreconds(Me,Agent,_,_,subclass(Class,_WrongSuperClass),OntType,Scenario,Outcome,change_class_definition) :-
        write('DIAGNOSIS: Change class definition: '),
        class(Class,WrongSuperClass),
        out(query(Me,Agent,question,subclass(Class,SuperClass))),
        in_noblock(reply(Agent,Me,question,subclass(Class,SuperClass))),
        WrongSuperClass \= SuperClass,
        write('According to '),write(Agent),
        write(' the superclass of '),write(Class),
        write(' should be '),write(SuperClass), nl,
        write('I thought that '),write(Class),
        write(' was of superclass '),write(SuperClass),
        write(' I will update my ontology to align with the '), write(Agent),write(' ontology '), nl.

interpretProblemPreconds(Me,Agent,Just,_,ProblemPrecond,OntType,Scenario,Outcome,Repair) :-
        write('DIAGNOSIS: problem precond: '),
        write(ProblemPrecond),nl,
        shapiroCheck(Me,Agent,ProblemPrecond,Just,OntType,Scenario,Outcome),
        Repair = [ProblemPrecond,shapiro,[problemPrecondition,Just]].


% this is the pre-Morike stuff:

% interpretProblemPreconds(_,Agent,_,_,none,failure,missing_precondition) :-
%       write(Agent),write(' agrees that all my preconditions are correct '),nl,
%       write('DIAGNOSIS: Missing precondition'),nl,
%       write('FAILURE - No repair is possible'),nl.


% %interpretProblemPreconds(Me,Agent,_,_,ProblemPrecond,_,problem_precondition) :-
% %     write('DIAGNOSIS: Problem precondition: '),write(ProblemPrecond),nl,
% %     checkTransitive_noQueries(Me,Agent,ProblemPrecond,TransClosure),
% %     write('Transclosure is possible.  Transclosure fact is '),write(TransClosure),nl.


% interpretProblemPreconds(Me,Agent,Just,_,ProblemPrecond,Outcome,problem_precondition) :-
%       write('DIAGNOSIS: Problem precondition: '),write(ProblemPrecond),nl,
%       shapiroCheck(Me,Agent,ProblemPrecond,Just,Outcome).




% compareArity(+PlanningAgent,+ServiceProvidingAgent,+RelevantSurprisingQuestion,+NameRelevantSurprisingQuestion,+ArityRSQ,+FailedAction,+Precondition,+ArityPrecond,-Outcome,-Repair)
% calls either domainRepair or propositionalRepair, depending on whether the received question and the expected question have the same arity or not.

compareArity(Me,Agent,Just,RelevantSurprisingQuestion,_,ArityRSQ,Action,Precond,ArityRSQ,_OntType,Scenario,Outcome,Repair) :-
        write('They also have the same arity ('),write(ArityRSQ),write(')'),nl,
        domainRepair(Me,Action,Agent,Just,RelevantSurprisingQuestion,Precond,OntType,Scenario,Outcome,Repair).


compareArity(Me,Agent,_,RelevantSurprisingQuestion,NameRSQ,ArityRSQ,_,Precond,ArityPrecond,OntType,Scenario,Outcome,Repair) :-
        write('They have different arity ('),write(ArityRSQ),write(' and '),write(ArityPrecond),write(')'),nl,
        propositionalRepair(Scenario,Me,Agent,RelevantSurprisingQuestion,Precond,ArityRSQ,ArityPrecond,NameRSQ,OntType,Outcome,Repair).



% attemptRefine(+RepairType,+RepairInformation,-Outcome)
% once a diagnosis has been made, the diagnostic algorithm calls the repair system to implement it.  AttemptRefine monitors whether this has been successful, and returns the outcome.

%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%
% instead of going straight to the refine in refinement.pl, go first to the refineCheck

% attemptRefine(+RepairType,+RepairInformation,-Outcome)
% once a diagnosis has been made, the diagnostic algorithm calls the repair system to implement it.  AttemptRefine monitors whether this has been successful, and returns the outcome.

attemptRefine(Scenario,RefineType,OntType,RefineInfo,Outcome) :-
	Approval = yes,
	ask(RefineType,RefineInfo,Approval),
	Approval == yes ->
    consultMeta(Scenario,RefineType,OntType,RefineInfo,Outcome);
	nl,nl,write('*** I will not perform the repair.'),nl.
	
ask(precondAA,[Rule,Precond],Approval) :-
	nl,nl,write('Precondition refinement:'),nl,
	write('The rule is: '),write(Rule),nl,
	write('The precondition is: '),write(Precond),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).
	
ask(precondAA,[Rule,RightClass,WrongClass],Approval) :-
	nl,nl,write('Precondition domain refinement:'),nl,
	write('The rule is: '),write(Rule),nl,
	write('The right class is: '),write(RightClass),nl,
	write('The wrong class is: '),write(WrongClass),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(propositionalAA,[Pred,ArgType,ArgValue,Position,Arity],Approval) :-
	nl,nl,write('Propositional refinement:'),nl,
	write('The predicate is: '),write(Pred),nl,
	write('The argument type is: '),write(ArgType),nl,
	write('The position is: '),write(Position),nl,
	write('The arity is: '),write(Arity),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(propositionalAANewType,[OldPred,NewPred,_,_,_],Approval) :-
	nl,nl,write('Propositional refinement:'),nl,
	write('The old predicate is: '),write(OldPred),nl,
	write('The new predicate is: '),write(NewPred),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).
	
ask(propositionalAANewType,[_,Pred,ArgType,ArgValue,_],Approval) :-
	nl,nl,write('Propositional refinement:'),nl,
	write('The predicate is: '),write(Pred),nl,
	write('The argument type is: '),write(ArgType),nl,
	write('The argument value is: '),write(ArgValue),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(propositionalA,[PredName,ArgPosition],Approval) :-
	nl,nl,write('Propositional refinement:'),nl,
	write('The predicate name is: '),write(PredName),nl,
	write('The argument position is: '),write(ArgPosition),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(domainA,[Pred,OldType,NewType,NewTypeSuperType,Position],Approval) :-
	nl,nl,write('Domain refinement:'),nl,
	write('The predicate is: '),write(Pred),nl,
	write('The old type is: '),write(OldType),nl,
	write('The new type is: '),write(NewType),nl,
	write('The super type of the new type is: '),write(NewTypeSuperType),nl,
	write('The position is: '),write(Position),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(predicateAA,[OldPred,NewPred,Rule],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The old predicate is: '),write(OldPred),nl,
	write('The new predicate is: '),write(NewPred),nl,
	write('The rule is: '),write(Rule),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).
	
ask(predicateAANewType,[OldPredName,NewPredName,_],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The old predicate name is: '),write(OldPredName),nl,
	write('The new predicate name is: '),write(NewPredName),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(predicateAANewType,[OldPred,NewPred,_],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The old predicate is: '),write(OldPred),nl,
	write('The new predicate is: '),write(NewPred),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(predicateAANewType,[OldPred,NewPred,Rule],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The old predicate is: '),write(OldPred),nl,
	write('The new predicate is: '),write(NewPred),nl,
	write('The rule is: '),write(Rule),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).
	
ask(problemPrecond,[Precond,_,FirstIndividual],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The precondition is: '),write(Precond),nl,
	write('The first individual is: '),write(FirstIndividual),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(problemPrecond,[_,Precond,FirstIndividual],Approval) :-
	nl,nl,write('Predicate refinement:'),nl,
	write('The precondition is: '),write(Precond),nl,
	write('The first individual is: '),write(FirstIndividual),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(switchArgs,PredName,Approval) :-
	nl,nl,write('Switching arguments:'),nl,
	write('The predicate name is: '),write(PredName),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(removePostcond,[PredName,RuleName],Approval) :-
	nl,nl,write('Switching arguments:'),nl,
	write('The predicate name is: '),write(PredName),nl,
	write('The rule name is: '),write(RuleName),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(negatePrecond,[RuleName,PrecondName,negToPos],Approval) :-
	nl,nl,write('Negate preconditions of an action:'),nl,
	write('The rule name is: '),write(RuleName),nl,
	write('The precondition name is: '),write(PrecondName),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).

ask(changeInstDef,[Item,WrongClass,Class],Approval) :-
	nl,nl,write('Changing an instance definition:'),nl,
	write('The item is: '),write(Item),nl,
	write('The wrong class is: '),write(WrongClass),nl,
	write('The class should be: '),write(Class),nl,
	nl,nl,write('Do you want to perform this refinement? (yes.) '),
	read(yes).
	
	
	
% attemptRefine(Scenario,RefineType,OntType,RefineInfo,success) :-
%       refine(Scenario,RefineType,OntType,RefineInfo),
%       nl,write('the appropriate repair has been performed'),nl.

%attemptRefine(_,_,_,_,failure) :-
%       write('WARNING: repair has failed'),nl.

%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%


% attemptRefineMeta(+RepairType,+RepairInformation,-Outcome)
% as for attemptRefine, only refines the meta ontology.

attemptRefineMeta(Scenario,agent,TransAgent,success) :-
	refineMeta(Scenario,agent,TransAgent),
        nl,write('The appropriate repair has been performed'),nl.

attemptRefineMeta(_Scenario,agent,_,failure) :-
        write('WARNING: Repair has failed'),nl.
        
attemptRefineMetaProtection(Scenario,agent,Agent) :-
	nl,write('In protected refine'),nl,
	write(Scenario),nl,
	write(Agent),nl,
        refineMeta(Scenario,agent,Agent),
        nl,write('The appropriate repair has been performed'),nl.
       
                   




% propositionalRepair(+PlanningAgent,+ServiceProvidingAgent,+RelevantSurprisingQuestion,+ExpectedPrecondition,+ArityRSQ,+ArityExP,+NameRSQ,-Outcome)
% diagnoses exactly how propositional repair should be performed, finds the necessary information for refining, and attemps to perform it.

propositionalRepair(Scenario,Me,Agent,RSQ,ExPrecond,ArityRSQ,ArityExP,NameRSQ,OntType,Outcome,[ExPrecond,RSQ,[propositional_anti_abstraction,[UnmatchedClass,Position]]]) :-
	1 is ArityRSQ - ArityExP,
        findArgsClassMine(ExPrecond,ExPArgsClassList),
        findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
        write(RSQArgsClassList),
        compareClass(ExPArgsClassList,RSQArgsClassList,[],HisUnmatchedClasses,MyUnmatchedClasses),
        findHisUnmatchedClass(MyUnmatchedClasses,HisUnmatchedClasses,[UnmatchedClass|[Position]]),
        checkUnmatchedClass(Me,Agent,NameRSQ,UnmatchedClass,Position,ArityExP,RSQ,OntType,Scenario,Outcome),!.
       

propositionalRepair(Scenario,Me,Agent,RSQ,ExPrecond,ArityRSQ,ArityExP,NameRSQ,OntType,Outcome,Repair) :-
	-1 is ArityRSQ - ArityExP,
        nl,write('DIAGNOSIS: Propositional abstraction'),nl,
        findArgsClassMine(ExPrecond,ExPArgsClassList),
        write('My arguments are of these types : '),write(ExPArgsClassList),nl,
        findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
        findClassAndPosition(ExPArgsClassList,RSQArgsClassList,_UnmatchedClass,Position),
        translatePred(NameRSQ,TransName),
        attemptRefine(Scenario,propositionalA,OntType,[TransName,Position],Outcome),
        Repair = [ExPrecond,RSQ,[propositionalA,[NameRSQ,Position]]],!.


propositionalRepair(_,_,_,_,_,_,_,_,_,failure,[]) :-
	write('3'),nl,
        write('The difference in arity is greater than 1.  The system cannot currently deal with this.'),nl,!.



% checkUnmatchedClass(+PlanningAgent,+ServiceProvidingAgent,+NameRSQ,+UnmatchedClass,+Position,+ArityExP,+RSQ,-Outcome)
% in proprositional anti-abstraction, there is always an unmatched class.  checkUnmatchedClasses performs propositional anti-abstraction on the basis of what this unmatched class is and whether it is already known.  If it is not, it must be added to the class hierarchy.

checkUnmatchedClass(_,Agent,NameRSQ,uninstantiated,_,ArityExP,_,OntType,Scenario,Outcome) :-
        write(NameRSQ),write(' requires an extra argument of undetermined type.'),nl,
		out(query(Me,Agent,types,NameRSQ)),
		(	in_noblock(reply(Agent,Me,types,[NameRSQ,SA_Types]))
		;
			sleep(1),
			in_noblock(reply(Agent,Me,types,[NameRSQ,SA_Types]))
		 ),
		findMissingType(NameRSQ,SA_Types,MissingType,MissingPos),
		propAATranslate(NameRSQ,MissingType,MissingPos,ArityExP,TransInfo),
		attemptRefine(Scenario,propositionalAA,OntType,TransInfo,Outcome).


checkUnmatchedClass(_,_,NameRSQ,thing,Position,ArityExP,_,OntType,Scenario,Outcome) :-
        write(NameRSQ),write(' requires an extra argument of type '),write(thing),nl,nl,
        write('I already know that '),write(thing),write(' is the highest superclass '),
        write('position is '),write(Position),nl,
        propAATranslate(NameRSQ,thing,Position,ArityExP,TransInfo),
        attemptRefine(Scenario,propositionalAA,OntType,TransInfo,Outcome).
       
checkUnmatchedClass(_,_,NameRSQ,UnmatchedClass,Position,ArityExP,_,OntType,Scenario,Outcome) :-
        subclass(UnmatchedClass,Class),
        write(NameRSQ),write(' requires an extra argument of type '),write(UnmatchedClass),nl,nl,
        write('I already know that '),write(UnmatchedClass),write(' is a subclass of '),write(Class),
        propAATranslate(NameRSQ,UnmatchedClass,Position,ArityExP,TransInfo),
        attemptRefine(Scenario,propositionalAA,OntType,TransInfo,Outcome).
   
checkUnmatchedClass(Me,Agent,NameRSQ,UnmatchedClass,Position,ArityExP,RSQ,OntType,Scenario,Outcome) :-
        out(query(Me,Agent,question,subclass(UnmatchedClass,Class))),
        in_noblock(reply(Agent,Me,question,subclass(UnmatchedClass,Class))),
        write(Agent),write(' tells me that '),write(UnmatchedClass),write(' is of class '),write(Class),nl,
        classTranslate(UnmatchedClass,Class,TransClass,TransSuper),
        addClass(TransClass,TransSuper),
        propAATranslate(NameRSQ,UnmatchedClass,Position,ArityExP,TransInfo),
        findIndividual(Position,RSQ,InstArg),
        translatePred(InstArg,TransInst),
        addIndividual(TransInst,TransClass),
        attemptRefine(Scenario,propositionalAA,OntType,TransInfo,Outcome).



% domainRepair(+PlanningAgent,+FailedAction,+ServiceProvidingAgent,+Justification,+RSQ,+ExpectedPrecond,-Outcome,-Repair)
% if the expected preconditions have the same name and the same arity, domain repair determines how they are mismatched by looking at the classes of the arguments.

domainRepair(Me,Action,Agent,_,class(Object,RightClass),class(Object,WrongClass),OntType,Scenario,Outcome,Repair) :-
        write('My object is of the wrong class.'),nl,
        checkSubClass(Me,Agent,RightClass,WrongClass),
        write(Agent),write(' expected an object of class '),write(RightClass),write(' whereas I thought that an object of class '),write(WrongClass),write(' would be acceptable.  '),nl,
        write(WrongClass),write(' is a subclass of '),write(RightClass),write(' so I need to be more specific about this object'),
        precondDomTranslate(Me,Agent,Action,class,RightClass,WrongClass,TransInfo),
        attemptRefine(Scenario,precondAA,OntType,TransInfo,Outcome),
        Repair = [WrongClass,RightClass,[precondAA,class]].


domainRepair(Me,Action,Agent,Just,RSQ,ExPrecond,OntType,Scenario,Outcome,Repair) :-
        findArgsClassMine(ExPrecond,ExPArgsClassList),
        write('Args class list is '),write(ExPArgsClassList),nl,nl,
        findArgsClassHis(Me,Agent,RSQ,RSQArgsClassList),
        compareClass(RSQArgsClassList,ExPArgsClassList,[],_,MyDiffList),
        compareClass(ExPArgsClassList,RSQArgsClassList,[],_,HisDiffList),
        write('My unmatched classes: '),write(MyDiffList),nl,
        write('His unmatched classes: '),write(HisDiffList),nl,
        examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,RSQArgsClassList,OntType,Scenario,Outcome,Repair).


domainRepair(_,_,_,_,_,_,_,_,failure,[]) :-
        write('This is more complex: need to sort this out later'),nl.




% examineDiffs(+PlanningAgent,+ServiceProvidingAgent,+FailedActoin,+RSQ,+ExpectedPrecondition,+Justification,+MyUnmatchedClassList,+HisUnmatchedClassList,+ExpectedPrecondArgsClassList,+RSQArgsClassList,-Outcome,-Repair)
% examines how the classes differ between the class list of the expected precondition and the class list of the actual precondition.

examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,_,OntType,Scenario,Outcome,Repair) :-
        length(MyDiffList,1),
        length(HisDiffList,1),
        firstEl(MyDiffList,MyDiff),
        firstEl(HisDiffList,HisDiff),
        compareDiffs(Me,Agent,Action,RSQ,ExPrecond,ExPArgsClassList,Just,MyDiff,HisDiff,OntType,Scenario,Outcome,Repair).

examineDiffs(Me,Agent,Action,RSQ,ExPrecond,Just,MyDiffList,HisDiffList,ExPArgsClassList,RSQArgsClassList,OntType,Scenario,Outcome,Repair) :-
        MyDiffList = [],
        HisDiffList = [],
        compareClassOrder(RSQArgsClassList,ExPArgsClassList,[],SwitchList),
        checkSwitchList(Me,Agent,Action,RSQ,ExPrecond,Just,SwitchList,OntType,Scenario,Outcome,Repair).



% checkSwitchList(+PlanningAgent,+ServiceProvdingAgent,+RSQ,+ExpectedPrecondition,+Justification,+SwitchList,-Outcome,-Repair)
% if the classes of the arguments of the two predicates are the same, checkSwitchList examines whether the order is the same and attempts to refine accordingly.

checkSwitchList(Me,Agent,_,RSQ,ExPrecond,_,[],_,_,success,[]) :-
        checkTransitive(Me,Agent,RSQ,ExPrecond),
        write('DIAGNOSIS: Transitive closure.  New Pred is '),write(RSQ),nl.


checkSwitchList(Me,Agent,_,_,ExPrecond,Just,[],OntType,Scenario,Outcome,Repair) :-
        write('Class agreement between the args.  Incorrect instantiation'),nl,
        shapiroCheck(Me,Agent,ExPrecond,Just,OntType,Scenario,Outcome),
        Repair = [ExPrecond,shapiro,[incorrectInstantiation,Just]].
       

checkSwitchList(_,_,_,_,ExPrecond,_,SwitchList,OntType,Scenario,Outcome,Repair) :-
        write('Right classes for args of this pred but in the wrong order.'),nl,
        write('Switch list is '),write(SwitchList),nl,
        ExPrecond =.. [PredName|_PredArgs],
        translatePred(PredName,TransPred),
        attemptRefine(Scenario,switchArgs,OntType,TransPred,Outcome),
        Repair = [ExPrecond,RSQ,[switchArgs,PredName]].

       

% compareDiffs(+PlanningAgent,+ServiceProvidingAgent,+FailedAction,+RSQ,+ExpectedPrecondition,+ExectedPreconditionArgsClassList,+Justification,+MyDifference,+HisDifference,-Outcome,-Repair)
% If there are arguments that are mismatched, compareDiffs explores how they are mismatched and attempts repair on that basis.

compareDiffs(Me,Agent,_,RSQ,_,ExPArgsClassList,_,MyDiff,HisDiff,OntType,Scenario,Outcome,Repair) :-
        checkSubClass(Me,Agent,MyDiff,HisDiff) ->
        write('DIAGNOSIS: Domain abstraction.  '),write(MyDiff),write(' is a subtype of '),write(HisDiff),nl,
        domTranslate(RSQ,HisDiff,MyDiff,ExPArgsClassList,TransInfo),
        attemptRefine(Scenario,domainA,OntType,TransInfo,Outcome),
        Repair = [ExPrecond,RSQ,[domainA,[ExPArgsClassList,MyDiff,HisDiff]]].

compareDiffs(Me,Agent,_,RSQ,_,ExPArgsClassList,_,MyDiff,HisDiff,OntType,Scenario,Outcome,Repair) :-
        checkSubClass(Me,Agent,HisDiff,MyDiff) ->
        write('DIAGNOSIS: Domain anti-abstraction. '),write(HisDiff),write(' is a subtype of '),write(MyDiff),nl,
        domTranslate(RSQ,MyDiff,HisDiff,ExPArgsClassList,TransInfo),
        attemptRefine(Scenario,domainAA,OntType,TransInfo,Outcome),
        Repair = [ExPrecond,RSQ,[domainAA,[ExPArgsClassList,MyDiff,HisDiff]]].

compareDiffs(Me,Agent,_,_,ExPrecond,_,Just,MyDiff,HisDiff,OntType,Scenario,Outcome,Repair) :-
        write('DIAGNOSIS: Arguments of different types and are not subtypes of each other. '),nl,
        write('My argument is of type '),write(MyDiff),write(', his argument is of type '),write(HisDiff),nl,
        shapiroCheck(Me,Agent,ExPrecond,Just,OntType,Scenario,Outcome),
        Repair = [ExPrecond,shapiro,[changeOfType,Just]].

compareDiffs(_,_,_,_,_,_,_,_,_,_,_,failure,[]) :-
        write('The system cannot diagnose the appropriate repair.'),nl.
       




% predicateRepair(+PlanningAgent,+ServiceProvidingAgent,+FailedAction,+Justification,+RelevantSurprisingQuestion,-Outcome,-Repair)
% finds the relevant preconditions and determines how the predicates are related.

predicateRepair(Me,Agent,Action,Just,RelevantSurprisingQuestion,OntType,Scenario,Outcome,Repair) :-
        locateRule(Action,Just,Preconds,_),
        checkPredicateSubTypes(Me,Agent,Action,RelevantSurprisingQuestion,Preconds,OntType,Scenario,Outcome,Repair).



% problemPreconds(+PlanningAgent,+ServiceProvidingAgent,+Justification,+FailedAction,-Problem)
% checks with the other agent to find a problem precond if one exists, returning [none] if none can be found.

problemPreconds(Me,Agent,Just,Action,Problem) :-
        locateRule(Action,Just,Preconds,_),
        instantiatePreconds(Preconds),!,
        problemPreconds(Me,Agent,Preconds,Problem).

problemPreconds(_,_,[],none).

problemPreconds(Me,Agent,[FirstPrecond|RestPreconds],Problem) :-
        myFacts(MyFacts),
        FirstPrecond =.. [FirstPrecondName|_],
        member(FirstPrecondName,MyFacts),
        problemPreconds(Me,Agent,RestPreconds,Problem).

problemPreconds(Me,Agent,[FirstPrecond|RestPreconds],Problem) :-
        out(query(Me,Agent,truth,FirstPrecond)),
        sleep(1),
        in_noblock(reply(Agent,Me,truth,Reply)),
        problemPrecondReply(Me,Agent,FirstPrecond,RestPreconds,Reply,Problem).

% problemPrecondReply(+PlanningAgent+ServiceProvidingAgent,+FirstPrecond,+RestPreconds,+Agent'sReply,-Problem)
% interprets agent's reply to find the problem precond, recursing on the rest of the preconds if necessary.

problemPrecondReply(Me,Agent,class(Object,_),_,no,class(Object,NewClass)) :-
        out(query(Me,Agent,question,class(Object,NewClass))),
        in_noblock(reply(Agent,Me,question,class(Object,NewClass))).
       
problemPrecondReply(_,_,FirstPrecond,_,no,FirstPrecond).

problemPrecondReply(Me,Agent,_,RestPreconds,yes,Problem) :-
        problemPreconds(Me,Agent,RestPreconds,Problem).


%findMissingType - finds the arguments of a pred and compares them to what they should be (propAA).

findMissingType(NameRSQ,SA_Types,MissingType,MissingPos) :-
	predicate(Predicate),
	Predicate =.. [NameRSQ|PA_Types],
	findMissingType(PA_Types,SA_Types,1,MissingType,MissingPos).
	
findMissingType([Type1|RestTypes],[Type1|OtherTypes],CurrentPos,MissingType,MissingPos) :-
	NewPos is CurrentPos + 1,
	findMissingType(RestTypes,OtherTypes,NewPos,MissingType,MissingPos).
	
findMissingType([Type1|_RestTypes],[Type2|_OtherTypes],MissingPos,Type2,MissingPos).


% matchesPrecond(+SurprisingQuestion,-RSQName,-RSQArity,+FailedAction,-Precondition,-ArityPrecond)
% returns the first precondition from the precondition list who's name matches that of the surprising question.  fails if none match.  incidentally instantiates the name of the RSQ and its arity and the arity of the matching precondition, since these need to be calculated in this predicate and are useful later.

matchesPrecond(Pred,Name,ArityPred,Action,Precond,ArityPrecond) :-
        rule(Action,[_,PrecondList,_]),
        findPredInfo(Pred,Name,ArityPred),
        matchName(Name,PrecondList,Precond,ArityPrecond).

% matchName(+Name,+Preconditions,-Precond,-ArityPrecond)
% returns the name and arity of hte matching precondition; fails if none match.

matchName(Name,[],_,_) :-
        write('No preconditions have the same name as '),write(Name),nl,
        fail.

matchName(Name,[FirstPrecond|_],FirstPrecond,ArityPrecond) :-
        findPredInfo(FirstPrecond,Name,ArityPrecond).

matchName(Name,[_|Rest],Precond,ArityPrecond) :-
        matchName(Name,Rest,Precond,ArityPrecond).


% findPredInfo(+Predicate,-Name,-Arity)
% returns the name and arity of a predicate.

findPredInfo(Predicate,Name,Arity) :-
        Predicate =.. [Name|ArgsList],
        length(ArgsList,Arity).


% findArgsClassMine(+ExpectedPrecondition,-ArgsClassList)
% returns the classes of all the arguments of expected precondition.

findArgsClassMine(ExPrecond,ArgsClassList) :-
        instantiatePreconds([ExPrecond]),!,
        ExPrecond =.. [_|ArgsList],
        findArgsClassMine(ArgsList,[],ArgsClassList).

findArgsClassMine([],ArgsClassList,ArgsClassList).

findArgsClassMine([H|T],ClassesSoFar,ArgsClassList) :-
        number(H),
        findArgsClassMine(T,[number|ClassesSoFar],ArgsClassList).

findArgsClassMine([H|T],ClassesSoFar,ArgsClassList) :-
        class(H,HClass),
        findArgsClassMine(T,[HClass|ClassesSoFar],ArgsClassList).


% findArgsClassHis(+PlanningAgent,+ServiceProvidingAgent,+ExectedPrecondition,-ArgsClassList)
% returns the classes of all the arguments of expected precondition.  this is very similar to findArgsClassMine, but the serviceproviding agent can be contacted for information if necessary.

findArgsClassHis(Me,Agent,ExPrecond,ArgsClassList) :-
	ExPrecond =.. [_|ArgsList],
        findArgsClassHis(Me,Agent,ArgsList,[],ArgsClassList).

findArgsClassHis(_,_,[],ArgsClassList,ArgsClassList).
	

findArgsClassHis(Me,Agent,[uninstantiated|T],ClassesSoFar,ArgsClassList) :-
	findArgsClassHis(Me,Agent,T,[uninstantiated|ClassesSoFar],ArgsClassList).
       
findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
        number(H),
        findArgsClassHis(Me,Agent,T,[number|ClassesSoFar],ArgsClassList).

findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
        class(H,HClass),
        findArgsClassHis(Me,Agent,T,[HClass|ClassesSoFar],ArgsClassList).

findArgsClassHis(Me,Agent,[H|T],ClassesSoFar,ArgsClassList) :-
	out(query(Me,Agent,question,class(H,HClass))),
        in_noblock(reply(Agent,Me,question,class(H,HClass))),
        write(Agent),write(' tells me that '),write(H),write(' is of class '),write(HClass),nl,
        findArgsClassHis(Me,Agent,T,[HClass|ClassesSoFar],ArgsClassList).


% compareClass(+ExpectedPrecondArgsClassList,+RSQArgsClassList,+HisUnmatchedClassesSoFar,-HisUnmatchedClass,-MyUnmatchedClass)
% returns two lists: one of all the classes of the arguments of the surprising question that are not matched by the classes of arguments of the expected precondition, and one vice versa.

compareClass(ExPArgsClassList,RSQArgsClassList,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass) :-
        reverse(ExPArgsClassList,RevExPArgsClassList),
        reverse(RSQArgsClassList,RevRSQArgsClassList),
        compareClass(RevExPArgsClassList,RevRSQArgsClassList,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,1).

compareClass(MyUnmatchedClass,[],UnmatchedClass,UnmatchedClass,MyUnmatchedClass,_).

compareClass(ExPArgsClassList,[FirstClass|OtherClass],HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,Counter) :-
        NewCounter is Counter + 1,
        member(FirstClass,ExPArgsClassList),
        deleteEl(ExPArgsClassList,FirstClass,NewExPArgsClassList),
        compareClass(NewExPArgsClassList,OtherClass,HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,NewCounter).

compareClass(ExPArgsClassList,[FirstClass|OtherClass],HisUnmatchedClassSoFar,HisUnmatchedClass,MyUnmatchedClass,Counter) :-
        NewCounter is Counter + 1,
        compareClass(ExPArgsClassList,OtherClass,[[FirstClass,Counter]|HisUnmatchedClassSoFar],HisUnmatchedClass,MyUnmatchedClass,NewCounter).



% compareClassOrder(+RSQClassList,+ExpectedPreconditionClassList,+SwitchListSoFar,-SwitchList)
% if the classes all match but the order is not the same, this returns a list of the arguments that need to be switched.

compareClassOrder([],[],SwitchList,SwitchList).

compareClassOrder([FirstRSQClass|RestRSQ],[FirstRSQClass|RestExP],SwitchListSoFar,SwitchList) :-
        compareClassOrder(RestRSQ,RestExP,SwitchListSoFar,SwitchList).

compareClassOrder([FirstRSQClass|RestRSQ],[FirstExPClass|RestExP],SwitchListSoFar,SwitchList) :-
        compareClassOrder(RestRSQ,RestExP,[[FirstRSQClass,FirstExPClass]|SwitchListSoFar],SwitchList).


% checkSubClass(+PlanningAgent,+ServiceProvidingAgent,+Class1,+Class2)
% succeeds if Class1 is a subclass of Class2, fails otherwise.  this may require interaction with the serviceProvidingAgent if sufficient class information is not already available.

checkSubClass(_,_,Class1,Class2) :-
        subclass(Class1,Class2).

checkSubClass(Me,Agent,Class1,Class2) :-
        out(query(Me,Agent,truth,subclass(Class1,Class2))),
        in_noblock(reply(Agent,Me,truth,yes)),!.

checkSubClass(Me,Agent,Class1,Class2) :-
        setof(SubClass,subclass(SubClass,Class2),SubClassList),
        checkSubClasses1(Me,Agent,Class1,SubClassList).


% checkSubClasses1(+PlanningAgent,+ServiceProvidingAgent,+Class,+SubclassList)
% if class1 is not an immediate subclass of class2, checks through all the subclasses of class2 to see if there is a more distant subclass relation.

checkSubClasses1(_,_,_,[]) :-
        fail.

checkSubClasses1(_,_,Class1,[FirstSubClass|_]) :-
        subclass(Class1,FirstSubClass).

checkSubClasses1(Me,Agent,Class1,[FirstSubClass|_]) :-
        checkSubClass(Me,Agent,Class1,FirstSubClass).

checkSubClasses1(Me,Agent,Class1,[_|RestSubClasses]) :-
        checkSubClasses1(Me,Agent,Class1,RestSubClasses).



% checkPredicateSubTypes(+PlanningAent,+ServiceProvidingAgent,+FailedAction,+Preconditions,-Outcome,-Repair)
% determines whether predicate abstraction or anti-abstraction is required.


checkPredicateSubTypes(Me,Agent,Action,RSQ,[FirstPrecond|RestPreconds],OntType,Scenario,Outcome,Repair)
:-
       findPredInfo(RSQ,NameRSQ,_),
       findPredInfo(FirstPrecond,NameFP,_),
       checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],NameRSQ,NameFP,OntType,Scenario,Outcome,Repair),!.

checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],_NameRSQ,class,OntType,Scenario,Outcome,Repair)
:-
       checkPredicateSubTypes(Me,Agent,Action,RSQ,RestPreconds,OntType,Scenario,Outcome,Repair),!.


checkPredicateSubTypes(_Me,_Agent,Action,_RSQ,[_|_],NameRSQ,NameFP,OntType,Scenario,Outcome,Repair)
:-
      predSubclass(NameRSQ,NameFP),
      write('DIAGNOSIS: Predicate anti-abstraction'),nl,
 %      Repair = predicate_anti_abstraction,
      write(NameRSQ),write(' is a subtype of '),write(NameFP),nl,
      predTranslate(NameFP,NameRSQ,Action,TransInfo),
      % is this a subtype we know ourselves?  or one we have to add?
      attemptRefine(Scenario,predicateAA,OntType,TransInfo,Outcome),
      write('I knew the type'),nl,
      write('The appropriate repair has been performed'),nl,
      Repair = [none,RSQ,[predicateAA,[NameRSQ,NameFP]]],!.



checkPredicateSubTypes(_,_,Action,_RSQ,[_|_],NameRSQ,NameFP,OntType,Scenario,Outcome,predicate_abstraction)
:-
       predSubclass(NameFP,NameRSQ),
       write('DIAGNOSIS: Predicate abstraction'),nl,
       write(NameFP),write(' is a subtype of '),write(NameRSQ),nl,
       predTranslate(NameFP,NameRSQ,Action,TransInfo),
       attemptRefine(Scenario,predicateAA,OntType,TransInfo,Outcome),!.


checkPredicateSubTypes(Me,Agent,Action,_RSQ,[_|_],NameRSQ,NameFP,OntType,Scenario,Outcome,predicate_anti_abstraction)
:-
       checkPredSubClass(Me,Agent,NameRSQ,NameFP),!,
       predTranslate(NameFP,NameRSQ,Action,TransInfo),
       classTranslate(NameRSQ,NameFP,NewPredClass,NewPredSuperClass),
       addPredClass(NewPredClass,NewPredSuperClass),
       attemptRefine(Scenario,predicateAA,OntType,TransInfo,Outcome),!.


checkPredicateSubTypes(Me,Agent,Action,_RSQ,[_|_],NameRSQ,NameFP,OntType,Scenario,Outcome,predicate_abstraction)
:-
       checkPredSubClass(Me,Agent,NameFP,NameRSQ),!,
       write('DIAGNOSIS: Predicate abstraction'),nl,
       write(NameFP),write(' is a subtype of '),write(NameRSQ),nl,
       predTranslate(NameFP,NameRSQ,Action,TransInfo),
       classTranslate(NameRSQ,NameFP,NewPredClass,NewPredSuperClass),
       addPredClass(NewPredClass,NewPredSuperClass),
       attemptRefine(Scenario,predicateAA,OntType,TransInfo,Outcome),!.

checkPredicateSubTypes(Me,Agent,Action,RSQ,[_|RestPreconds],_,_,OntType,Scenario,Outcome)
:-
       checkPredicateSubTypes(Me,Agent,Action,RSQ,RestPreconds,OntType,Scenario,Outcome),!.

% checkPredSubClass(+PlanningAgent,+ServiceProvidingAgent,+Class1,+Class2)
% succeeds if there is Class1 is a subclass of Class2, where class1 and class2 are predicates; fails otherwise.

checkPredSubClass(_,_,Class1,Class2) :-
        predSubclass(Class1,Class2).

checkPredSubClass(Me,Agent,Class1,Class2) :-
        out(query(Me,Agent,truth,subclass(Class1,Class2))),
        in_noblock(reply(Agent,Me,truth,Answer)),!,
        Answer = yes.

checkPredSubClass(Me,Agent,Class1,Class2) :-    
        setof(PredSubClass,subclass(PredSubClass,Class2),PredSubClassList),
        checkPredSubClasses1(Me,Agent,Class1,PredSubClassList).


% checkPredSubClasses1(+PlanningAgent,+ServiceProvidingAgent,+Class,+SubClassList)
% succeeds if there is an indirect subclass relation between Class and any of the classes in the subclass list.

checkPredSubClasses1(_,_,_,[]) :-
        fail.
checkPredSubClasses1(_,_,Class1,[FirstPredSubClass|_]) :-
        predSubclass(Class1,FirstPredSubClass).

checkPredSubClasses1(Me,Agent,Class1,[FirstPredSubClass|_]) :-
        checkPredSubClass(Me,Agent,Class1,FirstPredSubClass).

checkPredSubClasses1(Me,Agent,Class1,[_|RestPredSubClasses]) :-
        checkPredSubClasses1(Me,Agent,Class1,RestPredSubClasses).



% shapiroCheck(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,+Justification,-Outcome)
% this is used to determine how a problem precondition came to be believed.  it locates the rule which last changed the value of the precondition and then analyses why this happened

shapiroCheck(Me,Agent,ProblemPrecond,Just,OntType,Scenario,Outcome) :-
            locateProblemRule(ProblemPrecond,Just,ProblemRule,ProbPostconds,ProbAction),
            analyseProblemRule(Me,Agent,ProblemPrecond,ProblemRule,ProbPostconds,ProbAction,OntType,Scenario,Outcome).



% locateProblemRule(+ProblemPrecond,+Justification,-ProblemRule,-ProblemPostconds,-ProblemAction)
% locates the rule which last changed the value of the problem precondition and returns the instantiated features.

locateProblemRule(ProbPrecond,[[FirstFluents,start]],originalFact,none,none) :-
        member(ProbPrecond,FirstFluents).


locateProblemRule(ProbPrecond,[[Action,RuleNo,_,Postconds,[[LastFluents|_]|_],_]|_],RuleNo,Postconds,Action) :-
        member(ProbPrecond,LastFluents).

locateProblemRule(ProbPrecond,[[_,_,_,_,_,_]|RestJust],ProblemRule,ProbPostconds,ProbAction) :-
        locateProblemRule(ProbPrecond,RestJust,ProblemRule,ProbPostconds,ProbAction).



% analyseProblemRule(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,+ProblemRule,+ProblemPostconds,+ProbAction,-Outcome)
% determines whether the precondition had always been believed to be true (original fact) or whether it is a postcondition of a previous rule

analyseProblemRule(_,_,ProblemPrecond,originalFact,_,_,OntType,Scenario,Outcome) :-
        write(ProblemPrecond),write(' is an original fact in my ontology '),nl,
        translatePrecond(ProblemPrecond,TransInfo),
        attemptRefine(Scenario,problemPrecond,OntType,TransInfo,Outcome).

analyseProblemRule(Me,Agent,ProblemPrecond,_,ProbPostconds,ProbAction,OntType,Scenario,Outcome) :-
	write('2'),
        member(ProblemPrecond,ProbPostconds),
        checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,OntType,Scenario,Outcome).

analyseProblemRule(Me,Agent,ProblemPrecond,_,ProbPostconds,ProbAction,OntType,Scenario,Outcome) :-
	write('3'),
        member(inform(ProblemPrecond),ProbPostconds),
        checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,OntType,Scenario,Outcome).


% checkProblemPredicates(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecondition,+ProbAction,-Outcome)
% finds the agent that performed the old action and determines what it thinks the value of the precondition should be

checkProblemPredicates(Me,Agent,ProblemPrecond,ProbAction,OntType,Scenario,Outcome) :-
        queryProblemPrecond(Me,Agent,ProblemPrecond,InstPrecondNew),
        findPredInfo(ProbAction,Name,_),
        agentNeeded(OldAgent,Name),
        queryProblemPrecondOldAgent(Me,OldAgent,ProblemPrecond,InstPrecondOld),
        write('I believe that '),write(ProblemPrecond),write(' is the case because i think it is a postcond of '),write(ProbAction),write(' which was performed for me by '),write(OldAgent),write('.'),nl,nl,
        findAgreement(Agent,OldAgent,ProblemPrecond,InstPrecondNew,InstPrecondOld,ProbAction,OntType,Scenario,Outcome).


% findAgreement(+PlanningAgent,+OldServiceProvidingAgent,+ProblemPrecond,+NewInstantiationofPrecond,+OldInstantiationofPrecond,+ProblemAction,-Outcome)
% refines according to what agreement there is between the old and new instantiations of the preconditions.

findAgreement(Agent,OldAgent,ProblemPrecond,InstPrecondNew,InstPrecondNew,ProbAction,OntType,Scenario,Outcome) :-
        write(Agent),write(' and '),write(OldAgent),write(' agree with each other, so I must modify my ontology '),nl,
        refineRule(ProblemPrecond,ProbAction,OntType,Scenario,Outcome).

findAgreement(Agent,OldAgent,ProbPrecond,_,ProbPrecond,_,_,_,failure) :-
        write(OldAgent),write(' agrees with me here.  There seems to be disagreement between '),write(Agent),write(' and '),write(OldAgent),write('.  this is a problem for me.'),nl.

findAgreement(_,_,_,_,_,_,_,_,_) :-
        write('Shit!  Noone agrees with anyone.  This is too crazy for me.'),nl.



% refineRule(+ProblemPrecondition,+ProblemAction,-Outcome)
% refines a rule according to the problem postcondition determined in findAgreement

refineRule(ProblemPrecond,ProbAction,OntType,Scenario,Outcome) :-
        translatePrecond(ProblemPrecond,TransInfo),
        ProbAction =.. [ProbActionName|_Args],
        translatePred(ProbActionName,TransProbAction),
        ProblemPrecond =.. [ProbPrecondName|_OtherArgs],
        translatePred(ProbPrecondName,TransPrecond),
        attemptRefine(Scenario,problemPrecond,OntType,TransInfo,_),
        attemptRefine(Scenario,removePostcond,OntType,[TransPrecond,TransProbAction],Outcome).
       
       
% queryProblemPrecond(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,-NewInstantiation)
% returns the instantiation that the service providing agent believes the problem precondition ought to have and interprets the results
       
queryProblemPrecond(Me,Agent,class(Object,Class),class(Object,NewClass)) :-
        out(query(Me,Agent,question,class(Object,NewClass))),
        in_noblock(reply(Agent,Me,question,class(Object,class(Object,NewClass)))),
        Class \= NewClass.

queryProblemPrecond(_,_,class(_,_),failure) :-  
        write('Postcondition seems to be correct.  it is not clear why it is causing failure (shouldnt really be here)'),nl.

queryProblemPrecond(Me,Agent,Precond,Answer) :-    
        findRelevantInstantiation(Me,Agent,Precond,Answer).

queryProblemPrecond(_,_,_,_) :-
        write('I don\'t know why this has failed'),nl.


% findRelevantInstantiation(+PlanningAgent,+ServiceProvidingAgent,+Precondition,-InstantiatedPrecond)
% returns the instantiation that the service providing agent believes the problem precondition ought to have

findRelevantInstantiation(Me,Agent,Precond,InstPrecond) :-
        findPredInfo(Precond,Name,Arity),
        createList(Arity,List),
        UnInstPrecond =.. [Name|List],
        out(query(Me,Agent,question,UnInstPrecond)),
        in_noblock(reply(Agent,Me,question,InstPrecond)).


% createList(+Arity,-List)
% returns a list of arity Arity with all arguments uninstantiated

createList(Arity,List) :-
        createList(Arity,[],List).

createList(0,List,List).

createList(Arity,ListSoFar,List) :-
        NewArity is Arity - 1,
        createList(NewArity,[_|ListSoFar],List).



% queryProblemPrecondOldAgent(+PlanningAgent,+ServiceProvidingAgent,+ProblemPrecond,-NewInstantiation)
% queries the original action performing agent to return the correct instantiation of the precondtion according to that agent

queryProblemPrecondOldAgent(Me,OldAgent,class(Object,_),class(Object,NewClass)) :-
        out(query(Me,OldAgent,question,class(Object,NewClass))),
        in_noblock(reply(OldAgent,Me,question,class(Object,NewClass))).

queryProblemPrecondOldAgent(Me,OldAgent,ProblemPrecond,InstPrecond) :-      
        findRelevantInstantiation(Me,OldAgent,ProblemPrecond,InstPrecond).




% translation predicates for preparing the relevant information for insertion into the KIF ontology:

% classTranslate(+Class,+SuperClass,-TransClass,-TransSuperClass)
% returns the class and superclass translated into KIF notation

classTranslate(Class,SuperClass,TransClass,TransSuperClass) :-
        translatePred(Class,TransClass),
        translatePred(SuperClass,TransSuperClass).


% propAATranslate(+PredName,+ClassofNewArg,+Position,+ArityPred,[-TransPredName,-TransNewType,-Position,-ArityPred)
% returns the translated information necessary for propositional translation
% the arity of the expected precond is increased to account for the fact that it has a situational argument in KIF but not in Prolog

propAATranslate(PredName,Type,Position,ArityExP,[TransPredName,NewType,NewType,Position,NewArityExP]) :-
        translatePred(PredName,TransPredName),
        translatePred(Type,NewType),
        NewArityExP is ArityExP + 1.


% propTranslateNewType(+OldType,+PredName,+Class,+Position,+ArityPred,[-TransOldType,-TransPredName,-TransType,-Position,-ArityExP])
% returns the translated information necessary for propositional translation when the class of the additional argument is not currently in the ontology

propTranslateNewType(OldType,PredName,Type,Position,ArityExP,[TransOldType,TransPredName,NewType,NewType,Position,NewArityExP]) :-
        % we need to make an appropriate sting of the predicate name.
        translatePred(OldType,TransOldType),
        translatePred(PredName,TransPredName),
        translateType(Type,NewType),
        NewArityExP is ArityExP + 1.


% translatePred(+PredName,-TransPredName)
% translates a name (for example, of a predicate) from Prolog to KIF notation

translatePred(PredName,TransPredName) :-
        name(PredName,[FirstPredNameChar|RestPredNameChars]),
        NewFirstChar is FirstPredNameChar - 32,
        translatePredRec(RestPredNameChars,TransPredChars),
        append([NewFirstChar],TransPredChars,FullTransPredChars),
        name(TransPredName,FullTransPredChars).


% translatePredRec(+Chars,-TransChars)
% recurses through all the characters of a name and translates them as appropriate

translatePredRec(Chars,TransChars) :-
        translatePredRec(Chars,[],TransChars).

translatePredRec([],TransSoFar,TransChars) :-
        reverse(TransSoFar,TransChars).

translatePredRec([FirstChar|RestChars],TransSoFar,TransChars) :-
        FirstChar < 91,
        FirstChar > 64,
        name('-',Dash),
        append([FirstChar],Dash,NewChar),
        append(NewChar,TransSoFar,NewTransSoFar),
        translatePredRec(RestChars,NewTransSoFar,TransChars).

translatePredRec([FirstChar|RestChars],TransSoFar,TransChars) :-            
        translatePredRec(RestChars,[FirstChar|TransSoFar],TransChars).


% translateType(+Class,-NewClass)
% translates a class

translateType(Type,NewType) :-
        name(Type,[FirstTypeChar|RestTypeChars]),
        NewTypeChar is FirstTypeChar - 32,
        NewTypeChars = [NewTypeChar|RestTypeChars],
        name(NewType,NewTypeChars).

% translatePrecond(+Fact,[-TransFact,-TransSingleFact,-TransFirstIndiv])
% translates a precondition and returns its first argument.

translatePrecond(Fact,[TransFact,TransSingleFact,TransFirstIndiv]) :-
        Fact =.. [FactName|[FirstIndiv]],
        translatePred(FactName,TransName),
        translatePred(FirstIndiv,TransFirstIndiv),
        name('(',RightBracket),
        name(')',LeftBracket),
        name(' ',Space),
        name(TransName,NameChars),
        name(TransFirstIndiv,FirstIndivChars),
        append(RightBracket,NameChars,Fact1),
        append(Fact1,Space,Fact2),
        append(Fact2,FirstIndivChars,Fact3),
        append(Fact3,LeftBracket,TransFactChars),
        name(TransFact,TransFactChars),
        append(Fact2,LeftBracket,TransSingleChars),
        name(TransSingleFact,TransSingleChars).

translatePrecond(Fact,[TransFact,TransSingleFact,TransFirstIndiv]) :-
        Fact =.. [FactName|[FirstIndiv|RestArgs]],
        translatePred(FactName,TransName),
        translatePred(FirstIndiv,TransFirstIndiv),
        translatePrecondArgs(RestArgs,[],TransArgs),
        name('(',RightBracket),
        name(')',LeftBracket),
        name(' ',Space),
        name(TransName,NameChars),
        name(TransFirstIndiv,FirstIndivChars),
        append(RightBracket,NameChars,Fact1),
        append(Fact1,Space,Fact2),
        append(Fact2,FirstIndivChars,Fact3),
        append(Fact3,Space,Fact4),
        append(Fact4,TransArgs,Fact5),
        append(Fact5,LeftBracket,TransFactChars),
        name(TransFact,TransFactChars),
        append(Fact2,TransArgs,Fact6),
        append(Fact6,LeftBracket,TransSingleChars),
        name(TransSingleFact,TransSingleChars).


% translatePrecondArgs(+LastArg,+TransSoFar,-TransArgs)
% recursive section of translatePrecond, translating each argument

translatePrecondArgs([LastArg],TransSoFar,TransArgs) :-
        translatePred(LastArg,TransLast),
        name(TransLast,LastName),
        reverse(LastName,RevLast),
        append(RevLast,TransSoFar,NewTrans),
        reverse(NewTrans,TransArgs).

translatePrecondArgs([FirstArg|Rest],TransSoFar,TransArgs) :-
        translatePred(FirstArg,FirstTrans),
        name(FirstTrans,FirstName),
        name(' ',Space),
        append(FirstName,Space,NewFirstArg),
        reverse(NewFirstArg,RevFirst),
        append(RevFirst,TransSoFar,NewTrans),
        translatePrecondArgs(Rest,NewTrans,TransArgs).
       

% domTranslate(+SurprisingQuestion,+PlanningAgentDifferentArgument,+ServiceProvidingAgentDiffArg,+ArgumentClassList,[-TransPredName,-TransOldType,-TransNewType,-TransOldType,-Position])
% translates the information necessary for domain repair

domTranslate(RSQ,MyDiff,HisDiff,ArgsClassList,[TransPredName,TransOldType,TransNewType,TransOldType,Position]) :-
        RSQ =.. [PredName|_],
        translatePred(PredName,TransPredName),
        findPos(ArgsClassList,MyDiff,RevPos,1),
        length(ArgsClassList,Length),
        FullLength is Length + 1,
        Position is FullLength - RevPos,
        translatePred(MyDiff,TransOldType),
        translatePred(HisDiff,TransNewType).


% findPos(+ArgsClassList,+Argument,-Position,+PositionSoFar)
% returns the position in a list of class arguments of a given argument

findPos([Arg|_],Arg,Position,Position).

findPos([_|Rest],Arg,Position,PosSoFar) :-
        NextPos is PosSoFar + 1,
        findPos(Rest,Arg,Position,NextPos).


% predTranslate(+OldName,+NewName,+Action,[-TransOldName,-TransNewName,-TransAction])
% translates the information necessary for predicate translation

predTranslate(OldName,NewName,Action,[TransOld,TransNew,TransAction]) :-
        translatePred(OldName,TransOld),
        translatePred(NewName,TransNew),
        Action =.. [ActionName|_],
        translatePred(ActionName,TransAction).


% precondDomTranslate(+PlanningAgent,+ServiceProvidingAgent,+Precondition,+RightClass,+WrongClass,[-TransRule,-TransRightClass,-TransWrongClass])
% translates the information necessary for domain translation

precondDomTranslate(_Me,_Agent,Action,class,RightClass,WrongClass,[TransRule,TransRightClass,TransWrongClass]) :-
        Action =.. [ActionName|_],
        translatePred(ActionName,TransRule),
        translatePred(RightClass,TransRightClass),
        translatePred(WrongClass,TransWrongClass).


% precondTranslate(+PlanningAgent,+ServiceProvidingAgent,+Action,+Precondition,[-TransAction,-TransPrecond])
% translates the information necessary for precondition translation
       
precondTranslate(Me,Agent,Action,Precond,[TransRule,TransPrecond]) :-
        Action =.. [ActionName|_],
        translatePred(ActionName,TransRule),
        Precond =.. [PrecondName|PrecondArgs],
        translatePred(PrecondName,TransName),
        translateArgs(Me,Agent,PrecondArgs,TransPrecondArgs,[]),
        name('(',RightBracket),
        name(TransName,PrecondChars),
        append(RightBracket,PrecondChars,Start),
        append(Start,TransPrecondArgs,MostPrecond),
        name(' ?Sit1',SitArg),
        name(')',LeftBracket),
        append(MostPrecond,SitArg,SitPrecond),
        append(SitPrecond,LeftBracket,FullPrecond),
        name(TransPrecond,FullPrecond),
        write('This is the precondition I need to add: '),write(TransPrecond),nl.


% translateArgs(+PlanningAgent,+ServiceProvidingAgent,+ArgumentList,-TranslatedPrecond,+TransSoFar)
% translates each of the arguments of the new precondition, which may involve discussion with the service providing agent over classes.

translateArgs(_,_,[],TransPrecond,TransArgs) :-
        reverse(TransArgs,TransPrecond).

translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-
        FirstArg = uninstantiated,
        name(' Pseudo-Var',PV),
        reverse(PV,RevFirst),
        append(RevFirst,TransSoFar,NewTransSoFar),
        translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).
       
translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-
        % first, we need to find out the class of the first arg:
        class(FirstArg,FirstClass),
        write('I know that '),write(FirstArg),write(' is of class '),write(FirstClass),nl,
        name(' ?',ArgBegin),
        translatePred(FirstClass,TransFirst),
        name(TransFirst,TransFirstChars),
        append(ArgBegin,TransFirstChars,NewFirst),
        reverse(NewFirst,RevFirst),
        append(RevFirst,TransSoFar,NewTransSoFar),
        translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).

translateArgs(Me,Agent,[FirstArg|Rest],TransPrecond,TransSoFar) :-          
        % or ask the other agent:
        out(query(Me,Agent,question,class(FirstArg,FirstClass))),
        in_noblock(reply(Agent,Me,question,class(FirstArg,FirstClass))),
        write(Agent),write(' tells me that '),write(FirstArg),write(' is of class '),write(FirstClass),nl,
        name(' ?',ArgBegin),
        translatePred(FirstClass,TransFirst),
        name(TransFirst,TransFirstChars),
        append(ArgBegin,TransFirstChars,NewFirst),
        reverse(NewFirst,RevFirst),
        append(RevFirst,TransSoFar,NewTransSoFar),
        translateArgs(Me,Agent,Rest,TransPrecond,NewTransSoFar).


% findHisUnmatchedClass(+MyUnmatchedClasses,+HisUnmatchedClass,-UnmatchClass)
% calls emptyMyClasses

findHisUnmatchedClass(MyUnmatchedClasses,HisUnmatchedClasses,UnmatchedClass) :-
        emptyMyClasses(MyUnmatchedClasses,HisUnmatchedClasses,[UnmatchedClass]).


% emptyMyClasses(+MyUnmatchedClasses,+HisUnmatchedClasses,-[UnmatchedClass]).
% determines whether there is a class in HisUnmatchedClasses that is not also in MyUnmatchedClasses


emptyMyClasses([],Remainder,Remainder).

emptyMyClasses([_|MyRest],HisUnmatched,Remainder) :-
        member(['uninstantiated',Counter],HisUnmatched),
        deleteEl(HisUnmatched,['uninstantiated',Counter],NewUnmatched),
        emptyMyClasses(MyRest,NewUnmatched,Remainder).

% findClassAndPosition(+MyClasses,+HisClasses,-UnmatchedClass,-Position)
% returns the UnmatchedClass and its position

findClassAndPosition(MyClasses,HisClasses,UnmatchedClass,Position) :-
        reverse(MyClasses,RevMine),
        reverse(HisClasses,RevHis),
        findClassAndPosition(RevMine,RevHis,1,UnmatchedClass,Position).

findClassAndPosition([MyFirst|MyRest],[MyFirst|HisRest],PosSoFar,UnmatchedClass,Position) :-
        NewPos is PosSoFar + 1,
        findClassAndPosition(MyRest,HisRest,NewPos,UnmatchedClass,Position).

findClassAndPosition([MyFirst|_],_,Position,MyFirst,Position).



% checkFullyInstantiated(+Predicate)
% succeeds if predicate is fully instantiated; fails if any of the arguments are uninstantiated

checkFullyInstantiated(Predicate) :-
        Predicate =.. [_PredName|Arguments],
        checkArgsInstantiated(Arguments).

checkArgsInstantiated([]).

checkArgsInstantiated([FirstArg|Rest]) :-
        \+ FirstArg = fullInstantiationCheck,
        checkArgsInstantiated(Rest).


% findRightNegation(+MostRecentQuery,+PrecondList,-RightNegation)
% when a precondition is negated, returns the correct way in which this should be done (negate if already positive, make positive if already negated

findRightNegation(MostRecentQuery,PrecondList,posToNeg) :-
        member(MostRecentQuery,PrecondList),
        write('I have to negate this '),nl.

findRightNegation(MostRecentQuery,PrecondList,negToPos) :-
        member(not(MostRecentQuery),PrecondList),
        write('I have to unnegate this '),nl.


% findIndividual(+Position,+Query,-InstantiatedArgument)
% returns the argument in a given position of a query

findIndividual(Position,Query,InstArg) :-
        Query =.. [_|QueryArgs],
        findIndivArg(Position,QueryArgs,InstArg).

findIndivArg(1,[InstArg|_],InstArg).

findIndivArg(Position,[_FirstArg|RestArgs],InstArg) :-
        NewPosition is Position - 1,
        findIndivArg(NewPosition,RestArgs,InstArg).


% locateRule(+Action,+Justification,-Preconditions,-Postconditions)
% finds a specific action from the justification and returns its preconditions and postconditions.

locateRule(_,[],_,_) :-
%       write('this rule does not appear to be executable'),nl,
        fail.

locateRule(Action,[[Action,_,Preconds,Postconds,_,_]|_],Preconds,Postconds).

locateRule(Action,[_|RestJust],Preconds,Postconds) :-
        locateRule(Action,RestJust,Preconds,Postconds).








%fact(uniqueIdentifier(familyName,"$ORS_HOME/semantic_matching/ServiceProvidingAgents/html/DrinkingCup.html")).


%process_create('~/bin/Metric-FF/ff',['-o','domainOnt.pddl','-f','problemOnt.pddl'],[stdout(pipe(Stream))]),
       


