%:- use_module(library('linda/client')).




% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).



spa(MyName) :-
    nl,write('I am '),write(MyName),nl,
    listen(MyName).

% note: spa1 is for spa's which have kif onts and therefore need to translate.  ultimately this will replace spa.
spa1(MyName) :-
	nl,write('I am '),write(MyName),nl,
	scenario_name(Scenario),
	
	ontoType(OntType),
	copyOntFiles(OntType,MyName,Scenario),
	
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translation', Ontol_transAbsolutePath),
	consult(Ontol_transAbsolutePath),
	
	find_ontology_path(Scenario,MyName,ScenarioPath),
	translation(Scenario,ScenarioPath),
		reconsultOntology(ScenarioPath),
	nl,nl,nl,
	listen(MyName).

listen(MyName) :-
	
    in(query(QAgent,MyName,QueryType,Query)),!,
    nl,write(MyName),write(' has received a query ..'),nl,
    write(Query),write(' '),write(QueryType),nl,nl,
    resolve(MyName,QueryType,Query,QAgent),
    listen(MyName).











% resolve(+QueryType,+Query,+QAgent): solves the query and sends the answer to the querying agent.

%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%

resolve(MyName,repair,[Scenario,InverseRepairType,RepairInfo],QAgent) :-
	nl,write('* I was asked to repair my ontology.'),nl,
	solve(MyName,repair,[Scenario,InverseRepairType,RepairInfo],NegOutcome,QAgent),
	out(reply(MyName,QAgent,[Scenario,InverseRepairType,RepairInfo],NegOutcome)),
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translation', Ontol_transAbsolutePath),
	consult(Ontol_transAbsolutePath),
	find_ontology_path(Scenario,MyName,ScenarioPath),
	translation(Scenario,ScenarioPath),
	reconsultOntology(ScenarioPath).
	%nl,write('* I replied to the repair request by giving my Utility for this repair.'),nl.
	
resolve(MyName,repair2,[Scenario,InverseRepairType,RepairInfo],QAgent) :-
	nl,write('* I was asked again to repair my ontology. '),nl,
	solve(MyName,repair2,[Scenario,InverseRepairType,RepairInfo],NegOutcome,QAgent),
	out(reply(MyName,QAgent,[Scenario,InverseRepairType,RepairInfo],NegOutcome)),
	get_absolute_path('/ORS/translation/Ontolingua_to_Prolog/translation', Ontol_transAbsolutePath),
	consult(Ontol_transAbsolutePath),
	find_ontology_path(Scenario,MyName,ScenarioPath),
	translation(Scenario,ScenarioPath),
	reconsultOntology(ScenarioPath),
	nl,write('* I replied to the second repair request.'),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%NEGOTIATION                                                          % %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



resolve(MyName,negotiation,[participantDoesRepair,end],QAgent):-	
	write('* The negotiation is over. The outcome is that I am performing the repair '), nl,nl,
	write('* I do the repair!. '),nl,nl,
	refineInfo(Scenario,InverseRepairType,ont,RepairInfo),
%write(RepairInfo),nl,nl,	
%repairOutcome2(lowProtection,[Scenario,InverseRepairType,RepairInfo],MyName,doneSPA).
% refine(Scenario,RepairType,onto,RepairInfo).	
	refineAgent(Scenario,InverseRepairType,onto,RepairInfo,MyName),
		find_ontology_path(Scenario,MyName,ScenarioPath),
	translation(Scenario,ScenarioPath),
	reconsultOntology(ScenarioPath).
   
resolve(MyName,negotiation,[TQuery,end],QAgent):-
write('* The negotiation is over. The outcome is : '), write(TQuery),nl,nl,
out(reply(MyName,QAgent,negotiation,[TQuery,end])).




resolve(MyName,negotiation,[TQuery,NegotiationPhase],QAgent):-
 
negotiationPath(PathName),
consult(PathName),

%% first negotiationRule pair : two similar rules that do not contain the utility functions initiator-participant
(negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('* I sent a '),write(MyQuery),write(' for the negotiation object'),write(Repair),nl,nl
%write('-- initiator - participant apla rules '),nl,nl
);
		%% initiator - initiator
(negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(FQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(reply(MyName,QAgent,negotiation,[FQuery,NegotiationPhase])),
write('* I received a '),write(MyQuery),write(' for the negotiation object'),write(Repair),nl,nl,

write('* I need more details to continue the negotiation '),nl,nl
%write('-- initiator - initiator apla rules '),nl,nl
);

		%%participant - participant
(negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('* I sent a '),write(MyQuery),write(' for the negotiation object'),write(Repair),nl,nl
%write('-- participant - participant apla rules '),nl,nl
);


		%%participant - initiator
(negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('* I received a '),write(MyQuery),write('  for the negotiation object'),write(Repair),nl,nl,

write('* I need more details to continue the negotiation '),nl,nl
);






%% second negotiationRule pair : two non similar rules %%initiator - participant
(
negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('* I sent a '),write(MyQuery),write('  for the negotiation object'),write(Repair),nl,nl
%write('-- initiator - participant different rules '),nl,nl
);

		%% initiator - initiator
(
negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(FQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('* I received a '),write(TQuery),write('  for the negotiation object'),write(Repair),nl,nl,

write('* I need more details to continue the negotiation '),nl,nl
%write('-- initiator - initiator different rules '),nl,nl
);

		%% participant - participant

(







negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),








out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),

write('* I sent a '),write(MyQuery),write(' for the negotiation object'),write(Repair),nl,nl
%write('-- participant - participant different rules '),nl,nl
);

		%% participant - initiator

(







negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),








out(reply(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('* I received a '),write(TQuery),write(' for the negotiation object'),write(Repair),nl,nl,

write('* I need more details to continue the negotiation '),nl,nl
);










%% third negotiationRule pair : two similar rules with the utility functions   %%initiator - participant
(



negotiationRule(TQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('* I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl

%write('-- initiator - participant complicates rules '),nl,nl


);


		%%initiator -initiator

(



negotiationRule(TQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(FQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('* I received a '),write(TQuery),write(' for the negotiation object'),write(Repair),nl,nl,

write('* I need more details to continue the negotiation '),nl,nl


%write('-- initiator - initiator complicates rules '),nl,nl



);

		%%participant - participant




(



negotiationRule(TQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('* I sent a '),write(MyQuery),write(' for the negotiation object'),write(Repair),nl,nl



%write('-- participant - participant complicates rules '),nl,nl


);
		%%participant - participant




(



negotiationRule(TQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(reply(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('* I received a '),write(TQuery),write(' for the negotiation object'),write(Repair),nl,nl

%write('** I need more details to continue the negotiation '),nl,nl

).


%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%





resolve(MyName,request,Query,QAgent) :-
	solve(MyName,request,Query,Answer,QAgent),
	write('I am saying '),write(Answer),write(' to '),write(Query),nl,
	out(reply(MyName,QAgent,Query,Answer)).




resolve(MyName,QueryType,Query,QAgent) :-
	solve(MyName,QueryType,Query,Answer,QAgent),
	write('I am saying '),write(Answer),write(' to '),write(Query),nl,
	out(reply(MyName,QAgent,QueryType,Answer)).

resolve(MyName,_,Query,QAgent) :-
	out(reply(MyName,QAgent,Query,problem)).


% solve(+QueryType,+Action,-Outcome,+QueryAgent): tries to perform Action, which is of QueryType, for the QueryingAgent, and returns the Outcome.

	

solve(MyName,request,Action,ok,Agent) :-
	write('I want to perform '),nl,write(Action),write('for '),write(Agent),nl,nl,
	tasksIPerform(TasksList),	
	write(' tasks I perform: '),write(TasksList),nl,
	Action =.. [ActionName|_],
	write('Action= '),write(Action),nl,
	write('ActionName= '),write(ActionName),nl,
	member(ActionName,TasksList),
	
	rule(Action,[_,Preconds,_]),
	write('Preconds='),write(Preconds),nl,
	ask(AskList),nl,
	wait(WaitList),
	write('I\'m checking preconds...'),nl,nl,
	checkPreconds(MyName,Preconds,AskList,WaitList,Agent,[],[]),
	write(Action),write(' has been completed satisfactorily.'),nl,nl.
	











solve(_MyName,request,Action,problem,_Agent) :-
	tasksIPerform(TasksList),	
	Action =.. [ActionName|_],
	\+ member(ActionName,TasksList),
	write('don\'t perform '),write(Action),nl,nl.










%% **** horrible hack for Agnieszka's evaluation ***

solve(MyName,request,Action,ok,Agent) :-
	write('I want to perform '),nl,write(Action),write('for '),write(Agent),nl,nl,
	tasksIPerform(TasksList),	
	Action =.. [ActionName|_],
	member(ActionName,TasksList),
	addBogusArg(Action,BogusAction),
	rule(BogusAction,[_,Preconds,_]),
 	ask(AskList),nl,
	wait(WaitList),
	write('I\'m checking preconds...'),nl,nl,
	checkPreconds(MyName,Preconds,AskList,WaitList,Agent,[],[]),
	write(Action),write(' has been completed satisfactorily.'),nl,nl.


addBogusArg(Action,BogusAction) :-
	Action =.. [ActionName|ActionArgs],
	bogusArg(ActionArgs,BogusArgs),
	BogusAction =.. [ActionName|BogusArgs].

bogusArg([FirstArg|Rest],[FirstArg|[_|Rest]]).


solve(_MyName,request,_,problem,_) :-
	write('Sorry, I cannot perform the task you request!'),nl,nl.

solve(_MyName,question,performTask(Action,_),Answer,_Agent) :-
	write('question is '),write('performTask('),write(Action),write(')'),nl,
	write('checking tasks ...'),
	tasksIPerform(TasksList),
	write(TasksList),nl,
	Action =.. [Pred|_Args],
	( member(Pred,TasksList) ->
	    Answer = yes
	;   
	    Answer = no
	),
	write('answer is '),write(Answer).

solve(_MyName,question,Question,Question,_Agent) :-
	nonFacts(PredList),
        Question =.. [QuePred|_Args],
	member(QuePred,PredList),
	Question.

solve(_MyName,question,Question,Question,_Agent) :-
	fact(Question).

solve(_MyName,truth,class(Thing,Class),yes,_Agent) :-
	checkClass(class(Thing,Class)).

solve(_MyName,truth,class(_Thing,_Class),no,_Agent).

solve(_MyName,truth,subclass(Class1,Class2),yes,_Agent) :-
	checkSubClass(Class1,Class2).

solve(_MyName,truth,subclass(_Class1,_Class2),no,_Agent).

solve(_MyName,truth,Query,yes,_Agent) :-
	nonFacts(PredList),
	member(Query,PredList),
	Query.

solve(_MyName,truth,Query,yes,_Agent) :-
	fact(Query).

solve(_MyName,truth,_Query,no,_Agent).

%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%

solve(MyName,repair,[Scenario,InverseRepairType,RepairInfo],[protected,Outcome],pa) :-
	asserta(refineInfo(Scenario,InverseRepairType,ont,RepairInfo)),		
	write('** The repair is: '),write(InverseRepairType),nl,
	write('* Information I received: '),write(RepairInfo),nl,
	get_absolute_path('/ORS/refinement/Ontolingua_refinement/refinement',RefinePath),
	consult(RefinePath),
	consultMetaAgent(Scenario,InverseRepairType,onto,RepairInfo,[protected,Outcome,_Level],MyName,Utility),
	write('**For the repair '),write(InverseRepairType),write(' my Utility is '),write(Utility),nl,nl,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     expert agent's opinion?	
	repairOutcome([protected,Outcome],Utility).

solve(MyName,repair2,[Scenario,InverseRepairType,RepairInfo],Result,pa) :-
		
	asserta(refineInfo(Scenario,InverseRepairType,ont,RepairInfo)),	
	get_absolute_path('/ORS/refinement/Ontolingua_refinement/refinement', RefinePath),
        consult(RefinePath),
	consultMetaAgent(Scenario,InverseRepairType,onto,RepairInfo,[protected,_Outcome,Level],MyName,Utility),
	repairOutcome2(Level,[Scenario,InverseRepairType,RepairInfo],MyName,Result).
	
	
% repairOutcome(+NegOutcome)
% behaves accordingly to the decision

repairOutcome([protected,ok],Utility) :-
	%write('* I agree to perform the repair.'),nl,!,
out(reply(pa,MyName,question,Utility)),
	write('** I sent my utlity for this repair: '),write(Utility),nl,nl,
	in(query(pa,MyName,expert,SpaExpert)),
	write('** I was asked if I want an expert advice. '),nl,nl,
	MyExpert=yes,	
	out(reply(pa,MyName,expert,yes)),
	write('** And I replied yes. '),nl,nl,
	response(yes).
		
response(no):-
%in(query(pa,MyName,expert,no)),
	write('We did not agree to ask for the advice. '),nl,nl,



	
	in(query(pa,MyName,negotiation,NegoOntoPath)),
	write('** I received the negotiation ontology path: '),write(NegoOntoPath),nl,nl,

	consult(NegoOntoPath),
	write('**The negotiation ontology has been consulted. I am assigned with the role of the participant '),nl,nl,
	asserta(negotiationPath(NegoOntoPath)),
	out(reply(pa,MyName,negotiation,ok)).
	
	response(yes):-
	(in(query(pa,MyName,expert,ExpOutcome)),
	write('PA informed me that the expert suggested the agent '),write(ExpOutcome),write('.'),nl,nl,
	MyExpOutcome=yes,
	out(reply(pa,MyName,expert,MyExpOutcome)),
	write('I do want to follow this advice. '),nl,nl,
	


	
	in(query(pa,MyName,negotiation,NegoOntoPath)),
	write('** I received the negotiation ontology path: '),write(NegoOntoPath),nl,nl,

	consult(NegoOntoPath),
	write('**The negotiation ontology has been consulted. I am assigned with the role of the participant '),nl,nl,
	asserta(negotiationPath(NegoOntoPath)),
	out(reply(pa,MyName,negotiation,ok))).
	
	
repairOutcome([protected,no],Utility) :-	
	out(reply(pa,MyName,question,Utility)),
	write('** I sent my utlity for this repair: '),write(Utility),nl,nl,
	in(query(pa,MyName,expert,SpaExpert)),
	write('** I was asked if I want an expert advice. '),nl,nl,
	out(reply(pa,MyName,expert,yes)),
	MyExpert=yes,
	write('** And I replied yes. '),nl,nl,
	response(MyExpert).


	%in(query(pa,MyName,expert,ExpOutcome)),
	%write('PA informed me that the expert suggested the agent '),write(ExpOutcome),write('.'),nl,nl,
	%MyExpOutcome=no,
	%out(reply(pa,MyName,expert,MyExpOutcome)),
	%write('I do bot want to follow this advice. '),nl,nl,
	%in(query(pa,MyName,negotiation,NegoOntoPath)),
	%write('** I received the negotiation ontology path: '),write(NegoOntoPath),nl,nl,
%	consult(NegoOntoPath),

%	asserta(negotiationPath(NegoOntoPath)),
%	out(reply(pa,MyName,negotiation,ok)).
	
a:-
negotiationRule(Query,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],Effect]),


negotiationRule(MyQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Effect]],MyEffect]),

write('I received a query '), write(Query),write(' for the repair '), write(Repair),nl,nl,


out(reply(pa,MyName,negotiation,MyQuery)),
checkForDoubleMessages(MyQuery,MyEffect),
write('I am replying '),write(MyQuery),write(' for the repair '), write(Repair),nl,nl.





startNegotiation(Query,QAgent):-
%negotiationPath(PathName),
%consult(PathName),
(
negotiationRule(Query,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],Effect]),


negotiationRule(MyResponse, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Effect,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

write('I have not given much information about the decision I made for the negotiation. I must give more details.Previously I sent a  '),write(Query),write('I have to be more psecific '),nl,nl,


out(reply(pa,MyName,negotiation,MyResponse)),
%checkForDoubleMessages(MyQuery,MyEffect),
write('I am replying '),write(MyResponse),write(' for the repair '), write(Repair),nl,nl);



(
negotiationRule(Query, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],Effect]),


negotiationRule(MyResponse, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Effect,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

write('I have not given much information about the decision I made for the negotiation. I must give more details.Previously I sent a  '),write(Query),write('I have to be more psecific '),nl,nl,


out(reply(pa,MyName,negotiation,MyResponse)),
%checkForDoubleMessages(MyQuery,MyEffect),
write('I am replying '),write(MyResponse),write(' for the repair '), write(Repair),nl,nl).














	
% repairOutcome2(+NegOutcome)
% second attempt to repair
	
repairOutcome2(lowProtection,[Scenario,InverseRepairType,RepairInfo],MyName,doneSPA) :-
	refineAgent(Scenario,InverseRepairType,onto,RepairInfo,MyName),
	nl,write('* I finally agree to perform the repair'),nl,!.
		
		
repairOutcome2(highProtection,_,_,noneSPA) :-
	nl,write('* I refuse again. '),nl,!.		
	
% reconsultOntology(Scenario)
% reconsults signature and theory at the beginning and after any attepmt to repair
	
reconsultOntology(ScenarioPath) :-
	atom_concat(ScenarioPath, '/centralThy', ThyPath),
	atom_concat(ScenarioPath, '/centralSig', SigPath),
	reconsult(ThyPath),
	reconsult(SigPath).	

%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%
	

%checkPreconds(+Spa,+PrecondList,+AskList,+WaitList,+QueryAgent,+ThingsToAsk,+WaitingThings): in order to perform an action, the agent must check that all the Preconds are valid.  Some of these the agent can check itself; others must be asked of the QueryAgent (these types of things are listed in the AskList and built up in ThingsToAsk); others - such as calculations - can only be evaluated once all variables have been instantiated (these types of things are lists in WaitList and built up in WaitingThings). When checking classes: either the agent knows that Thing is of type Class (great), or that thing is of a different class and this different class (which leads to failure: we can assume this wrongclass is not a subclass of Class, or else the first checkClass would have succeeded), or it knows nothing and asks the PA.

checkPreconds(MyName,[],_,_,Agent,ThingsToAsk,WaitingThings) :-
	checkWithAgent(MyName,ThingsToAsk,Agent),!,
	checkWaitingThings(WaitingThings).


% checkPreconds(MyName,[class(Thing,Class)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
% 	checkClass(class(Thing,Class)),!,
% 	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

% checkPreconds(_MyName,[class(Thing,Class)|_T],_AskList,_WaitList,_Agent,_ThingsToAsk,_WaitingThings) :-
% 	class(Thing,WrongClass),!,
%         Class \= WrongClass,       
%         fail.

checkPreconds(MyName,[class(Thing,Class)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :- !,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[class(Thing,Class)|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	member(Pred,AskList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[not(H)|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	\+ Pred = not,
	member(Pred,AskList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,[H|ThingsToAsk],WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	H =.. [Pred|_Args],
	\+ Pred = not,
	member(Pred,WaitList),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,[H|WaitingThings]).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	nonFacts(PredList),
	member(H,PredList),
	\+ H,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	nonFacts(PredList),
	member(H,PredList),
	H,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[not(H)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	\+ fact(H),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[H|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	fact(H),!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).

checkPreconds(MyName,[calculation(Sum)|T],AskList,WaitList,Agent,ThingsToAsk,WaitingThings) :-
	Sum,!,
	checkPreconds(MyName,T,AskList,WaitList,Agent,ThingsToAsk,WaitingThings).



%checkWithAgent(+Spa,+ListOfQuestions,+Agent): all those preconditions which require an answer are put to Agent.
%Note: the variable binding Question = Answer is inelegant: better to use the same variable name and if the binding is impossible, we fail.  However, this means that the spa will only look for messages such that Answer can match Question: if Answer is something unexpected (such as 'no'), then the spa will wait indefinitely for message that will match, rather than this predicate failing.

checkWithAgent(_MyName,[],_).

checkWithAgent(MyName,[Question|T],Agent) :- 
	write('have asked '),write(Agent),write(' about '),write(Question),nl,
	out(query(MyName,Agent,question,Question)),
	in(reply(Agent,MyName,question,Answer)),
	write(Agent),write(' has replied '),write(Answer),nl,
	Question = Answer,
	checkWithAgent(MyName,T,Agent).

	    
%checkWaitingThings(+ListOfWaitingThings): once all the other preconditions have been evaluated, all possible variables have been bound, so the list of waiting things can be evaluated.

checkWaitingThings([]).

checkWaitingThings([calculation(Sum)|T]) :-
	Sum,!,
	checkWaitingThings(T).

checkWaitingThings([H|T]) :-
	fact(H),
	checkWaitingThings(T).


%checkClass(class(?Object,?Class)): if given, evaluates whether Objects is of class Class (or a subclass of Class); otherwise, instantiates these variables appropriately.

checkClass(class(Object,Class)) :-
	class(Object,Class).

checkClass(class(Object,Class)) :-
	subclass(SubClass,Class),
	checkClass(class(Object,SubClass)).

%checkSubClass(?SubClass,?Class): succeeds if Subclass is a subclass of Class, or, if these are variables, there is an instantiation which makes this possible.

checkSubClass(SubClass,Class) :-
	subclass(SubClass,Class).

checkSubClass(SubClass,Class) :-
	subclass(MidClass,Class),
	checkSubclass(SubClass,MidClass).

copyOntFiles(sumo,_MyName,_Scenario).

copyOntFiles(onto,MyName,Scenario) :-
	find_original_ontology_path(Scenario,MyName,OrigScenarioPath),
	find_ontology_path(Scenario,MyName,ScenarioPath),
	atom_concat(OrigScenarioPath,'/ont.in',OrigOnt),
	atom_concat(ScenarioPath,'/ont.in',OntIn),
	see(OrigOnt),tell(OntIn),
	copyOntFile,
	told,seen,
	atom_concat(OrigScenarioPath,'/metaOnt.in',OrigMetaOnt),
	atom_concat(ScenarioPath,'/metaOnt.in',MetaOntIn),
	see(OrigMetaOnt),tell(MetaOntIn),
	copyOntFile,
	told,seen.


copyOntFile :-
	readLine(Line),
	(   extractLast(Line,end,Rest),nl,
	    copyThisLine(Rest)
	;
	    copyThisLine(Line),
	    copyOntFile
	).


% readLine(-Text)
% reads the current stream until it reaches the end of the line

readLine(Text):-
   get_code(C),
   addtoline(C, Text).

% addtoline(+character,-text)
% checks if this is a new line or end of file character.  if not builds adds it to the text of the line and returns to readLine.
 
addtoline(C, []):-
    newline(C).

addtoline(C,[end]) :-
	endoffile(C).
 
addtoline(C, [C|More]):-
    readLine(More).

readLine(Stream,Text):-
	get_code(Stream, C),
	addtoline(Stream, C, Text).

addtoline(_, C, []):-
	newline(C).

addtoline(_, C, [end]):-
	endoffile(C).

addtoline(Stream, C, [C|More]):-
	readLine(Stream, More).

newline(10).
 
endoffile(-1).

% extractLast(+List,?Last,-Rest)
% returns the last element of a list (which may or may not be specified: if specified, it will fail if the last is not the specified element), together with the rest of the list (with the last element removed).

extractLast(List,Last,Rest) :-
	extractLast(List,Last,[],RevRest),
	reverse(RevRest,Rest).
	
extractLast([H],H,Rest,Rest).

extractLast([H|T],Last,RestSoFar,Rest) :-
	extractLast(T,Last,[H|RestSoFar],Rest).

% copyThisLine(+Line)
% write Line as is to the out stream

copyThisLine(Line) :-
	name(LineName,Line),
	write(LineName),nl.




find_ontology_path(Scenario,MyName,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/SPA/',Scenario3),
	atom_concat(Scenario3,MyName,Scenario4),
	atom_concat(Scenario4,'/updated',ScenarioPath).


find_original_ontology_path(Scenario,MyName,ScenarioPath) :-
	environ('ORS_HOME', ORSHomePath),
	atom_concat(ORSHomePath,'/agent_environment/scenarios/',Scenario1),
	atom_concat(Scenario1,Scenario,Scenario2),
	atom_concat(Scenario2,'/SPA/',Scenario3),
	atom_concat(Scenario3,MyName,Scenario4),
	atom_concat(Scenario4,'/original',ScenarioPath).

matchExpression(Before,Identifier,After,Whole) :-
	findPosition(Identifier,Whole,Pos1,Pos2),
	findBefore(Whole,Pos1,[],Before),
	findAfter(Whole,Pos2,After).

% findPosition(+Identifier,+Whole,-BeginPos,-AfterPos)
% returns the start and end positions of the identifier within whole

findPosition([FirstID|Rest],Whole,Pos1,Pos1,EndPos) :-
	nth1(Pos1,Whole,FirstID),
	findPosition([FirstID|Rest],Whole,Pos1,EndPos).

findPosition([Last],Whole,EndPos,EndPos) :-
	nth1(EndPos,Whole,Last).

findPosition([FirstID|Rest],Whole,CurrentPos,EndPos) :-
	nth1(CurrentPos,Whole,FirstID),
	NextPos is CurrentPos + 1,
	findPosition(Rest,Whole,NextPos,EndPos).


% findBefore(+Whole,+Position,+BeforeSoFar,-Before)
% returns what comes before position in whole

findBefore(_,1,RevBefore,Before) :-
	reverse(RevBefore,Before).

findBefore([FirstWhole|RestWhole],Counter,BeforeSoFar,Before) :-
	NewCounter is Counter - 1,
	findBefore(RestWhole,NewCounter,[FirstWhole|BeforeSoFar],Before).

% findAfter(+Whole,+Position,-After)
% returns what comes after position in whole

findAfter(Whole,Pos2,After) :-
	length(Whole,End),
	Start is Pos2 +1,
	findAfter(Whole,Start,End,[],After).

findAfter(_,Last,End,RevAfter,After) :-
	Last is End + 1,
	reverse(RevAfter,After).

findAfter(Whole,Counter,End,AfterSoFar,After) :-
	nth1(Counter,Whole,Element),
	NewCounter is Counter + 1,
	findAfter(Whole,NewCounter,End,[Element|AfterSoFar],After).


deleteEl(List,El,Ans) :- 

	deleteEl(List,El,[],Ans). 



deleteEl([H|T],El,SoFar,Ans) :-

	H == El,
	append(SoFar,T,Ans).



deleteEl([H|T],El,Ans,X) :- 

	append(Ans,[H],SoFar), 
	deleteEl(T,El,SoFar,X). 
