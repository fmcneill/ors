% This is the front end of ORS: it is called when an agent starts interacting with other agents.  Its job is to listen in on the conversation between the PA and the SPAs and track surprising questions.  When the PA's plan execution fails, it will call ORS and ORS will use this knowledge to diagnose the problem.  It will then suggest a patch to the PA and, if the PA agrees, it will implement this refinement.

ors(PA) :-
    get_absolute_path('/ORS/plan_deconstruction/planDecon', PlanDeconPath),
    consult(PlanDeconPath),
    get_absolute_path('/ORS/diagnosis/diagnosticAlgorithm', DiagPath),
    consult(DiagPath),
    get_absolute_path('/ORS/refinement/Ontolingua_refinement/refinement', RefPath),
    consult(RefPath),
    get_absolute_path('/ORS/refinement/Ontolingua_refinement/metaRefinement', MetaRefPath),
    consult(MetaRefPath),
    get_absolute_path('/ORS/exp_sharing/expClient.pl',ExpClient),
    consult(ExpClient),
    assert(messages([])).


proxyOut(Message) :-
    out(Message),
    messages(Messages),
    retract(messages(Messages)),
    append(Messages,[out(Message)],NewMessages),
    assert(messages(NewMessages)).
%    write('messages are '),write(NewMessages).


% ORS needs to listen out for the particular kind of message that indicates failure, and use this as a cue to diagnose problems.


% note: commented code allows ORS to act proactively: automatically diagnosing the problem when it sees that a problem has occurred.  But at the moment we are assuming that the PA asks for a diagnosis, so it is not needed.
% proxyIn(reply(SPA,PA,Outcome)) :-
%     in(reply(SPA,PA,problem)),
%     diagnose(Diagnosis),
%     permission(Diagnosis),
%     performRefinement(Diagnosis).

% proxyIn(reply(SPA,PA,Outcome)) :-
%     sleep(1),
%     in(reply(SPA,PA,problem)),
%     diagnose(Diagnosis),
%     permission(Diagnosis),
%     performRefinement(Diagnosis).


proxyIn(Message) :-
    in_noblock(Message),
    messages(Messages),
    retract(messages(Messages)),
    append(Messages,[in(Message)],NewMessages),
    assert(messages(NewMessages)).
%    write('messages are '),write(NewMessages).

proxyIn(Message) :-
    sleep(1),
    in_noblock(Message),
    messages(Messages),
    retract(messages(Messages)),
    append(Messages,[in(Message)],NewMessages),
    assert(messages(NewMessages)).
%    write('messages are '),write(NewMessages).


%%%%%%%%%%%%%%%%%
%%% PROTECTED %%%
%%%%%%%%%%%%%%%%%

% requestDiagnosis(+Plan,-Diagnosis,-NegOutcome,-OntType,+Scenario,+OnOrOff)
% find a diagnosis of the repair

requestDiagnosis(Plan,Diagnosis,NegOutcome,OntType,Scenario,OnOrOff,Agent) :-
    deconstruct(Plan,Just),
    messages(Messages),
	
    write('** I am requesting a diagnosis ...'),
    findLast(Messages,in(reply(Agent,Me,Action,problem))),
    findQueries(Messages,Agent,Me,Action,Just,ListOfQueries,ListOfSurprisingQuestions),
	setParticipantKnowledge(ListOfSurprisingQuestions),
	write('** The list of surprising questions are '),write(ListOfSurprisingQuestions),nl,nl,    
	examineAction(Action,Just,Agent,ListOfQueries,ListOfSurprisingQuestions,ExpRefinements,OnOrOff),
    diagnoseFailure(Me,OntType,Scenario,Action,Agent,ListOfQueries,Just,ListOfSurprisingQuestions,ExpRefinements,RepairOutcome,Diagnosis),
	asserta(ontType(OntType)),
	asserta(scenario(Scenario)),
    nl,write('** ORS proposed the following repair: '),write(Diagnosis),nl,
	%inverseRepairType(RepairType,InverseRepairType),     
	checkOutcome(Me,RepairOutcome,NegOutcome,Scenario,Agent),
    shareExps(OnOrOff,[Agent,Action,ListOfSurprisingQuestions,ListOfQueries,Diagnosis]).
   
% checkOutcome(+Me,+RepairOutcome,-NegOutcome,+Scenario,+Agent)
% if PA's ontology is protected start negotiation


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Andriana Negotiation%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

checkOutcome(_Me,[_protected,RepairType,RepairInfo,Protection],[protected,Result],Scenario,Agent) :-

	
	asserta(repairInfo(RepairInfo)),
	asserta(repair(RepairType)),
	inverseRepairType(RepairType,InverseRepairType),
	theScenario(Scenario),    
%inverseRepairType(RepairType,InverseRepairType),
	write('** The inverse repair which I am going to negotiate is '),write(InverseRepairType),nl,nl,
    
	%if the repair does not have an inverse, then no negotiation is possible
	( InverseRepairType == no,
	Result= ok
	
	);
	(
	inverseRepairType(RepairType,InverseRepairType), 
	nl,write('** I am informing the SPA: '),write(Agent),write(' about the repair the ORS suggested, so he can check his utility. The repair I am sending his is '),write(InverseRepairType),nl,
    	out(query(pa,Agent,repair,[Scenario,InverseRepairType,RepairInfo])),
	write('** The cause of the failure is the  '),write(RepairInfo),nl,nl,%write('This is the scenario  '),write(Scenario),nl,nl,	
	%% kalese to works, kai apothikefse ta scenario, inverseRepair, Repair kai Repair Info gia ton third agent
	in(reply(pa,Agent,question,SpaUtility)),
	write('** I received a response for SPA Utility, which is'), write(SpaUtility),nl,nl,
	write('the repair for the negotiation is '),write(RepairType),nl,nl,
	out(query(pa,Agent,expert,SpaExpert)),
	write('** I want to consult an expert agent. '),
	MyAdvice=yes,
	write('** I just asked agent '),write(Agent), write(' if it also wants to get the expert agent advice. '),nl,nl,
	in(reply(pa,Agent,expert,SpaExpert)),
	write('** It replied '), write(SpaExpert),write(' .'),nl,nl,
	suggestionExpert(Agent, MyAdvice,SpaExpert,SpaUtility,Outcome, SpaNewUtility),
		
%	MyExpertOutcome=yes,
	
%%%%%%%%%%%%%%%%%%%%%%%%%%% ask spa for expert.	
	domain(initiator,RepairType,Agent,SpaNewUtility,no),!,
	negotiationIs(over),    
	negototiationOntologyPath(NegoOntoPath),
	asserta(negotiationOnto(NegoOntoPath)),
	out(query(pa,Agent,negotiation,NegoOntoPath)),
	write('** I am sending to the SPA the negotiation ontology path:  '),write(NegoOntoPath),nl,nl,
	in(reply(pa,Agent,negotiation,ok)),	

	consult(NegoOntoPath),
	
	negotiationRule(Messages,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[start]],Effect]),
	out(query(MyName,Agent,negotiation,[Messages,Effect])),
	write('** I am sending a '),write(Messages),write(' query to the SPA,'),write(Agent), write(' for the repair '),write(Repair),nl,nl,
	in(reply(Agent,MyName,negotiation,[Response,NegotiationPhase])),
	%write('** I received a '),write(Response), write(' for the repair '),write(Repair),nl,nl,
	continueTheNegotiation(Response,Agent,NegotiationPhase)).



suggestionExpert(Agent, MyAdvice,SpaExpert,SpaUtility,Outcome, SpaNewUtility):-

(MyAdvice==yes,
SpaExpert=yes,
suggestionBeforeNegotiation(SpaUtility, InitUtility, Outcome),

write('** I just Informed the '),write(Agent),write(' about the advice outcome and asked it whether it wants to follow this advice. '),	
	out(query(pa,Agent,expert,Outcome)),
	in(reply(pa,Agent,expert,SpaExpertOutcome)),
MyExpertOutcome=yes,
	applySuggestionExpert(SpaExpertOutcome, MyExpertOutcome,SpaUtility, SpaNewUtility)






);
(Outcome=no,
SpaNewUtility= SpaUtility,
%out(query(pa,Agent,expert,no)),
write('** We did not agree to consult an expert agent. '),nl,nl).


applySuggestionExpert(yes, MyExpertOutcome,SpaUtility, SpaNewUtility):-

MyExpertOutcome==yes,
write('** We agreed to follow this advice. '),nl,nl,
%SpaExpertOutcome=yes,
applySuggestionBeforeNegotiation(SpaUtility, InitUtility,SpaNewUtility).

applySuggestionExpert(no, MyExpertOutcome,SpaUtility, SpaNewUtility):-
write('** We did not agree to follow the advice. '),
SpaNewUtility=SpaUtility,nl,nl.


	%checkOutcome(idid,[_protected,_RepairType,_RepairInfo,_Protection],[protected,ok],Scenario,Agent):-
	%Result=ok.



continueNegotiation(Response,QAgent,Effect):-

(


%% first two different rules
negotiationRule(Response,
		[Agent,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Precond]],Effect]),


negotiationRule(MyResponse, [Agent,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Effect,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),
write('as I told you before, I received a  '),write(Response),write('whose precond is '),write(Precond),write('and whose effect is  '),write(Effect),nl,nl,

out(query(MyName,QAgent,negotiation,[MyResponse,MyEffect])),
write('** I am sending a '),write(MyResponse),write(' to the SPA for the repair '),write(Repair),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegotiationPhase])),
write('** I received a '),write(Responsed),nl,nl,


continueNegotiation(Responsed,QAgent,NegotiationPhase)
);
(

negotiationRule(Response, [Agent,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Precond,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],Effect]),
negotiationRule(MyResponse, [Agent,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Effect,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),


out(query(MyName,QAgent,negotiation,[MyResponse,MyEffect])),
write('** I am sending a '),write(MyResponse),write(' to the SPA for the repair '),write(Repair),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegotiationPhase])),
write('** I received a '),write(Responsed),nl,nl,


continueNegotiation(Responsed,QAgent,NegotiationPhase));

(

out(query(MyName,QAgent,negotiation,[Response,Effect])),


write('** I need more details about the SPA negotiation decision'),nl,nl,

in(reply(QAgent,MyName,negotiation,[Responsed,NegotiationPhase])),
write('** I received a '),write(Responsed),nl,nl,


continueNegotiation(Responsed,QAgent,NegotiationPhase)).
	
continueTheNegotiation(initiatorDoesRepair,QAgent,end):- 

write('** The negotiation is over. I am performing the repair '),

abolish(negotiationRule/2),
abolish(suBclass/2),
abolish(isWinner/1),
negotiationOnto(NegoOntoPath),
delete_file(NegoOntoPath),


iamPerformingTheRepair.

iamPerformingTheRepair:-

asserta(repairResult(donePA)),
ontType(OntType),
scenario(Scenario),
repairInfo(RepairInfo),
repair(RepairType),
	write(' '),write(RepairType),
	write(' in '), write(RepairInfo),
	refine(Scenario,RepairType,onto,RepairInfo).
	%checkOutcome(idid,[_protected,_RepairType,_RepairInfo,_Protection],[protected,ok],Scenario,Agent).





continueTheNegotiation(Responsed,QAgent,end):- 
out(query(MyName,QAgent,negotiation,[Responsed,end])),
write('-- The negotiation is over. The repair is performed by the '),write(QAgent),nl,nl,

abolish(negotiationRule/2),
abolish(suBclass/2),
abolish(isWinner/1),

negotiationOnto(NegoOntoPath),
delete_file(NegoOntoPath),

asserta(repairResult(doneSPA)),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



   %resolve(MyName,negotiation,[TQuery,NegotiationPhase],QAgent):-
 
continueTheNegotiation(TQuery,QAgent,NegotiationPhase):-

%	
%% first negotiationRule pair : two similar rules that do not contain the utility functions initiator-participant
(negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),

write('** I need more details to continue the negotiation '),nl,nl,
%write('-- initiator - participant apla rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)





);
		%% participant - participant
(negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(FQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),

write('** I received a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,

out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),

write('** I need more details to continue the negotiation '),nl,nl,
%write('-- participant - participant apla rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)



);

		%%initiator - initiator
(negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,
%write('-- initiator - initiator apla rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)



);




		%%participant - initiator
(negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[NegotiationPhase]],MyEffect]),
out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
%write('-- participant - initiator apla'),nl,nl,
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,
%write('-- initiator - initiator apla rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)



);




%% second negotiationRule pair : two non similar rules %%initiator - participant
(
negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),

write('** I need more details to continue the negotiation '),nl,nl,
%write('-- initiator - participant different rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)




);

		%% participant - participant
(
negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(FQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),
%write('--- edw eimai '),nl,nl,
out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('** I received a '),write(TQuery),write(' for the negotiation object '),write(Repair),nl,nl,

%write('-- participant - participant different rules'),nl,nl,
write('** I need more details to continue the negotiation '),nl,nl,

in(reply(QAgent,MyName,negotiation,[FQuery,NegPhase])),
	
write('** I received a '),write(FQuery),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(FQuery,QAgent,NegPhase)



);

		%% initiator - initiator

(




negotiationRule(TQuery,
		[initiator,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),








out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,
%write('-- initiator - initiator different rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)


);



		%% participant - initiator

(




negotiationRule(TQuery,
		[participant,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],[Preconditions]],NegotiationPhase]),


negotiationRule(MyQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),








out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,

in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)


);










%% third negotiationRule pair : two similar rules with the utility functions   %%initiator - participant
(



negotiationRule(TQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),

write('** I need more details to continue the negotiation '),nl,nl,



%write('-- initiator - participant complicated rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)



);


		%%participant - participant

(



negotiationRule(TQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(FQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],Effect]),


out(query(MyName,QAgent,negotiation,[TQuery,NegotiationPhase])),
write('** I received a '),write(TQuery),write(' for the negotiation object '),write(Repair),nl,nl,




write('** I need more details to continue the negotiation '),nl,nl,
%write('-- participant - participant complicated rules'),nl,nl,


in(reply(QAgent,MyName,negotiation,[FQuery,NegPhase])),
	
write('** I received a '),write(FQuery),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(FQuery,QAgent,NegPhase)


);

		%%initiator - initiator




(



negotiationRule(TQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),

out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,
%write('-- initiator - initiator complicated rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),
%write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,



continueTheNegotiation(Responsed,QAgent,NegPhase)




);


		%%participant - initiator




(






negotiationRule(TQuery, [participant,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[Preconditions,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],NegotiationPhase]),


negotiationRule(MyQuery, [initiator,[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility,utility),class(PartUtility,utility)],[NegotiationPhase,[hasUtility(initiator,Repair,InitUtility),hasUtility(participant,Repair,PartUtility)]]],MyEffect]),
%write('------ participant - initiator complicated'),nl,nl,
out(query(MyName,QAgent,negotiation,[MyQuery,MyEffect])),
write('** I sent a '),write(MyQuery),write(' for the negotiation object '),write(Repair),nl,nl,
%write('-- initiator - initiator complicated rules'),nl,nl,
in(reply(QAgent,MyName,negotiation,[Responsed,NegPhase])),


write('** I received a '),write(Responsed),write(' for the negotiation object '),write(Repair),nl,nl,

%%%%%%%%%%%% na valw edw ena if 

continueTheNegotiation(Responsed,QAgent,NegPhase)




).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






















	%waitForAnswer(Agent,[protected,Res1]),
    %afterResponse(Res1,Protection,Outcome,RepairType,InverseRepairType,RepairInfo,Scenario,Agent,Result ).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ¬Andriana Negotiation%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5   
checkOutcome(_Me,success,none,_Scenario,_Agent).


afterResponse(no,highProtection,Outcome,_RepairType,InverseRepairType,RepairInfo,Scenario,Agent,Result) :-
    nl,write(' * '),write(Agent),write(' cannot do it'),nl,
    lowProtectionSPA(Outcome,InverseRepairType,RepairInfo,Scenario,Agent,Result),!.
    
afterResponse(no,lowProtection,Outcome,_RepairType,InverseRepairType,RepairInfo,Scenario,Agent,Result) :-
    nl,write(' * '),write(Agent),write(' cannot do it'),nl,
    lowProtectionPA(Outcome,RepairType,RepairInfo,Scenario,Agent,Result),!.
    
afterResponse(ok,_Protection,_Outcome,_RepairType,_InverseRepairType,_RepairInfo,_Scenario,Agent,_) :-
    write('* '),write(Agent),write(' replied ok to repair request '),!.


% lowProtectionSPA(no,+RepairType,+RepairInfo,+Scenario,+Agent,-Result)
% if PA's protection is low and SPA did not perform, PA performs the repair

lowProtectionSPA(no,InverseRepairType,RepairInfo,Scenario,Agent,Result) :-
    write('* My protection is high.'),nl,
    write('* I am asking SPA to try again.'),nl,
    out(query(pa,Agent,repair2,[Scenario,InverseRepairType,RepairInfo])),
    waitForAnswer(Agent,Result),!.
   
% lowProtectioPA(ok/no,+RepairType,+RepairInfo,+Scenario,-Result)
% if PA's protection is low and SPA did not perform, PA performs the repair

lowProtectionPA(no,RepairType,RepairInfo,Scenario,Agent,donePA) :-
    write('* '),write(Agent),write(' could not do it.'),nl,
    write('* My protection is low. '),nl,
    write('* I am performing the repair'),nl,
    refine(Scenario,RepairType,onto,RepairInfo),!.
   
lowProtectionPA(ok,RepairType,RepairInfo,Scenario,Agent,_).
   
   
% inverseRepairType(+RepairType,-InverseRepairType)
% having the repair of an PA, find a repair for SPA.

inverseRepairType(propositionalAA,X) :-
    =(propositionalA,X),!.

inverseRepairType(propositionalA,X) :-
    =(propositionalAA,X),!.

inverseRepairType(precondAA,X) :-
    =(no,X),
write('*Hmm this repair does not have an inverse... so I can not negotiate!'),nl,nl,
iamPerformingTheRepair,!.



 %   =(precondA,X),!.

inverseRepairType(precondA,X) :-
    =(no,X),
write('*Hmm this repair does not have an inverse... so I can not negotiate!'),nl,nl,
iamPerformingTheRepair,!.


 %   =(precondAA,X),!.

inverseRepairType(switchArgs,X) :-
    =(switchArgs,X),!.
   
inverseRepairType(domainA,X) :-
    =(domainAA,X),!.
   
inverseRepairType(domainAA,X) :-
    =(domainA,X),!.       

inverseRepairType(predicateAA,X) :-
    =(predicateA,X),!.
   
inverseRepairType(predicateA,X) :-
    =(predicateAA,X),!.
%% the following repairs do not have an inverse one
inverseRepairType(negatePrecond,X):-
    =(no,X),
write('*Hmm this repair does not have an inverse... so I can not negotiate!'),nl,nl,
iamPerformingTheRepair,!.

inverseRepairType(removePostcond,X):-
    =(no,X),
write('*Hmm this repair does not have an inverse... so I can not negotiatiate!'),nl,nl,!,
iamPerformingTheRepair.
inverseRepairType(changeOfType,X):-
    =(no,X),
write('*Hmm this repair does not have an inverse... so I can not negotiatiate!'),nl,nl,
iamPerformingTheRepair,!.





   
%%%%%%%%%%%%%%%%%%
%%% ~PROTECTED %%%
%%%%%%%%%%%%%%%%%%


%findQueries(+Messages,+Agent,+Me,+Action,+Justification,-ListOfQueries,-SurprisingQuestions)
%finds the messages relating to this action by ignoring anything that came before the most recent request to perform the action, then finds which of those are queries, and which of these are surprising.

findQueries(Messages,Agent,Me,Action,Justification,ListOfQueries,SurprisingQuestions) :-
    matchExpression(Before,[out(query(Me,Agent,request,Action))],RelevantMessages,Messages),
    \+ member(out(query(Me,Agent,request,Action)),Before),
    findQueries(RelevantMessages,Agent,Me,Action,Justification,[],[],ListOfQueries,SurprisingQuestions).

findQueries([],_Agent,_Me,_Action,_Justification,ListOfQueries,SurprisingQuestions,ListOfQueries,SurprisingQuestions).

findQueries([in(query(Agent,Me,question,Query))|Rest],Agent,Me,Action,Justification,QueriesSoFar,SQsSoFar,ListOfQueries,SurprisingQuestions) :-
    locateRule(Action,Justification,Preconds,_Postconds),
    (   member(Query,Preconds)
    ;
        member(not(Query),Preconds)
    ),
    findQueries(Rest,Agent,Me,Action,Justification,[Query|QueriesSoFar],SQsSoFar,ListOfQueries,SurprisingQuestions).

findQueries([in(query(Agent,Me,question,Query))|Rest],Agent,Me,Action,Justification,QueriesSoFar,SQsSoFar,ListOfQueries,SurprisingQuestions) :-
    findQueries(Rest,Agent,Me,Action,Justification,[Query|QueriesSoFar],[Query|SQsSoFar],ListOfQueries,SurprisingQuestions).


findQueries([_FirstMessage|Rest],Agent,Me,Action,Justification,QueriesSoFar,SQsSoFar,ListOfQueries,SurprisingQuestions) :-
    findQueries(Rest,Agent,Me,Action,Justification,QueriesSoFar,SQsSoFar,ListOfQueries,SurprisingQuestions).



findLast(Messages,in(reply(Agent,Me,Action,problem))) :-
    last(Messages,in(reply(Agent,Me,Action,problem))).

findLast(Messages,in(reply(Agent,Me,Action,problem))) :-
    reverse(Messages,[_WrongLast|RestMessages]),
    findLast(RestMessages,in(reply(Agent,Me,Action,problem))).
