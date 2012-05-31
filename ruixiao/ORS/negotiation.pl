:- use_module(library(system)),use_module(library(lists)),  
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)),
   use_module(library(codesio)).



% takes as inputs the agents' utilities and the repair suggested by the ORS and: 1)finds the inverse repair 2)compares the agents' utilities 3)consults the negotiation ontology and builds a simple plan of how the agents
% will reach an agreement 4)consults the expert agent on demand 5)creates an ontology for every negotiation round. The ontology that is built is going to be provided to the negotiation parties. 




:- consult('NegotiationOnt.pl').
 %:-consult('thirdAgent.pl').





%domain/5
domain(initiator, Repair,ActionSpa,ParUtility, ThirdAgent):-
	asserta(hasRole(ActionSpa,participant)),
	asserta(theRepair(Repair)),
	hasUtility(initiator,IntUtility),
	write('** My utility for this negotiation round is: '),write(IntUtility),nl,nl,
	write('** I am assigned with the role of the initiator, therefore I am starting the negotiation'),nl,
	asserta(hasRole(participant,ActionSpa)),
	BothAgentsUtility=[IntUtility, ParUtility],
	bothAgentsUtility(BothAgentsUtility),
		((ThirdAgent==yes,
		suggestion(IntUtility, ParUtility, InitUtility, PartUtility),
		findInverseRepair(Repair,InverseRepair),
		findUtility(initiator,Repair, InitUtility),
		%write('the repair suggested for the SPA is : '), write(InverseRepair),
		findUtility(participant,InverseRepair, PartUtility),
		setCurrentUtilityFunction(InitUtility, PartUtility, CurrentUtility),
		begin(CurrentUtility, InitUtility, PartUtility,Repair));


		(findUtility(initiator,Repair, IntUtility),
		findInverseRepair(Repair,InverseRepair),
		findUtility(participant,InverseRepair, ParUtility),
		setCurrentUtilityFunction(IntUtility, ParUtility, CurrentUtility),	
		begin(CurrentUtility, IntUtility, ParUtility,Repair))
).


bothAgentsUtility(BothAgentsUtility):-
length(BothAgentsUtility,2).



participantWantsExpert(SpaExpert):-
asserta(participantExpert(SpaExpert)).


%bothAgentsExpert(+ThirdAgent,+SpaExpert, -Expert)
	bothAgentsExpert(yes,yes, Expert):-
	Expert=yes.

	bothAgentsExpert(yes,no, Expert):-
	Expert=no.

	bothAgentsExpert(no,yes, Expert):-
	Expert=no.

	bothAgentsExpert(no,no, Expert):-
	Expert=no.





%%% if bot agents agree to advice
%suggestion(+IntUtility, +ParUtility, InitUtility, PartUtility, +ThirdAgent):-


%%+ParUtility,-InitUtility,-PartUtility
suggestionBeforeNegotiation(ParUtility, InitUtility, Outcome):-
	
	expertKnowledge(Suggestion),
	believes(participant,PartKnowledge),	
	write('** The expert agent says that the correct one is the '),write(Suggestion),
	write(' the SPA believes that the correct one is the '), write(PartKnowledge),
	
	(Suggestion==PartKnowledge,
	write(' and it has right. '),
	Outcome=pa,
	write('  Therefore, the expert agent believes that I should make the repair. '),nl,nl);
	( write(' and is wrong. '),
	write('  Therefore, the expert agent believes that the SPA should make the repair. '),nl,nl,
	Outcome=spa).


applySuggestionBeforeNegotiation(ParUtility, InitUtility, PartUtility):-
	hasUtility(initiator,IntUtility),
	expertKnowledge(Suggestion),
	believes(participant,PartKnowledge),	
	
	(Suggestion==PartKnowledge,
	InitUtility is IntUtility+1,	
	retract(hasUtility(initiator,IntUtility)),
	asserta(hasUtility(initiator, InitUtility)),
	PartUtility=ParUtility);
	( PartUtility is ParUtility+1,
	InitUtility=IntUtility).





suggestion(IntUtility, ParUtility, InitUtility, PartUtility):-
	expertKnowledge(Suggestion),
	believes(participant,PartKnowledge),	
	write('** The expert agent says that the correct one is the '),write(Suggestion),
	write(' the SPA believes that the correct one is the '), write(PartKnowledge),
	
	(Suggestion==PartKnowledge,
	InitUtility is IntUtility+1,
	PartUtility=ParUtility,
	write(' and it has right. '),
	write('  Therefore, the expert agent believes that I should make the repair. '),nl,nl);
	( write(' and is wrong. '),
	write('  Therefore, the expert agent believes that the SPA should make the repair. '),nl,nl,
	PartUtility is ParUtility+1,
	InitUtility=IntUtility).

	
	%shouldDoTheRepair(Repair,SuggestedAgent),
	%(SuggestedAgent==initiator,
	%InitUtility is IntUtility+1,
	%PartUtility is ParUtility);
	%(SuggestedAgent==participant,
	%InitUtility is ParUtility+1,
	%InitUtility is IntUtility).





findInverseRepair(Repair,InverseRepair):-
	inverseOf(Repair,InversedRepair),
	InverseRepair=InversedRepair.



%begins the negotiation
begin(CurrentUtility, InitUtility, PartUtility,Repair):-
	CurrentState = start,
	plan(CurrentUtility, InitUtility, PartUtility, Repair, BothAgentsUtility, CurrentState, end, Messages).



%adds the relation hasUtility with the curent utility values 
findUtility(initiator, Repair, CurrentUtility):-
	asserta(hasUtility(pa, Repair, CurrentUtility)),
	tell('NegotiationOntology.pl'), 
	write('dynamic hasUtility/3.'), nl,
	listing(hasUtility/3), told.



findUtility(participant, Repair, CurrentUtility):-
	hasRole(participant,ActionSPA),
	asserta(hasUtility(ActionSPA, Repair, CurrentUtility)),
	tell('NegotiationOntology.pl'), 
	write('dynamic hasUtility/3.'), nl,
	listing(hasUtility/3), told.


%assign the negotiation parties roles
findParticipants(ActionSpa,pa):-
	asserta(hasRole(initiator,pa)),
	asserta(hasRole(participant,ActionSpa)),
	tell('NegotiationParties.pl'),
	write('dynamic hasRole/2.'),nl,
	listing(hasRole/2),
	told.

%set the initiator utility  and start the negotiation (it is called directly by the PA)
setInitiatorUtilityAndStartNegotiation(Utility):-
	asserta(hasUtility(initiator,InitUtility)).



%construct the plan , find the next applicable message based on the current state

plan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,CurrentState,end, Messages):-
	applicableMessages(CurrentUtility,InitUtility,PartUtility,Repair,CurrentState,Messages, NewCurrentState).












%checks whether the message is applicable to the current state.
%firstly the state preconditions must be true
%secondly the utility preconditions must be true
%the list of the messages' preconditions is of the form [State,Utility]
applicableMessages(CurrentUtility,InitUtility,PartUtility,Repair,CurrentState, Messages,NewCurrentState) :- 
	%write('current Utility'),write(CurrentUtility),nl,nl,
	NegotiationObject=Repair,
	ParticipantUtility=PartUtility,
	InitiatorUtility=InitUtility,
%this is for  cfp and receiveCfp messages, since they only have the state precondition	
	
									
			(negotiationRule(Message,Repair,Agent,[CurrentState,[]],Effect),						
%			write('mpika sto proto negotiation rule gia to currentState  '), write(CurrentState),nl,nl,
			NewCurrentState=Effect,
%			write('newCurrentState '),write(NewCurrentState),nl,nl,
			Messages=Message,
%			write('to messages einai to '),write(Messages),nl,nl,
	
			inverseOf(Repair,SpaRepair),
	       %constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NewCurrentState,NegotiationObject)
		  
									
			(							
			negotiationRule(Messages,Repair,initiator,[CurrentState,[]],Effect),
			Preconditions=[CurrentState],
			createOntology(initiator, Preconditions, Messages,Effect,CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NegotiationObject, CurrentState)
				
			);							
		%create the ontology
			(							
			negotiationRule(Messages,Repair,participant,[CurrentState,[]],Effect),
			Preconditions=[CurrentState],
			createOntology(participant, Preconditions, Messages,Effect,CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NegotiationObject, CurrentState)
			)
			
									
  
			);									




								
									


			(negotiationRule(Message, NegotiationObject, Agent, [CurrentState, 	
							 [hasUtility(initiator, NegotiationObject, InitiatorUtility),
							 hasUtility(participant, NegotiationObject, ParticipantUtility),CurrentUtility]],
					       Effect),

			inverseOf(Repair,SpaRepair),

%			write('The current negotiation phase is: '), write(CurrentState),			
%			write(' Agent '),write(QAgent),write(' sends a '), write(Message),write(' for the repair: '), write(SpaRepair),nl,
			NewCurrentState=Effect,
	  		Messages=Message,
			(negotiationRule(Messages, NegotiationObject, initiator, [CurrentState,	%8 
							 [hasUtility(initiator, Repair, InitUtility),
							 hasUtility(participant, Repair, PartUtility),CurrentUtility]],Effect),
			Preconditions=[CurrentState, 
							 [hasUtility(initiator, Repair, InitUtility),
							 hasUtility(participant, Repair, PartUtility)]],
			createOntology(initiator, Preconditions, Messages,Effect,CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,Repair, CurrentState)
			%constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NewCurrentState));

			);								%8

			(negotiationRule(Messages, NegotiationObject, participant, [CurrentState, 				%9
							 [hasUtility(initiator, Repair, InitUtility),
							  hasUtility(participant, Repair,PartUtility),CurrentUtility]],Effect),
			Preconditions=[CurrentState, 
							 [hasUtility(initiator, Repair, InitUtility),
							 hasUtility(participant, Repair, PartUtility)]],
			createOntology(participant, Preconditions, Messages,Effect,CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,Repair, CurrentState)
			%createOntology(initiator, Preconditions, Message,Effect),
			%constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NewCurrentState)
			)								

).											







%setCurrentUtilityFunction(+InitiatorUtility,+ParticipantUtility,-CurrentUtility) CurrentUtility is the relatioship between the initiator's and the participant's utility
setCurrentUtilityFunction(InitUtility, PartUtility, CurrentUtility):-
	(PartUtility=\=0,
	InitUtility<PartUtility,
	
	CurrentUtility ='InitiatorUtility<ParticipantUtility');
	(PartUtility=\=0,
	InitUtility>PartUtility,
	
	CurrentUtility ='InitiatorUtility>ParticipantUtility');

			(PartUtility=:=0,
						
			
			 CurrentUtility='ParticipantUtility=:=0');
	(PartUtility==InitUtility,	
		suggestion(InitUtility, PartUtility, IntUtility, ParUtility),
		setCurrentUtilityFunction(IntUtility,ParUtility,CurrentUtility)).
									
			
										

										



%hasBothAgentsUtility chacks whether both utilities have been obtained 
%hasBothAgentsUtility(BothAgentsUtility) :-
%length(BothAgentsUtility,2).


%apply(+Messages, +NewCurrentState) applies the first message from the list L. Normally this list must coontain only one element
%apply(Messages, NewCurrentState):-

%write('AFTI EINAI I LISTA ME TA APPLICABLE MESSAGES '), nl, write(Messages), write('kai afto einai to current state: '), write(CurrentState).


constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,end) :-
	consult('~/trunk/andriana/new-ors/ORS/NegotiationOntology.pl'),
	open('~/trunk/andriana/new-ors/ORS/NegotiationOntology.pl',append,H),
	findall((Messages, Effect), negotiationRule(Messages, [Agent,
								[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],
							       Preconditions],
						    Effect]), List),
		
	dump(H, List).
%createHistory.

dump(H, []) :- close(H).
%assert(negotiationIs(over)), negotiationIs(L).
dump(H, [(M, end) | T]) :- 
	(M==initiatorDoesRepair,
	%hasRole(ActionSpa, participant),
	suBclass(Repair,negotiationObject),
	     %these are comments because of a a bug in the sicstus version I'm using
	%write(H, 'agreementrule(initiatorDoesRepair,NegotiationObject,[initiator,participant],'),write(H,'[end,[hasUtility(initiator, NegotiationObject,'),write('InitiatorUtility),hasUtility(participant, NegotiationObject, 	ParticipantUtility),initiatorUtility>participantUtility],agreed(initiator,NegotiationObject)]). '),nl,nl,
	%write(H,'obligationrule( send(initiator,initiatorDoesRepair), NegotiationObject, initiator,'),write(H, '[proposalRefused,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),initiatorUtility>participantUtility],obligated(initiator, NegotiationObject)]). '),nl,nl,

	write(H,'obligated(initiator,'),write(H,Repair),write(H,'). '),nl,
	write(H,'instanceOF('),write(H, M),write(H,','),write(H,'negotiationMessageTerminator'),write(H,'). '),nl,
	write(H,'instanceOF('),write(H,end),write(H,','),write(H,'negotiationPhase). '),nl,
	%write(H,'hasRole('),write(H,ActionSpa),write(H,',participant'),write(H,'). '),nl,
	%write(H,'hasRole(pa, initiator). '),nl,nl,
	write(H,'agreed(initiator,'),write(H,Repair),write(H,'). '),nl,
	
	assert(negotiationIs(over)), negotiationIs(L), 
	dump(H, T) );
	(M==participantDoesRepair,
	suBclass(Repair,RepairType),
	suBclass(RepairType,negotiationObject),
	%hasRole(ActionSpa, participant),
	inverseOf(Repair,InversedRepair),
        %these are comments because of a a bug in the sicstus version I'm using
	%write(H,'obligationrule(send(participant,participantDoesRepair), NegotiationObject, participant, [proposalAccepted,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),initiatorUtility<participantUtility],obligated(participant, NegotiationObject)]). '),nl,nl,
	write(H,'obligated(participant,'),write(H,InversedRepair),write(H,'). '),nl,
	write(H,'relation(agreed(participant,'),write(H,InversedRepair),write(H,')). '),nl,	
	write(H,'instanceOF('),write(H, M),write(H,','),write(H,'negotiationMessageTerminator'),write(H,'). '),nl,
	write(H,'instanceOF('),write(H,end),write(H,','),write(H,'negotiationPhase). '),nl,
	%write(H,'hasRole('),write(H,ActionSpa),write(H,',participant'),write(H,'). '),nl,
	%write(H,'hasRole(pa, initiator). '),nl,nl,
	assert(negotiationIs(over)), negotiationIs(L),
	%abolish(hasRole/2),
	abolish(hasUtility/2),
	%abolish(isWinner/1), 
	dump(H, T)).
dump(H, [(M, E) | T]) :-
	E\=end,
	instanceOf(M,NegotiationMessageCategory),
	subClass(NegotiationMessageCategory, negotiationMessage),
	instanceOf(E,NegotiationPhaseCategory),
	subClass(NegotiationPhaseCategory, negotiationPhase),
	
	write(H,'instanceOF('),write(H,M),write(H,','),write(H,NegotiationMessageCategory),write(H,'). '),nl,
	
	
	write(H,'instanceOF('),write(H,E),write(H,','),write(H,'negotiationPhase). '),nl,
	dump(H, T).


%createHistory:-

%	consult('~/trunk/andriana/new-ors/ORS/NegotiationOntology.pl'),
%	open('~/trunk/andriana/new-ors/ORS/NegotiationOntologyHistory.pl',append,H),

	

%	findall((Messages, Effect), negotiationRule(Messages, [Agent,
%								[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],
%							       Preconditions],
%						    Effect]), List),
%	write(H, 'class( '),write(H,Repair),write(H,',negotiationObject).'),
%	write(H,Messages),nl,nl,write(H,Effects),nl,nl,	
%	dump(H, List).


constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NewCurrentState):-

	plan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,NewCurrentState,end, Messages).



%% makes available to the agents the path of the specific negotiation ontology for this negotiation round
negototiationOntologyPath(NegoOntoPath):-
	NegoOntoPath='~/trunk/andriana/new-ors/ORS/NegotiationOntology.pl'.


%% makes available to the agents the path of the ontology which conatins the protocol 
protocolPath(ProtocolPath):-
	ProtocolPath='~/trunk/andriana/new-ors/ORS/NegotiationOnt.pl'.

%create the Ontologies depending on the negotiation role each agent has
%the rules must be of the form 
%rule(receiveCall,[initiator,[class(NegotliiationObject,negotiationObject),class(participant,agent),class(InitiatorUtility, utility), class(ParticipantUtility, utility),[callSent,[]],callReceived).

createOntology(Agent, Preconditions, Messages,Effect,CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,Repair,CurrentState):-
	instanceOf(Repair,ClssNegotiationObject),
	subClass(ClssNegotiationObject, NegotiationObject),
	subClass(NegotiationObject, NegotiationProtocol),
	instanceOf(Message,NegotiationMessageType),
	subClass(NegotiationMessageType,NegotiationMessage),
	%subClass(NegotiationMessage, NegotiationProtocol),
	inverseOf(Repair,InversedRepair),
	instanceOf(CurrentState,NegotiationPhaseClass),
	subClass(NegotiationPhaseClass, NegotiationPhase),
	%subClass(NegotiationPhase, NegotiationProtocol),!,
	assert(negotiationRule(Messages,
		[Agent,
		[[class(Repair,negotiationObject),class(initiator,negotiationPartyRole),class(participant,negotiationPartyRole),class(InitUtility, utility), class(PartUtility, utility)],Preconditions],Effect])),

	tell('~/trunk/andriana/new-ors/ORS/NegotiationOntology.pl'), 
	write(':-dynamic negotiationRule/2, suBclass/2, instanceOF/2, relation/1.'), nl,
	write('instanceOF('),write(Repair),write(','),write(ClssNegotiationObject),write(').'),nl,
	write('suBclass('),write(ClssNegotiationObject),write(','),write(NegotiationObject),write(').'),nl,	
	write('instanceOF(initiator,negotiationPartyRole).'),nl,
	write('instanceOF(participant,negotiationPartyRole).'),nl,
	write('suBclass(negotiationPartyRole,negotiationParty).'),nl,
	write('suBclass('),write(NegotiationObject),write(','),write(NegotiationProtocol),write(').'),nl,
	write('suBclass('),write(NegotiationParties),write(','),write(NegotiationProtocol),write(').'),nl,
	write('suBclass(negotiationMessageInitiator,negotiationMessage).'),nl,
	write('suBclass(negotiationMessageReactor,negotiationMessage).'),nl,
	write('suBclass(negotiationMessageTerminator,negotiationMessage).'),nl,
	write('suBclass(negotiationMessage,negotiationProtocol).'),nl,
	write('suBclass(negotiationPhase,negotiationProtocol).'),nl,
	write('suBclass(rule,negotiatiationProtocol).'),nl,
	write('suBclass(negotiationRule,rule).'),nl,
	write('suBclass(agreementRule,rule).'),nl,
	write('suBclass(obligationRule,rule).'),nl,
	
	write('suBclass(negotiationProtocol,thing).'),nl,
	write('relation(hasRole(negotiationParty,initiator)).'),nl,
	write('relation(hasRole(negotiationParty,participant)).'),nl,
	write('relation(hasParticipant(negotiationProtocol, negotiationParty)).'),nl,
	write('relation(hasObject(negotiationProtocol, negotiationObject)).'),nl,
	write('relation(hasPhase(negotiationProtocol, negotiationPhase)).'),nl,
	write('relation(governedBy(negotiationProtocol, rules)).'),nl,
	write('relation(hasMessage(negotiationProtocol, negotiationMessage)).'),nl,
	write('relation(correspondTo(negotiationMessage, negotiationPhase)).'),nl,
	write('relation(negotiatiateOver(negotiationParty,negotiationObject)).'),nl,
	write('relation(hasUtility(negotiationParty, negotiationObject, utility)).'),nl,
	write('relation(inverseOf('),write(Repair),write(','),write(InversedRepair),write(')).'),nl,
	write('relation(hasPhase(negotiationMessage, negotiationPhase)).'),nl,
	write('relation(send(negotiationParty,negotiationMessage)).'),nl,

	listing(negotiationRule/2),
	told,
	constructPlan(CurrentUtility,InitUtility,PartUtility,Repair,BothAgentsUtility,Effect).



theScenario(Scenario):-
%ExpertScenario = Scenario,
findExpert(Scenario). 

findExpert(ExperScenario):-
atom_concat('~/trunk/andriana/new-ors/agent_environment/scenarios/', ExperScenario, ExpertScenario),
atom_concat( ExpertScenario, '/thirdAgent.pl', ThirdAgentScenario),
consult(ThirdAgentScenario).




setInitiatorUtility(InitUtility):-
asserta(hasUtility(initiator,InitUtility)).


setParticipantKnowledge(PartKnowledge):-
asserta(believes(participant,PartKnowledge)).

