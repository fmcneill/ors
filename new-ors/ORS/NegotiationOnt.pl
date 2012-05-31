%This is a top dow ontology of the negotiation protocol
%:- dynamic hasUtility/3.


%messages are of the form rule(Action,Agent,[Precondition],Effect), where Action is the name of the message; Agent indicates which agent is allowed to send this message; Precondition is a list of the form [State, UtilityPrecond]; UtilityPrecond is of the form InitiatorCurrentUtility < ParticipantCurrentUtility or the opposite.

negotiationRule(cfp,  NegotiationObject,initiator, [start,[]], callSent).

negotiationRule(receiveCall, NegotiationObject, participant, [callSent,[]], callReceived).

negotiationRule(refuseCall, NegotiationObject, participant, [callReceived,  
							[hasUtility(initiator, NegotiationObject, InitiatorUtility),
							hasUtility(participant,NegotiationObject, ParticipantUtility),'ParticipantUtility=:=0']],
						  callRefused). %participant refuses to participte if its Utility is 0

negotiationRule(propose, NegotiationObject, participant, [callReceived, 
							 [hasUtility(initiator, NegotiationObject, InitiatorUtility),
							 hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility>ParticipantUtility']],
					       proposalSent). 


negotiationRule(propose, NegotiationObject, participant, [callReceived, 
							 [hasUtility(initiator, NegotiationObject, InitiatorUtility),
							 hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility<ParticipantUtility']],
					       proposalSent). 


negotiationRule(acceptProposal, NegotiationObject, initiator, [proposalSent,[hasUtility(initiator, NegotiationObject, InitiatorUtility), hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility<ParticipantUtility']], proposalAccepted).

negotiationRule(refuseProposal, NegotiationObject, initiator, [proposalSent, [hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility>ParticipantUtility']], proposalRefused).

negotiationRule(participantDoesRepair, NegotiationObject, participant, [proposalAccepted, [hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility<ParticipantUtility']], end).

negotiationRule(initiatorDoesRepair, NegotiationObject, initiator, [proposalRefused, [hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility>ParticipantUtility']], end).

negotiationRule(initiatorDoesRepair, NegotiationObject, initiator, [participantLeftNegotiation, [hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'ParticipantUtility=:=0']], end).


negotiationRule(failure, NegotiationObject, participant, [callRefused, [hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'ParticipantUtility=:=0']],participantLeftNegotiation).


obligationRule( send(initiator,initiatorDoesRepair), NegotiationObject, initiator, [proposalRefused,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility>ParticipantUtility'],obligated(initiator, NegotiationObject)]).
obligationRule(send(participant,participantDoesRepair), NegotiationObject, participant, [proposalAccepted,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility<ParticipantUtility'],obligated(participant, NegotiationObject)]).

agreementRule(initiatorDoesRepair,NegotiationObject,[initiator,participant],[end,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility>ParticipantUtility'],agreed(initiator,NegotiationObject)]).


agreementRule(participantDoesRepair,NegotiationObject,[initiator,participant],[end,[hasUtility(initiator, NegotiationObject, InitiatorUtility),hasUtility(participant, NegotiationObject, ParticipantUtility),'InitiatorUtility<ParticipantUtility'],agreed(participant,NegotiationObject)]).






%class hierarchy


subClass(negotiationProtocol,thing).
subClass(negotiationProcess, negotiationProtocol).
subClass(negotiationParty, negotiationProtocol).
subClass(negotiationMessage, negotiationProtocol).
subClass(negotiationObject, negotiationProtocol).

subClass(negotiationPhase, negotiationProtocol).
subClass(rule, negotiationProtocol).



%%% utility einai class or property?
subClass(participantUtility, utility).
subClass(initiatorUtility, utility).
subClass(utility, negotiationProtocol).



subClass(changePredicateName, negotiationObject).
subClass(changePredicateArity, negotiationObject).
subClass(changePredicateArgumentType, negotiationObject).
subClass(changePrecondition,negotiationObject).
subClass(changeInstanceDefinition, negotiationObject).


%messages
subClass(negotiationMessageInitiator, negotiationMessage).
subClass(negotiationMessageReactor, negotiationMessage).
subClass(negotiationMessageTerminator, negotiationMessage).


%disjoint rule
dijointClass(X,Y):-
instanceOf(A,X),
\+(instanceOf(A,Y)).

%disjoint classes
disjointClass(negotiationMessage, negotiationParty).
disjointClass(negotiationMessage, negotiationPhase).
disjointClass(negotiationMessage, utility).
disjointClass(negotiationObject,negotiationParty).
disjointClass(negotiationObject,negotiationMessage).
disjointClass(negotiationObject,utility).
disjointClass(negotiationParty,utility).

%the rules
subClass(negotiatioRule,rule).
subClass(obligationRule,rule).
subClass(agreementRule,rule).


%the permitted negotiation messafes
instanceOf(cfp,negotiationMessageInitiator).
instanceOf(receiveCall,negotiationMessageReactor).
instanceOf(refuseCall,negotiationMessageReactor).
instanceOf(refuseProposal,negotiationMessageReactor).
instanceOf(acceptProposal,negotiationMessageReactor).
instanceOf(propose,negotiationMessageReactor).
instanceOf(participantDoesRepair,negotiationMessageTerminator).
instanceOf(failure,negotiationMessageReactor).
instanceOf(initiatorDoesRepair,negotiationMessageTerminator).


%the negotiation party
subClass(negotiationPartyRole, negotiationParty).

instanceOf(initiator, negotiationPartyRole).
instanceOf(participant, negotiationPartyRole).
%instanceOf(PA,initiator).
%instanceOf(SPA, participant).



%the negotiation phases


subClass(negotiationPhaseInitial,negotiationPhase).
subClass(negotiationPhaseIntermediate,negotiationPhase).
subClass(negotiationPhaseFinal,negotiationPhase).


instanceOf(start,negotiationPhaseInitial).
instanceOf(callSent,negotiationPhaseIntermediate).
instanceOf(callReceived,negotiationPhaseIntermediate).
instanceOf(proposalSent,negotiationPhaseIntermediate).
instanceOf(proposalAccepted,negotiationPhaseIntermediate).
instanceOf(proposalRefused,negotiationPhaseIntermediate).
instanceOf(callRefused,negotiationPhaseIntermediate).
instanceOf(end,negotiationPhaseFinal).
instanceOf(participantLeftNegotiation,negotiationPhaseIntermediate).


%the negotiationObject hierarchy

subClass(changePredicate, negotiationObject).
subClass(changeActionRule, negotiationObject).

subClass(changePredicateName, changePredicate).
subClass(changePredicateArity, changePredicate).
subClass(changePredicateArgument, changePredicate).
subClass(changePrecondition, changeActionRule).





%instanceOf(change_class_definition,changePredicateName).
instanceOf(predicateAA,changePredicateName).
instanceOf(predicateA,changePredicateName).


instanceOf(propositionalA,changePredicateArity).
instanceOf(propositionalAA,changePredicateArity).


instanceOf(domainA,changePredicateArgument).
instanceOf(domainAA,changePredicateArgument).
instanceOf(switchArgs,changePredicateArgument).

%instanceOf(change_of_type,changePredicateArgumentType).
%instanceOf(change_instance_definition, changeInstanceDefinition).
instanceOf(preconditionAA, changePrecondition).
instanceOf(preconditionA, changePrecondition).



%%%%MENTAL NOTE: na valw ti eidous predicate einai afto ie oti kai na allaksei i seira twn arguments exei to idio noima
inverseOf(predicateAA,predicateA).
inverseOf(predicateA,predicateAA).
inverseOf(propositionalAA,propositionalA).
inverseOf(propositionalA,propositionalAA).
inverseOf(domainAA,domainA).
inverseOf(domainA,domainAA).
inverseOf(switchArgs,switchArgs).
inverseOf(change_of_type,change_of_type).
inverseOf(predicateAA,predicateA).
inverseOf(predicateA,predicateAA).
inverseOf(change_instance_definition,change_instance_definition).
inverseOf(preconditionAA,preconditionA).
inverseOf(preconditionA,preconditionAA).
inverseOf(negated_precondition,negated_precondition).
inverseOf(change_class_definition,change_class_definition).
inverseOf(precondAA,precondA).
inverseOf(precondA,precondAA).






%properties


property(hasRole(negotiationParty,initiator)).
property(hasRole(negotiationParty,participant)).
property(hasParticipant(negotiationProtocol, negotiationParty)).
property(hasObject(negotiationPrtocol, negotiationObject)).
property(hasPhase(negotiationProtocol, negotiationPhase)).
property(governedBy(negotiationProtovol, rule)).
property(hasMessage(negotiationProtocol, negotiationMessage)).
property(correspondTo(negotiationMessage, negotiationPhase)).
property(negotiatiateOver(negotiationParty,negotiationObject)).
property(hasUtility(negotiationParty, negotiationObject, utility)).
property(inverseOf(negotiationObject,negotiationObject)).
property(obligated(negotiationParty, negotiationObject)).
property(hasPhase(negotiationMessage, negotiationPhase)).
property(send(negotiationParty,negotiationMessage)).
property(agreed(negotiationParty,NegotiationObject)).


%%% new properties

property(participates(negotiationPaarty,rule)).
