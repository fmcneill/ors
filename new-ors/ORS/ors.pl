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
%       write('messages are '),write(NewMessages).


% ORS needs to listen out for the particular kind of message that indicates failure, and use this as a cue to diagnose problems.


% note: commented code allows ORS to act proactively: automatically diagnosing the problem when it sees that a problem has occurred.  But at the moment we are assuming that the PA asks for a diagnosis, so it is not needed.
% proxyIn(reply(SPA,PA,Outcome)) :-
%       in(reply(SPA,PA,problem)),
%       diagnose(Diagnosis),
%       permission(Diagnosis),
%       performRefinement(Diagnosis).

% proxyIn(reply(SPA,PA,Outcome)) :-
%       sleep(1),
%       in(reply(SPA,PA,problem)),
%       diagnose(Diagnosis),
%       permission(Diagnosis),
%       performRefinement(Diagnosis).


proxyIn(Message) :-
in_noblock(Message),
messages(Messages),
retract(messages(Messages)),
append(Messages,[in(Message)],NewMessages),
assert(messages(NewMessages)).
%       write('messages are '),write(NewMessages).

proxyIn(Message) :-
sleep(1),
in_noblock(Message),
messages(Messages),
retract(messages(Messages)),
append(Messages,[in(Message)],NewMessages),
assert(messages(NewMessages)).
%       write('messages are '),write(NewMessages).


requestDiagnosis(Plan,Diagnosis,ProtectionInfo,OntType,Scenario,OnOrOff) :-
deconstruct(Plan,Just),
messages(Messages),
write('I am requesting a diagnosis ...'),
findLast(Messages,in(reply(Agent,Me,Action,problem))),
test,
findQueries(Messages,Agent,Me,Action,Just,ListOfQueries,ListOfSurprisingQuestions),
examineAction(Action,Just,Agent,ListOfQueries,ListOfSurprisingQuestions,ExpRefinements,OnOrOff),
diagnoseFailure(Me,OntType,Scenario,Action,Agent,ListOfQueries,Just,ListOfSurprisingQuestions,ExpRefinements,Outcome,Diagnosis),
checkOutcome(Me,Outcome,Scenario,Agent),
saveExps(CorrectAgent,Action,ExpRefinements),
shareExps(OnOrOff,[Agent,Action,ListOfSurprisingQuestions,ListOfQueries,Diagnosis]).


checkOutcome(_Me,success,_Agent,_Scenario).
checkOutcome(_Me,failure,_Agent,_Scenario).
checkOutcome(Me,[protect,Suggestion],Scenario,Agent) :-
out(request(Me,ActionSPA,repair,Suggestion)),
in_noblock(reply(ActionSPA,Me,repair,Outcome)),
persuationOutcome(ActionSPA,Scenario,Outcome).

persuationOutcome(_ActionSPA,_Scenario,ok).

persuationOutcome(ActionSPA,Scenario,no) :-
refineMeta(Scenario,agent,ActionSPA).





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
