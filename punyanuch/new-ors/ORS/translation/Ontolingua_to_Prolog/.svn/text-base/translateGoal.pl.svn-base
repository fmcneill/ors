% this is to translate the goal into a form readable by PDDL.  this means putting initial letters into upper case and putting a dash in where a capital comes in the middle of the word, and then altering the bracketing and the spacing.


translateGoal(Goal,TransGoal) :-
	name(Goal,GoalChars),
	name('and(',MultiIndicator),
	matchExpression([],MultiIndicator,Preds,GoalChars),
	translateAllPreds(Preds,[],TransPreds),
	reverse(TransPreds,RevPreds),
	name('(And ',MultiBegin),
	append(MultiBegin,RevPreds,TransChars),
	name(TransGoal,TransChars).

translateGoal(Goal,TransGoal) :-
	name(Goal,GoalChars),
	translateGoalChars(GoalChars,TransChars),
	name(TransGoal,TransChars).


translateAllPreds(Preds,TransPredsSoFar,TransPreds) :-
	name(',',Comma),
	matchExpression(FirstPred,Comma,RestPreds,Preds),
	translateGoalChars(FirstPred,TransFirst),
	reverse(TransFirst,RevFirst),
	name(' ',Space),
	append(Space,RevFirst,GoodFirst),
	append(GoodFirst,TransPredsSoFar,NewTransPreds),
	translateAllPreds(RestPreds,NewTransPreds,TransPreds).

translateAllPreds(LastPred,TransPredsSoFar,TransPreds) :-
	translateGoalChars(LastPred,TransLast),
	reverse(TransLast,RevLast),
	append(RevLast,TransPredsSoFar,TransPreds).
	

translateGoalChars(GoalChars,TransChars) :-
	name('(',StartArgs),
	matchExpression(Name,StartArgs,Args,GoalChars),
	translateArg(Name,TransName),
	name(' ',Space),
	append(TransName,Space,TransBegin),
	append(StartArgs,TransBegin,FullBegin),
	translateGoalChars(Args,TransArgs,[]),
	append(FullBegin,TransArgs,TransChars).

translateGoalChars(ArgsChars,TransArgs,TransSoFar) :-
	name(',',Comma),
	matchExpression(FirstArg,Comma,RestArgs,ArgsChars),
	translateArg(FirstArg,TransFirst),
	name(' ',Space),
	append(TransFirst,Space,FullFirst),
	append(TransSoFar,FullFirst,NewTrans),
	translateGoalChars(RestArgs,TransArgs,NewTrans).

translateGoalChars(LastArg,TransChars,TransSoFar) :-
	translateArg(LastArg,TransLast),
	append(TransSoFar,TransLast,TransChars).

translateArg([FirstArg|Rest],[FirstArg|TransRest]) :-
	FirstArg < 58,
	FirstArg > 47,
	translateRest(Rest,TransRest,[]).

translateArg([40|[SecondArg|Rest]],[40|[NewSecond|TransRest]]) :-
	NewSecond is SecondArg - 32,
	translateRest(Rest,TransRest,[]).

translateArg([FirstArg|Rest],[NewFirst|TransRest]) :-
	NewFirst is FirstArg - 32,
	translateRest(Rest,TransRest,[]).


translateRest([],TransRest,RevRest) :-
	reverse(TransRest,RevRest).

translateRest([H|T],TransRest,RestSoFar) :-
	H > 64,
	H < 91,
	name('-',Dash),
	append([H],Dash,NewH),
	append(NewH,RestSoFar,NewRest),
	translateRest(T,TransRest,NewRest).

translateRest([H|T],TransRest,RestSoFar) :-
	translateRest(T,TransRest,[H|RestSoFar]).

