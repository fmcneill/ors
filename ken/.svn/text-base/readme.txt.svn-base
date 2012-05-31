README
written by Ken Hu

Experience Sharing with the Ontology Repair System
implementedd by Ken Hu and Fiona McNeill



System Requirement:
SICSTUS Prolog

Note: it would be much easier if you can run it on Informatics' DICE instead of your home machine
Note: the commands documented are for Linux systems

Steps for Running:
Copy and paste the ors folder onto your personal folder
Open command console and change the current director to the ors folder

For experiemental system --
Depending on the test case you are interested in reproducing run one of the following command:
	1, 4, 7: cp agents/stored-exp/expDb.varA.txt agents/expDb.pl
	2, 5, 8: cp agents/stored-exp/expDb.varB.txt agents/expDb.pl
	3, 6, 9: cp agents/stored-exp/expDb.varAB.txt agents/expDb.pl
Finally, execute one of the following command base on the test cases you are interested in:
	1 ~ 3: sh run_files/ken-shoppingRun1.sh
	2 ~ 6: sh run_files/ken-shoppingRun2.sh
	7 ~ 9: sh run_files/ken-shoppingRun3.sh

For control system --
Open the file expClient.pl
comment out the following two predicates:
	shareExps([[SPA,Action,SuprisingQuestions,QueryList,OldOnt,NewOnt,Repair]|Rest]) :-
		write('Im Sharing a refinement Experience regarding agent: '),write(SPA),nl,
		write('executing action: '),write(Action),nl,nl,
		RequestInfo = [SPA,Action,SuprisingQuestions,QueryList,OldOnt,NewOnt,Repair,success],
		out(request(lucas,expServerAgent,store,RequestInfo)),
		shareExps(Rest),!.
and
	examineAction(Action,Just,CorrectAgent,QueryList,SurprisingQuestions,RefinementList,[Refinements|RefinementList]) :-
		hasSavedExps(CorrectAgent,Action,_),
		write('Im requesting for relevant experiences ...'),nl,nl,
		requestExps(CorrectAgent,Action,SurprisingQuestions,QueryList,Experiences),
		write('Im Attempting to consume experiences ...'),nl,nl,
		consumeExperiences(CorrectAgent,Action,Experiences,Refinements),
		Refinements \= [],
		saveExps(CorrectAgent,Action,Experiences),!.
Finally, execute one of the following command base on the variations you are interested in:
	A: sh run_files/ken-shoppingRun1.sh
	B: sh run_files/ken-shoppingRun2.sh
	C: sh run_files/ken-shoppingRun3.sh

Once all the windows are loaded, execute the following commands on the main (blue) window:
plan.
sh.
The planning agent will start executing the goal and repair the mismatches now.