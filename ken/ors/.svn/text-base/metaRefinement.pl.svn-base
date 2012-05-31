:- use_module(library(lists)),use_module(library(random)).


% refineMeta(+Type,+Info)
% similar to refine/2 but performs refinements on the meta files.  reads in the meta file, performs the refinement, writes to 'metaOnt.out' and then copies this new file onto the original meta file.


refineMeta(Type,Info) :-
	see('metaOnt.in'),tell('metaOnt.out'),
	performMetaRef(Type,Info),
	told,seen,
	copyMetaFiles.


addPredClass(PredClass,PredSuperClass) :-
	see('metaOnt.in'),tell('metaOnt.out'),
	copyOnt,
	addClasses([[PredClass,PredSuperClass]]),
	seen,told,
	copyMetaFiles.


performMetaRef(Type,Info) :-
	readLineRef(Line),
	(   extractLast(Line,end,Rest),nl,
	    % if this is the last line then process the rest of the line
	    checkMetaLine(Type,Info,Rest)
	;
	    % else process the whole line and keep going
	    checkMetaLine(Type,Info,Line),
	    performMetaRef(Type,Info)
	).


checkMetaLine(Type,Info,Line) :-
	(   processMetaLine(Type,Info,Line)
	    % if this is the line to process, then process it
	;   
	    copyLine(Line)
	    % else just copy the line into the new ontology
	).




copyMetaFiles :-
	see('metaOnt.out'),tell('metaOnt.in'),
	copyFile,
	told,seen.


processMetaLine(agent,TransAgent,Line) :-
	% determine whether we have the right line:
	name('(Define-Frame ',DefFrame),
	matchExpression([],DefFrame,AxNameAndDef,Line),
	name(TransAgent,AgentChars),
	matchExpression([],AgentChars,_RestOfDef,AxNameAndDef),
	% now annotate this line with a star so that it is ignored during the translation process
	name('*',Star),
	append(Star,Line,NewLineChars),
	name(NewLine,NewLineChars),
	write(NewLine).
	

% addClasses(+ListofClassesAndSuperClasses)
% creates an entry for a new class.  since order is not important in the ontology, this is added in the next available space.  

%addClasses([]).

%addClasses([[FirstClass,FirstClassSuperClass]|Others]) :-
%	% first we build up the line declaring the beginning of the class entry
%	nl,
%	name(';;; ',Comment),
%	name(FirstClass,ClassChars),
%	append(Comment,ClassChars,CommentLine),
%	name(CommentLineName,CommentLine),
%	write(CommentLineName),
%	nl,nl,
%	% next we build up the actual class line
%	name('(Define-Class ',DefClass),
%	name(' (?X) :Def (And (',DefMiddle),
%	% if FirstClassSuperClass is uninstantiated or if it is instantiated to 'Thing', we add that name at the end
%	(   FirstClassSuperClass = 'Thing',
%	    name('Thing (?X)))',DefEnd)
%	;
%	    % otherwise, we add the name of the superclass
%	    name(FirstClassSuperClass,SuperClass),
%	    name(' (?X)))',RestOfDefEnd),
%	    append(SuperClass,RestOfDefEnd,DefEnd)
%	),
%	append(DefMiddle,DefEnd,SomeDef),
%	append(ClassChars,SomeDef,MostDef),
%	append(DefClass,MostDef,FullDef),
%	name(FullDefName,FullDef),
%	write(FullDefName),nl,
%	addClasses(Others).