:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%

% Main start server predicate


% Create a program whih will start a LINDA Server
createServerProgram:-
    
    get_absolute_path('/agent_environment/server.pl', ServerAbsPath),
    tell(ServerAbsPath),
    get_absolute_path('/agent_environment/server.addr', ServerAddressAbsPath),
    write(':- use_module(library(\'linda/server\')),'),nl,
    write('   use_module(library(\'system\')),'),nl,
    write('   use_module(library(\'process\')),'),nl,
    write('   process_id(PId),'),nl,
    write('   linda((Host:Port)-(tell(\''),
    write(ServerAddressAbsPath),
    write('\'),'),nl,
    write('                      '),
    write('write(\''),write(\),write('\'\'),'),nl,
    write('                      '),
    write('write(Host),'),nl,
    write('                      '),
    write('write(\''),write(\),write('\':\'),'),nl,
    write('                      '),
    write('write(Port-PId),'),nl,
    write('                      '),
    write('write(\'.\'),'),nl,
    write('                      '),
    write('told)).'),
    told.


get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).

% String Concatenation
concat(ListStrings,Concat):-
    ListStrings = [Str1|Strings],
    concatList(Strings,Str1,Concat).
concatList([],String,String).
concatList([S|Ss],StringSoFar,String):-
    concat(StringSoFar,S,NewStringSoFar),
    concatList(Ss,NewStringSoFar,String).
concat(Str1,Str2,Str1andStr2):-
    name(Str1,ASCStr1),
    name(Str2,ASCStr2),
    append(ASCStr1,ASCStr2,ASCStr1andStr2),
    name(Str1andStr2,ASCStr1andStr2).



:- createServerProgram.
