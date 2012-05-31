:- use_module(library('linda/server')),
   use_module(library('system')),
   use_module(library('process')).
  


start_server :-
   process_id(PId),
   get_absolute_path('/agent_environment/server.addr', ServerAddressAbsPath),
   linda((Host:Port)-(tell(ServerAddressAbsPath),write('\''),write(Host),write('\':'),write(Port-PId),write('.'),
                      told)).



get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).


:-  start_server.
