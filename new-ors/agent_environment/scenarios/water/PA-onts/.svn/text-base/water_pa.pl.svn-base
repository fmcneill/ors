

% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).


start :-
    get_absolute_path('/agent_environment/PA/planningAgent', PAAbsolutePath),
    reconsult(PAAbsolutePath),
    plan(onto,water).


% get_absolute_path(+RelativePath, ?AbsolutePath)
% e.g. get_absolute_path('/agent_environment').

get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).


% this is to connect the pa to the server:



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client

initialiseClient(PId):-
	sleep(5),
	get_absolute_path('/agent_environment/server.addr', ServerAddressAbsPath),
	find_host_port(ServerAddressAbsPath,Host,Port,PId),
	linda_client(Host:Port).


find_host_port(ServerAddressAbsPath,Host,Port,PId) :-
	file_exists(ServerAddressAbsPath),  
	see(ServerAddressAbsPath),
	read(Host:Port-PId),
	seen.

find_host_port(ServerAddressAbsPath,Host,Port,PId) :-
	sleep(1),
	find_host_port(ServerAddressAbsPath,Host,Port,PId).






%%%%%%%%%%%%%%%%%%%%%% START CLIENT


:- initialiseClient(_).
