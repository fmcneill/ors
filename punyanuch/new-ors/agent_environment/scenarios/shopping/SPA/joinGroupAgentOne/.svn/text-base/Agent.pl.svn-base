

% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).


specificServiceProvidingAgent :-

        assert(scenario_name(shopping)),  % =====> These two are the only lines that differentiate between the 
        assert(spa_name(joinGroupAgentOne)),  % =====> specific spas of different scenarios.

        set_prolog_flag(single_var_warnings, off),
        set_prolog_flag(discontiguous_warnings, off),
        scenario_name(Sce),
        get_absolute_path('/agent_environment/scenarios/', ScenPath),
        atom_concat(ScenPath, Sce, ScPath),
        atom_concat(ScPath, '/SPA/', ScPath2),
        spa_name(SpaName),
        atom_concat(ScPath2, SpaName, ScPath3),
        atom_concat(ScPath3, '/', ScPath4),
        atom_concat(ScPath4, 'AgentThy', ThyPath),
	atom_concat(ScPath4, 'AgentSig', SigPath),
	get_absolute_path('/agent_environment/SPA/spa', SpaAbsolPath),
	reconsult(ThyPath),
	reconsult(SigPath),
	reconsult(SpaAbsolPath),
	spa(SpaName).


% get_absolute_path(+RelativePath, ?AbsolutePath)
% e.g. get_absolute_path('/agent_environment').

get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).



% this is to connect the spa to the server:


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client

initialiseClient(PId):-
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

:- initialiseClient(_),specificServiceProvidingAgent.
