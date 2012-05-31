

% Load Linda Client Modules
:- use_module(library(system)),use_module(library(lists)), 
   use_module(library(random)),use_module(library('linda/client')),
   use_module(library(process)),use_module(library(file_systems)).


specificServiceProvidingAgent :-

        assert(scenario_name(water2)),  % =====> These two are the only lines that differentiate between the 
        assert(spa_name(plantOne)),  % =====> specific spas of different scenarios.
	assert(ontoType(onto)),
	set_prolog_flag(single_var_warnings, off),
        set_prolog_flag(discontiguous_warnings, off),
        spa_name(SpaName),
	get_absolute_path('/agent_environment/SPA/spa', SpaAbsolPath),
	reconsult(SpaAbsolPath),
	spa1(SpaName).




% get_absolute_path(+RelativePath, ?AbsolutePath)
% e.g. get_absolute_path('/agent_environment').

get_absolute_path(RelativePath, AbsolutePath) :- 
    environ('ORS_HOME', ORSHomePath), 
    atom_concat(ORSHomePath, RelativePath, AbsolutePath).





% this is to connect the spa to the server:


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START LINDA CLIENT %%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialise LINDA client
initialiseClient(PId):-
    get_absolute_path('/agent_environment/server.addr', ServerAddrAbsPath),
    see(ServerAddrAbsPath),
    read(Host:Port-PId),
    seen,
    linda_client(Host:Port).



%%%%%%%%%%%%%%%%%%%%%% START SERVER AND CLIENT

:- initialiseClient(_),specificServiceProvidingAgent.
