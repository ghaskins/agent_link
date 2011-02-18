%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(agent_link_sup).
-behaviour(supervisor).

-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

start_link() ->
    supervisor:start_link(?MODULE, []).

timeout() -> 5000.

init([]) ->
    {ok,{{one_for_all,0,1},
	 [{'agent-link-fsm',
	   {link_fsm,start_link,[]},
	   permanent, timeout(), worker,[link_fsm]},
	  {'agent-link-events',
	   {gen_event, start_link, [{local, agent_link_events}]},
	   permanent, timeout(), worker, dynamic}
	 ]
	}
    }.


