%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(agent_link_sup).
-behaviour(supervisor).

-export([start_link/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

start_link(Contacts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Contacts]).

timeout() -> 5000.

init([Contacts]) ->
    F = fun(Contact) ->
		{Contact,
		 {contact_fsm, start_link, [Contact]},
		 transient,
		 brutal_kill,
		 worker,
		 [contact_fsm]}
	end,

    {ok,{{one_for_all,0,1},
	 [{'agent-link-fsm',
	   {link_fsm,start_link,[]},
	   permanent, timeout(), worker,[link_fsm]},
	  {'agent-link-events',
	   {gen_event, start_link, [{local, agent_link_events}]},
	   permanent, timeout(), worker, dynamic}
	 ] ++ [ F(Contact) || Contact <- Contacts ]
	}
    }.


