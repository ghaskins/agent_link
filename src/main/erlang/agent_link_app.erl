%% Author: ghaskins
-module(agent_link_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->

    Contacts = [ list_to_atom(Contact) ||
		   Contact <- case application:get_env(contacts) of
				  undefined -> [];
				  {ok, Val} -> Val
			      end
	       ],

    agent_link_sup:start_link(Contacts).

stop(_State) ->
    ok.



