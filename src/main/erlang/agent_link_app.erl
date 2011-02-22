%% Author: ghaskins
-module(agent_link_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    % FIXME
    StartArgs = [{contacts, ["contact1@linux-mp", "contact2@linux-mp"]}],

    Contacts = [ list_to_atom(Contact) ||
		   Contact <- proplists:get_value(contacts, StartArgs, [])
	       ],

    agent_link_sup:start_link(Contacts).

stop(_State) ->
    ok.



