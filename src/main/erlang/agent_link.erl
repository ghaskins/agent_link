-module(agent_link).

-export([add_contact/1, remove_contact/1, get_state/0]).

add_contact(Contact) ->
    Spec = {Contact,
	    {contact_fsm, start_link, [Contact]},
	    transient,
	    brutal_kill,
	    worker,
	    [contact_fsm]},

    supervisor:start_child(agent_link_sup, Spec).

remove_contact(Contact) ->
    ok = supervisor:terminate_child(agent_link_sup, Contact),
    supervisor:delete_child(agent_link_sup, Contact).

get_state() ->
    link_fsm:get_state().

    
