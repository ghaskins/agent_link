-module(contact_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/1, start_link/3, handle_info/3, terminate/3]).
-export([initialize/2, firstconnect/2, reconnecting/2]).

-record(state, {contact, initial_timeout, timeout}).

start_link(Contact) ->
    start_link(Contact, 500, 5000).

start_link(Contact, InitialTimeout, Timeout) ->
    gen_fsm:start_link(?MODULE, [Contact, InitialTimeout, Timeout], []).

init([Contact, InitialTimeout, Timeout]) ->
    InitialState = #state{contact=Contact,
			  initial_timeout=InitialTimeout,
			  timeout=Timeout},
    {ok, initialize, InitialState, 0}.

initialize(timeout, State) ->
    ok = net_kernel:monitor_nodes(true),
    Nodes = sets:from_list(erlang:nodes()),
    case sets:is_element(State#state.contact, Nodes) of
	true ->
	    connected(State);
	false ->
	    connect(State, firstconnect, State#state.initial_timeout)
    end.

connect(State) ->
    connect(State, reconnecting, State#state.timeout).

connect(State, FailState, Timeout) ->
    net_adm:ping(State#state.contact),
    {next_state, FailState, State, Timeout}.

firstconnect(timeout, State) ->
    connect(State, firstconnect, State#state.initial_timeout).

reconnecting(timeout, State) ->
    connect(State).

handle_info({nodeup, Node}, StateName, State)
  when StateName =/= connected, Node =:= State#state.contact ->
    connected(State);
handle_info({nodedown, Node}, connected, State)
  when Node =:= State#state.contact ->
    disconnected(State);
handle_info(_, StateName, State) ->
    {next_state, StateName, State}.

connected(State) ->
    gen_fsm:send_event(link_fsm, {connected, State#state.contact}),
    {next_state, connected, State, hibernate}.

disconnected(State) ->
    gen_fsm:send_event(link_fsm, {disconnected, State#state.contact}),
    connect(State).

terminate(_Reason, _State, _Data) ->
    void.
