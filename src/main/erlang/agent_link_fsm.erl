-module(agent_link_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/1, handle_info/3, terminate/3]).
-export([firstconnect/2, reconnecting/2]).

-record(state, {contact, initial_timeout=500, timeout=5000}).

start_link(Contact, InitialTimeout, Timeout) ->
    gen_fsm:start_link(?MODULE, [Contact, InitialTimeout, Timeout], []).

init([Contact, InitialTimeout, Timeout]) ->
    InitialState = #state{contact=Contact,
			  initial_timeout=InitialTimeout,
			  timeout=Timeout},
    {ok, initialize, InitialState, 0}.

initialize(timeout, State) ->
    net_kernel:monitor_nodes(true),
    Nodes = sets:from_list(erlang:nodes()),
    case sets:is_element(State#state.contact, Nodes) of
	true ->
	    connected(State);
	false ->    
	    connect(State, firstconnect, State#state.initial_timeout)
    end

connect(State) ->
    connect(State, reconnecting, State#state.timeout).

connect(State, FailState, Timeout) ->
    net_adm:ping(State#state.contact),
    {next_state, FailState, State, Timeout};

firstconnect(timeout, State) ->
    connect(State, firstconnect, State#state.initial_timeout).

reconnecting(timeout, State) ->
    connect(State).

handle_info({nodeup, Node}, StateName, State) when StateName =/= connected ->
    connected(State);
handle_info({nodedown, Node}, connected, State) ->
    disconnected(State).

connected(State) ->
    error_logger:info_msg("Connected~n"),
    {next_state, connected, State, hibernate};    

disconnected(State) ->
    error_logger:info_msg("Disconnected~n"),
    connect(State);    

terminate(_Reason, _State, _Data) ->
    error_logger:info_msg("Terminating~n"),
    void.
    
	    

 		  
