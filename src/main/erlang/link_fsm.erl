-module(link_fsm).
-behavior(gen_fsm).
-export([init/1, get_state/0, start_link/0, handle_sync_event/4, terminate/3]).
-export([disconnected/2, marginally_connected/2, solidly_connected/2]).

-record(state, {contacts=sets:new()}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
    gen_fsm:sync_send_all_state_event(?MODULE, get_state).

init(_Args) ->
    {ok, disconnected, #state{}}.

add_ref(Contact, State) ->
    Contacts = case sets:is_element(Contact, State#state.contacts) of
		   true -> throw({duplicate_contact, Contact});
		   false -> sets:add_element(Contact, State#state.contacts)
	       end,
    State#state{contacts=Contacts}.

drop_ref(Contact, State) ->
    Contacts = case sets:is_element(Contact, State#state.contacts) of
		   true -> sets:del_element(Contact, State#state.contacts);
		   false -> throw({contact_not_found, Contact})
	       end,
    State#state{contacts=Contacts}.

disconnected({connected, Contact}, State) ->
    error_logger:info_msg("Agent-Link: Connected~n"),
    gen_event:notify(agent_link_events, connected),
    {next_state, marginally_connected, add_ref(Contact, State)}.

marginally_connected({connected, Contact}, State) ->
    {next_state, solidly_connected, add_ref(Contact, State)};
marginally_connected({disconnected, _Reason, Contact}, State) ->
    NewState = drop_ref(Contact, State),
    case sets:size(NewState#state.contacts) of
	0 ->
	    error_logger:info_msg("Agent-Link: Disconnected~n"),
	    gen_event:notify(agent_link_events, disconnected),
	    {next_state, disconnected, NewState};
	Refs ->
	    throw({illegal_refs, Refs})
    end.

solidly_connected({connected, Contact}, State) ->
    {next_state, solidly_connected, add_ref(Contact, State)};
solidly_connected({disconnected, _Reason, Contact}, State) ->
    NewState = drop_ref(Contact, State),
    case sets:size(NewState#state.contacts) of
	0 ->
	    throw({illegal_refs, 0});
	1 -> 
	    {next_state, marginally_connected, NewState};
	_ ->
	    {next_state, solidly_connected, NewState}
    end.

handle_sync_event(get_state, _From, StateName, State) ->
    ConnectedStates = sets:from_list([solidly_connected, marginally_connected]),
    CurrentState = case sets:is_element(StateName, ConnectedStates) of
		       true -> connected;
		       false -> disconnected
		   end,
    {reply, {ok, CurrentState}, StateName, State}.

terminate(_Reason, _State, _Data) ->
    void.
