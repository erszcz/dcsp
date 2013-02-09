-module(dcsp_agent).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
         initial/2,
         initial/3,
         step/2,
         step/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("dcsp.hrl").

-record(state, {id :: integer(),
                module :: atom(),
                problem :: problem(),
                agent_view = [] :: agent_view(),
                solver :: pid(),
                others = [] :: [{pos_integer(), pid()}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, Problem, Solver) ->
    gen_fsm:start_link(?MODULE, [Id, Problem, Solver], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([AId, Problem, Solver]) ->
    Mod = Problem#problem.module,
    AgentView = Mod:init(AId, Problem),
    S = #state{id = AId,
               module = Mod,
               problem = Problem,
               agent_view = AgentView,
               solver = Solver},
    error_logger:info_msg("Agent ~p initial state:~n~p~n", [AId, S]),
    {ok, initial, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

%% Wait for info {go, _} signalling that the simulation may start.
initial(_Event, State) ->
    {next_state, step, State}.

step(_Event, State) ->
    {next_state, step, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

initial(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

step(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({go, AgentIds}, initial, State) ->
    Others = [ {AId, Agent} || {AId, Agent} <- AgentIds, Agent /= self() ],
    error_logger:info_msg("Others: ~p~n", [Others]),
    NewState = check_agent_view(State#state{others = Others}),
    {next_state, step, NewState};
handle_info({is_ok, {AId, Val}}, step,
            #state{agent_view = AgentView} = S) ->
    NewAgentView = lists:keystore(AId, 1, AgentView, {AId, Val}),
    NS = check_agent_view(S#state{agent_view = NewAgentView}),
    {next_state, step, NS};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_agent_view(State) ->
    case is_consistent(State) of
        true ->
            State;
        false ->
            adjust_or_backtrack(State)
    end.

is_consistent(#state{module = Mod, agent_view = AgentView,
                     problem = Problem}) ->
    Mod:is_consistent(AgentView, Problem).

adjust_or_backtrack(#state{id = AId} = State) ->
    case try_adjust(State) of
        {ok, NewAgentView} ->
            NewState = State#state{agent_view = NewAgentView},
            send_is_ok(AId, NewAgentView, NewState),
            NewState;
        false ->
            backtrack(State)
    end.

try_adjust(#state{id = AId, agent_view = AgentView,
                  module = Mod, problem = Problem}) ->
    Mod:try_adjust(AId, AgentView, Problem).

send_is_ok(AId, AgentView, State) ->
    AgentVal = {AId, proplists:get_value(AId, AgentView)},
    [aid_to_pid(Other, State) ! {is_ok, AgentVal}
     || Other <- get_outgoing_links(State)].

get_outgoing_links(#state{id = AId, module = Mod, problem = P}) ->
    Mod:dependent_agents(AId, P).

backtrack(State) ->
    Nogoods = get_nogoods(State),
    case contains_empty_nogood(Nogoods) of
        true ->
            no_solution(State),
            State;
        false ->
            ok
    end.

get_nogoods(#state{id = AId, agent_view = AgentView,
                   module = Mod, problem = P}) ->
    Mod:nogoods(AId, AgentView, P).

contains_empty_nogood(Nogoods) ->
    lists:any(fun([]) -> true; (_) -> false end, Nogoods).

no_solution(#state{solver = Solver}) ->
    Solver ! no_solution.

aid_to_pid(AId, #state{others = Others}) ->
    proplists:get_value(AId, Others).
