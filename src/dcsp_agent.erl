-module(dcsp_agent).

-behaviour(gen_fsm).

%% API
-export([start_link/3,
         stop/1]).

%% gen_fsm callbacks
-export([init/1,
         initial/2, initial/3,
         step/2, step/3,
         done/2, done/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-include("dcsp.hrl").

-define(DONE_TIMEOUT, 1000).
-define(SINGLE_LINE_LOG, true).

-record(state, {id :: integer(),
                module :: atom(),
                problem :: problem(),
                agent_view = [] :: agent_view(),
                solver :: pid(),
                others = [] :: [{pos_integer(), pid()}],
                nogoods = sets:new() :: set(agent_view()),
                last_nogood :: agent_view()}).

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

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

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
    log("initial pos: ~p~n", [hd(AgentView)], S),
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

step(timeout, State) ->
    maybe_send_done(State),
    {next_state, done, State, ?DONE_TIMEOUT};
step(Event, S) ->
    log_unexpected(event, Event, step, S),
    {next_state, step, S}.

done(timeout, State) ->
    maybe_send_done(State),
    {next_state, done, State, ?DONE_TIMEOUT};
done(Event, S) ->
    log_unexpected(event, Event, done, S),
    {next_state, step, S, ?DONE_TIMEOUT}.

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

initial(Event, _From, State) ->
    log_unexpected(event, Event, initial, State),
    Reply = ok,
    {reply, Reply, state_name, State}.

step(Event, _From, State) ->
    log_unexpected(event, Event, step, State),
    Reply = ok,
    {reply, Reply, state_name, State}.

done(Event, _From, State) ->
    log_unexpected(event, Event, done, State),
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
handle_event(stop, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    log_unexpected("all state event", Event, StateName, State),
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
handle_sync_event(Event, _From, StateName, State) ->
    log_unexpected("sync all state event", Event, StateName, State),
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
handle_info({go, AgentIds}, initial, S) ->
    Others = [ {AId, Agent} || {AId, Agent} <- AgentIds, Agent /= self() ],
    NS = S#state{others = Others},
    send_is_ok(NS#state.id, NS#state.agent_view, NS),
    {next_state, step, NS, ?DONE_TIMEOUT};

handle_info({is_ok, Ref, {AId, Val}}, step,
            #state{agent_view = AgentView} = S) ->
    log("<< {is_ok, ~p, {~p,~p}}~n", [Ref, AId, Val], S),
    NewAgentView = lists:sort(lists:keystore(AId, 1, AgentView, {AId, Val})),
    NS = check_agent_view(S#state{agent_view = NewAgentView}),
    {next_state, step, NS, ?DONE_TIMEOUT};
handle_info({nogood, Ref, SenderAId, Nogood}, step,
            #state{agent_view = AgentView, nogoods = Nogoods} = S) ->
    NewAgentView = lists:ukeymerge(1, AgentView, Nogood),
    NewNogoods = sets:add_element(Nogood, Nogoods),
    log("<< {nogood, ~p, ~p, ~w}.~n  Nogoods: ~p~n",
        [Ref, SenderAId, Nogood, sets:to_list(NewNogoods)], S),
    AId = S#state.id,
    OldValue = proplists:get_value(AId, NewAgentView),
    NS = check_agent_view(S#state{agent_view = NewAgentView,
                                  nogoods = NewNogoods}),
    NewValue = proplists:get_value(AId, NS#state.agent_view),
    case OldValue == NewValue of
        true ->
            send_one_is_ok(SenderAId, {AId, NewValue}, NS);
        false ->
            ok
    end,
    {next_state, step, NS, ?DONE_TIMEOUT};
handle_info({done, _} = Done, step, State) ->
    handle_done(Done, State),
    {next_state, done, State, ?DONE_TIMEOUT};

handle_info({done, _} = Done, done, State) ->
    handle_done(Done, State),
    {next_state, done, State, ?DONE_TIMEOUT};
handle_info({is_ok, _, _} = Event, done, State) ->
    self() ! Event,
    {next_state, step, State};
handle_info({nogood, _, _, _} = Event, done, State) ->
    self() ! Event,
    {next_state, step, State};

handle_info(Info, StateName, State) ->
    log_unexpected(info, Info, StateName, State),
    {next_state, StateName, State}.

handle_done({done, ResultAgentView},
            #state{agent_view = AgentView, id = AId,
                   module = Mod, problem = P} = S) ->
    Merged = lists:ukeymerge(1, AgentView, ResultAgentView),
    case {Mod:is_consistent(AId, Merged, P),
          AId == 1}
    of
        {true, true} ->
            S#state.solver ! {result, Merged};
        {true, _} ->
            aid_to_pid(AId - 1, S) ! {done, Merged};
        {_, _} ->
            log("inconsistent merge result:~n~p~n", [Merged], S)
    end.

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
            log("inconsistent.~n", [], State),
            adjust_or_backtrack(State)
    end.

is_consistent(#state{module = Mod, agent_view = AgentView,
                     problem = Problem, id = AId, nogoods = Nogoods} = S) ->
    log("agent view: ~p~n", [AgentView], S),
    not sets:is_element(AgentView, Nogoods)
    andalso
    Mod:is_consistent(AId, AgentView, Problem).

adjust_or_backtrack(#state{id = AId, agent_view = AgentView} = S) ->
    case try_adjust(S) of
        {ok, NewAgentView} ->
            NS = S#state{agent_view = NewAgentView},
            send_is_ok(AId, NewAgentView, NS),
            log("adjusted.~n"
                "  Old agent view: ~p~n"
                "  New agent view: ~p~n",
                [AgentView, NewAgentView], NS),
            NS;
        false ->
            Nogood = lists:keydelete(AId, 1, AgentView),
            NewNogoods = sets:add_element(Nogood, S#state.nogoods),
            log("nogood: ~p.~n"
                "  Nogoods: ~p~n",
                [Nogood, sets:to_list(NewNogoods)], S),
            backtrack(S#state{nogoods = NewNogoods,
                              last_nogood = Nogood})
    end.

try_adjust(#state{id = AId, agent_view = AgentView,
                  module = Mod, problem = Problem,
                  nogoods = Nogoods}) ->
    Mod:try_adjust(AId, AgentView, Nogoods, Problem).

send_is_ok(AId, AgentView, State) ->
    AgentVal = {AId, proplists:get_value(AId, AgentView)},
    [send_one_is_ok(Other, AgentVal, State)
     || Other <- get_outgoing_links(State)].

send_one_is_ok(To, Val, State) ->
    Ref = make_ref(),
    log("~p ! {is_ok, ~p, ~p}~n", [To, Ref, Val], State),
    aid_to_pid(To, State) ! {is_ok, Ref, Val}.

get_outgoing_links(#state{id = AId, module = Mod, problem = P}) ->
    Mod:dependent_agents(AId, P).

backtrack(#state{nogoods = Nogoods} = State) ->
    case contains_empty_nogood(Nogoods) of
        true ->
            no_solution(State),
            State;
        false ->
            check_agent_view(send_nogoods(State))
    end.

contains_empty_nogood(Nogoods) ->
    sets:is_element([], Nogoods).

no_solution(#state{solver = Solver}) ->
    Solver ! no_solution.

send_nogoods(#state{last_nogood = Nogood} = S) ->
    AId = get_min_priority_agent(Nogood, S),
    Ref = make_ref(),
    log("~p ! {nogood, ~p, ~p, ~w}~n", [AId, Ref, S#state.id, Nogood], S),
    aid_to_pid(AId, S) ! {nogood, Ref, S#state.id, Nogood},
    NewAgentView = lists:keydelete(AId, 1, S#state.agent_view),
    S#state{agent_view = NewAgentView}.

get_min_priority_agent(AgentView, S) ->
    lists:max([AId || {AId, _} <- AgentView, AId /= S#state.id]).

maybe_send_done(#state{id = AId, agent_view = AgentView, problem = P} = S) ->
    case AId > 0 andalso AId == P#problem.num_agents of
        true ->
            log("~p ! {done, ~p}~n", [AId-1, AgentView], S),
            aid_to_pid(AId-1, S) ! {done, AgentView};
        false ->
            ok
    end.

aid_to_pid(AId, #state{others = Others}) ->
    proplists:get_value(AId, Others).

log_unexpected(What, Event, StateName, S) ->
    log("unexpected ~s in '~p' state: ~p~n",
        [What, StateName, Event], S).

log(Format, Args, #state{id = AId}) ->
    NewFormat = case ?SINGLE_LINE_LOG of
        true ->
            Opts = [global, {return, list}],
            NF = "[~b] " ++ Format,
            NNF = re:replace(NF, "~p", "~w", Opts),
            re:replace(NNF, "(~n)+", " ", Opts) ++ "~n";
        false ->
            "[~b] " ++ Format
    end,
    NewArgs = [AId] ++ Args,
    Msg = io_lib:format(NewFormat, NewArgs),
    file:write_file("dcsp.log", Msg, [append]),
    AgentLog = lists:flatten(io_lib:format("dcsp-~b.log", [AId])),
    file:write_file(AgentLog, Msg, [append]).
