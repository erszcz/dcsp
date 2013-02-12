-module(dcsp_solver).

-behaviour(gen_server).

%% API
-export([start/1, start/2,
         start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Private
-export([delete_spec/1]).

-export([get_agents/1]).

-include("dcsp.hrl").

-record(state, {id :: atom(),
                agents = [] :: [{pos_integer(), pid()}],
                result_handler :: fun() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start(Problem) ->
    start(Problem, undefined).

start(Problem, ResultHandler) ->
    Id = get_id(),
    supervisor:start_child(dcsp_sup,
                           solver_spec(Id, [Id, Problem, ResultHandler])).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Id, Problem, ResultHandler]) ->
    error_logger:info_msg("~p ~p started. Problem: ~p~n",
                          [Id, self(), Problem]),
    OksAgentsIds = [ {dcsp_agent:start_link(AId, Problem, self()), AId}
                     || AId <- lists:seq(1, Problem#problem.num_agents) ],
    AgentIds = [ {AId, Agent} || {{ok, Agent}, AId} <- OksAgentsIds ],
    [ Agent ! {go, AgentIds} || {_, Agent} <- AgentIds ],
    {ok, #state{id = Id, agents = AgentIds,
                result_handler = ResultHandler}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_agents, _From, State) ->
    {reply, State#state.agents, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({result, Result}, S) ->
    [ dcsp_agent:stop(Agent) || {_,Agent} <- S#state.agents ],
    error_logger:info_msg("~p result: ~p~n", [S#state.id, Result]),
    case S#state.result_handler of
        undefined ->
            ok;
        {Mod,Fun} ->
            Mod:Fun({result, Result});
        Fun ->
            Fun({result, Result})
    end,
    {stop, normal, S};
handle_info(no_solution, S) ->
    [ dcsp_agent:stop(Agent) || {_,Agent} <- S#state.agents ],
    error_logger:info_msg("~p: no solution~n", [S#state.id]),
    case S#state.result_handler of
        undefined ->
            ok;
        {Mod,Fun} ->
            Mod:Fun(no_solution);
        Fun ->
            Fun(no_solution)
    end,
    {stop, normal, S};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{id = Id} = S) ->
    [ catch exit(Pid, normal) || {_,Pid} <- S#state.agents ],
    timer:apply_after(1000, ?MODULE, delete_spec, [Id]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

solver_spec(SolverId, Args) ->
    {SolverId,
     {?MODULE, start_link, [Args]},
     transient, 5000, worker, [?MODULE]}.

get_id() ->
    list_to_atom("solver"
                 ++ integer_to_list(dcsp_srv:get_solver_id(dcsp_srv))).

delete_spec(Id) ->
    error_logger:info_msg("Removing ~p spec: ~p~n",
                          [Id, supervisor:delete_child(dcsp_sup, Id)]).

get_agents(Pid) ->
    gen_server:call(Pid, get_agents).
