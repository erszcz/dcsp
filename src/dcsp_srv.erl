-module(dcsp_srv).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get_solver_id/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {free_id = 0}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_solver_id(Pid) ->
    gen_server:call(Pid, get_solver_id).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

init(_Args) ->
    error_logger:info_msg("~s ~p started~n", [?SERVER, self()]),
    {ok, #state{}}.

handle_call(get_solver_id, _From, #state{free_id = Id} = State) ->
    {reply, Id, State#state{free_id = Id + 1}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("~s ~p terminated~n", [?SERVER, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

