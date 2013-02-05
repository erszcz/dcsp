-module(dcsp_srv).
-behaviour(gen_server).

%% API
-export([start_link/0,
         poke/1,
         get_pokes/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pokes = 0}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

poke(Pid) ->
    gen_server:cast(Pid, poke).

get_pokes(Pid) ->
    gen_server:call(Pid, get_pokes).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

init(_Args) ->
    error_logger:info_msg("~s ~p started~n", [?SERVER, self()]),
    {ok, #state{}}.

handle_call(get_pokes, _From, #state{pokes = Pokes} = State) ->
    {reply, Pokes, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(poke, #state{pokes = Pokes} = State) ->
    {noreply, State#state{pokes = Pokes + 1}};
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

