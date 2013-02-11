-module(dcsp).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% User API
-export([start/0,
         solve/1]).

-include("dcsp.hrl").

%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    dcsp_sup:start_link().

stop(_State) ->
    ok.

%% -------------------------------------------------------------------
%% User API
%% -------------------------------------------------------------------

start() ->
    application:start(dcsp).

-spec solve(problem()) -> term() | no_solution.
solve(Problem) ->
    Self = self(),
    Ref = make_ref(),
    {ok, _Pid} = dcsp_solver:start(Problem, mk_result_handler(Self, Ref)),
    receive
        {result, Ref, Result} ->
            Result;
        {no_solution, Ref} ->
            no_solution
    end.

mk_result_handler(Self, Ref) ->
    fun({result, Result}) ->
            Self ! {result, Ref, Result};
       (no_solution) ->
            Self ! {no_solution, Ref}
    end.
