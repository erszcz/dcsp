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
    ResultHandler = fun(Result) ->
        Self ! {result, Ref, Result}
    end,
    {ok, Pid} = dcsp_solver:start(Problem, ResultHandler),
    receive
        {result, Ref, Result} ->
            Result
        after 5000 ->
            exit(Pid, normal),
            no_solution
    end.
