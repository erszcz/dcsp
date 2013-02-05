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
    dcsp_solver:start(Problem).
