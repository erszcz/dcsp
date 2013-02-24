-module(dcsp_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [{group, main}].

groups() ->
    [{main, [{repeat, 50}], [check_result]}].

%% ------------------------------------------------------------------
%% Setup / Teardown
%% ------------------------------------------------------------------

init_per_suite(Config) ->
    dcsp:start(),
    Problem = dcsp_problem:from_file("../../nqueens5.problem"),
    [{problem, Problem} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ------------------------------------------------------------------
%% Test cases starts here.
%% ------------------------------------------------------------------

check_result(Config) ->
    Problem = ?config(problem, Config),
    ?assert(timeout /= dcsp:solve(Problem)).
