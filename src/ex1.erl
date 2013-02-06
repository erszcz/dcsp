-module(ex1).

%% API
-export([init/2,
         is_consistent/2]).

-include("dcsp.hrl").

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

init(AId, #problem{} = P) ->
    Domain = lists:nth(AId, P#problem.domains),
    Last = element(size(Domain), Domain),
    [{{x,AId}, Last}].

is_consistent(AgentView, #problem{} = P) ->
    VarView = agent_view_to_var_view(AgentView),
    lists:all(mk_check_constraint(VarView), P#problem.constraints).

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

agent_view_to_var_view(AgentView) ->
    [ {{x,AId},Val} || {AId,Val} <- AgentView ].

mk_check_constraint(VarView) ->
    fun({Var1, Op, Var2}) ->
            try
                {Var1,Val1} = proplists:lookup(Var1, VarView),
                {Var2,Val2} = proplists:lookup(Var2, VarView),
                apply(fun erlang:Op/2, [Val1, Val2])
            catch
                error:{badmatch, none} ->
                    true
            end
    end.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_consistent_test_() ->
    {setup,
     fun () ->
        #problem{constraints = [
            {{x,1}, '/=', {x,3}},
            {{x,2}, '/=', {x,3}}
        ]}
     end,
     fun(Problem) -> [
        ?_test(?assert(is_consistent([], Problem))),
        ?_test(?assert(is_consistent([{1,1}, {3,2}], Problem))),
        ?_test(?assert(is_consistent([{2,1}, {3,2}], Problem))),
        ?_test(?assert(is_consistent([{1,1}, {2,2}], Problem))),
        ?_test(?assertNot(is_consistent([{1,1}, {3,1}], Problem))),
        ?_test(?assertNot(is_consistent([{2,1}, {3,1}], Problem)))
     ] end}.

-endif.
