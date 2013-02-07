-module(ex1).

%% API
-export([init/2,
         is_consistent/2,
         try_adjust/3,
         dependent_agents/2]).

-include("dcsp.hrl").

-compile([export_all]).

%% Some assumptions:
%%
%% Initializing the variable and then selecting a new proposition
%% when its value must be changed are (and must be) highly coupled.
%% I.e. the way we choose a proposition is dependent on both domain
%% representation and the way the initial value was chosen.
%%
%% In this particular case the domain is represented as a set of values:
%%
%%   1: {1,2}
%%   2: {2}
%%   3: {1,2,4,5}
%%
%% The initializing function takes the last element from the domain
%% for the initial value.
%%
%% The function returning new proposition takes consecutive values from
%% the end of the domain.
%%
%% For example: in case of domain 3, the intial value would
%% be 5 and the consecutive propositions 4, 2, 1.


%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec init(aid(), problem()) -> agent_view().
init(AId, #problem{} = P) ->
    Domain = lists:nth(AId, P#problem.domains),
    Last = element(size(Domain), Domain),
    [{{x,AId}, Last}].

-spec is_consistent(agent_view(), problem()) -> boolean().
is_consistent(AgentView, #problem{} = P) ->
    VarView = agent_view_to_var_view(AgentView),
    lists:all(mk_check_constraint(VarView), P#problem.constraints).

-spec try_adjust(aid(), agent_view(), problem())
    -> {ok, agent_view()} | false.
try_adjust(AId, AgentView, Problem) ->
    Current = proplists:get_value(AId, AgentView),
    Domain = lists:nth(AId, Problem#problem.domains),
    lists:foldl(mk_adjust_one(AId, AgentView, Problem), false,
                still_not_tried(Current, Domain)).

-spec dependent_agents(aid(), problem()) -> [aid()].
dependent_agents(AId, #problem{} = P) ->
    Concerning = [ {A,B} || {{x,A},_,{x,B}} <- P#problem.constraints,
                            A == AId orelse B == AId ],
    {L,R} = lists:unzip(Concerning),
    [ E || E <- L ++ R, E /= AId ].

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

mk_adjust_one(AId, AgentView, Problem) ->
    fun(_, {ok, NewAgentView}) ->
            {ok, NewAgentView};
       (NewCurrent, false) ->
            NewAgentView = lists:keyreplace(AId, 1, AgentView,
                                            {AId, NewCurrent}),
            case is_consistent(NewAgentView, Problem) of
                true ->
                    {ok, NewAgentView};
                false ->
                    false
            end
    end.

still_not_tried(Current, Domain) ->
    [ Val || Val <- lists:reverse(tuple_to_list(Domain)), Val < Current ].

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

problem() ->
    #problem{
        domains = [{1,2}, {2}, {1,2}, {1,2,4,5}],
        constraints = [{{x,1}, '/=', {x,3}},
                       {{x,2}, '/=', {x,3}}] }.

is_consistent_test_() ->
    ?LET(Problem, problem(),
         [?_test(?assert(is_consistent([], Problem))),
          ?_test(?assert(is_consistent([{1,1}, {3,2}], Problem))),
          ?_test(?assert(is_consistent([{2,1}, {3,2}], Problem))),
          ?_test(?assert(is_consistent([{1,1}, {2,2}], Problem))),
          ?_test(?assertNot(is_consistent([{1,1}, {3,1}], Problem))),
          ?_test(?assertNot(is_consistent([{2,1}, {3,1}], Problem)))]).

still_not_tried_test_() ->
    ?LET(#problem{domains = [D1, D2, _, D4]}, problem(),
         [?_test(?assertEqual([1], still_not_tried(2, D1))),
          ?_test(?assertEqual([], still_not_tried(2, D2))),
          ?_test(?assertEqual([4,2,1], still_not_tried(5, D4))),
          ?_test(?assertEqual([1], still_not_tried(2, D4)))]).

try_adjust_test_() ->
    ?LET({Problem, AV1, AV2, AV3, AV4},
         {problem(),
          [{1,2}, {2,2}],
          [{1,1}, {2,2}],
          [{1,2}, {3,2}],
          [{1,1}, {3,2}]},
         [?_test(?assertEqual(false, try_adjust(2, AV1, Problem))),
          ?_test(?assertEqual({ok, AV2}, try_adjust(1, AV1, Problem))),
          ?_test(?assertEqual({ok, AV4}, try_adjust(1, AV3, Problem)))]).

dependent_agents_test_() ->
    ?LET(Problem, problem(),
         [?_test(?assertEqual([3], dependent_agents(1, Problem))),
          ?_test(?assertEqual([3], dependent_agents(2, Problem))),
          ?_test(?assertEqual([1,2], dependent_agents(3, Problem)))]).

-endif.