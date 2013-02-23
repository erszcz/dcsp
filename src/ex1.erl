-module(ex1).

%% API
-export([init/2,
         is_consistent/3,
         try_adjust/3,
         dependent_agents/2,
         nogoods/3]).

-include("dcsp.hrl").

%% TODO: remove this attribute
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
    [{AId, Last}].

-spec is_consistent(aid(), agent_view(), problem()) -> boolean().
is_consistent(AId, AgentView, #problem{} = P) ->
    VarView = agent_view_to_var_view(AgentView),
    Constraints = [ C || C = {{x,A},_,{x,B}} <- P#problem.constraints,
                         A == AId orelse B == AId ],
    lists:all(mk_check_constraint(VarView), Constraints).

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
    [ E || E <- L ++ R, E > AId ].

-spec nogoods(aid(), agent_view(), problem()) -> [agent_view()].
nogoods(AId, AgentView, _Problem) ->
    [ lists:keydelete(AId, 1, AgentView) ].

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

agent_view_to_var_view(AgentView) ->
    [ {{x,AId},Val} || {AId,Val} <- AgentView ].

var_view_to_agent_view(VarView) ->
    [ {AId,Val} || {{x,AId},Val} <- VarView ].

mk_check_constraint(VarView) ->
    fun({Var1, Op, Var2}) ->
            case {proplists:get_value(Var1, VarView, false),
                  proplists:get_value(Var2, VarView, false)}
            of
                {false, _} ->
                    true;
                {_, false} ->
                    true;
                {Val1, Val2} ->
                    apply(fun erlang:Op/2, [Val1, Val2])
            end
    end.

mk_adjust_one(AId, AgentView, Problem) ->
    fun(_, {ok, NewAgentView}) ->
            {ok, NewAgentView};
       (NewCurrent, false) ->
            NewAgentView = lists:keyreplace(AId, 1, AgentView,
                                            {AId, NewCurrent}),
            case is_consistent(AId, NewAgentView, Problem) of
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
         [?_test(?assert(is_consistent(1, [], Problem))),
          ?_test(?assert(is_consistent(1, [{1,1}, {3,2}], Problem))),
          ?_test(?assert(is_consistent(2, [{2,1}, {3,2}], Problem))),
          ?_test(?assert(is_consistent(1, [{1,1}, {2,2}], Problem))),
          ?_test(?assertNot(is_consistent(1, [{1,1}, {3,1}], Problem))),
          ?_test(?assertNot(is_consistent(2, [{2,1}, {3,1}], Problem)))]).

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
          ?_test(?assertEqual([], dependent_agents(3, Problem)))]).

view_conversions_test_() ->
    ?LET({AV, VV},
         {[{1,2}, {2,2}, {3,2}],
          [{{x,1}, 2}, {{x,2}, 2}, {{x,3}, 2}]},
         [?_test(begin
                    VV1 = agent_view_to_var_view(AV),
                    ?assertEqual(AV, var_view_to_agent_view(VV1))
                 end),
          ?_test(begin
                    AV1 = var_view_to_agent_view(VV),
                    ?assertEqual(VV, agent_view_to_var_view(AV1))
                 end)]).

nogoods_test_() ->
    ?LET({Problem,
          AV1, Nogood1,
          AV2, Nogood2,
          AV3, Nogood3},
         {problem(),
          [{1,1}, {2,2}, {3,1}], [[{1,1}, {2,2}]],
          [{1,1}, {2,2}, {3,2}], [[{1,1}, {2,2}]],
          [{1,1}, {2,2}], [[{1,1}]]},
         [?_test(?assertEqual(Nogood1, nogoods(3, AV1, Problem))),
          ?_test(?assertEqual(Nogood2, nogoods(3, AV2, Problem))),
          ?_test(?assertEqual(Nogood3, nogoods(2, AV3, Problem)))]).

-endif.
