-module(nqueens).

-behaviour(dcsp_problem_logic).

%% API
-export([init/2,
         is_consistent/3,
         try_adjust/4,
         dependent_agents/2]).

-include_lib("dcsp/include/dcsp.hrl").

%% TODO: remove this attribute
-compile([export_all]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec init(aid(), problem()) -> agent_view().
init(AId, #problem{} = P) ->
    try
        [{AId, lists:nth(AId, P#problem.initial)}]
    catch
        error:function_clause ->
            random:seed(now()),
            [{AId, {AId, random:uniform(P#problem.num_agents)}}]
    end.

-spec is_consistent(aid(), agent_view(), problem()) -> boolean().
is_consistent(AId, AgentView, Problem) ->
    VarView = agent_view_to_var_view(AgentView),
    lists:all(mk_check_constraint(VarView),
              concerning_constraints(AId, Problem)).

-spec try_adjust(aid(), agent_view(), set(agent_view()), problem())
    -> {ok, agent_view()} | false.
try_adjust(AId, AgentView, Nogoods, Problem) ->
    Current = proplists:get_value(AId, AgentView),
    lists:foldl(mk_adjust_one(AId, AgentView, Nogoods, Problem), false,
                still_not_tried(Current, Problem#problem.num_agents)).

-spec dependent_agents(aid(), problem()) -> [aid()].
dependent_agents(AId, Problem) ->
    GetAgents = fun({{x,A},_,{x,B}}) -> {A,B} end,
    Agents = [GetAgents(C) || C <- concerning_constraints(AId, Problem)],
    {L,R} = lists:unzip(Agents),
    [ E || E <- L ++ R, E > AId ].

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

agent_view_to_var_view(AgentView) ->
    [ {{x,AId},Val} || {AId,Val} <- AgentView ].

var_view_to_agent_view(VarView) ->
    [ {AId,Val} || {{x,AId},Val} <- VarView ].

concerning_constraints(AId, #problem{} = P) ->
    [ C || C = {{x,A},_,{x,B}} <- P#problem.constraints,
           A == AId orelse B == AId ].

mk_check_constraint(VarView) ->
    fun({Var1, safe, Var2}) ->
            case {proplists:get_value(Var1, VarView, false),
                  proplists:get_value(Var2, VarView, false)}
            of
                {false, _} ->
                    true;
                {_, false} ->
                    true;
                {Val1, Val2} ->
                    are_safe(Val1, Val2)
            end;
       ({_, Type, _}) ->
            error({unknown_constraint_type, Type})
    end.

%% "Safe" means that the queens can't harm each other - that is
%% they can't stand on the same horizontal or vertical line.
are_safe({Row1,Col1}, {Row2,Col2})
        when Row1 == Row2;
             Col1 == Col2 ->
    false;
%% They also can't stand on a diagonal.
are_safe({Row1,Col1}, {Row2,Col2}) ->
    {X1, Y1} = {Col1, Row1},
    {X2, Y2} = {Col2, Row2},
    {P, Q} = {X2-X1, Y2-Y1},
    not (P == Q orelse P == -Q).

mk_adjust_one(AId, AgentView, Nogoods, Problem) ->
    fun(_, {ok, NewAgentView}) ->
            {ok, NewAgentView};
       (NewCurrent, false) ->
            NewAgentView = lists:keyreplace(AId, 1, AgentView,
                                            {AId, NewCurrent}),
            case is_nogood(NewAgentView, Nogoods) of
                false ->
                    case is_consistent(AId, NewAgentView, Problem) of
                        true ->
                            {ok, NewAgentView};
                        false ->
                            false
                    end;
                true ->
                    false
            end
    end.

is_nogood(AgentView, Nogoods) ->
    sets:is_element(AgentView, Nogoods).

still_not_tried({Row,Col}, NumAgents) ->
    [{Row,C} || C <- lists:seq(Col+1, NumAgents) ++ lists:seq(1, Col-1)].

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

problem() ->
    #problem{num_agents = 4,
             constraints = [{{x,1}, safe, {x,2}},
                            {{x,1}, safe, {x,3}},
                            {{x,1}, safe, {x,4}},

                            {{x,2}, safe, {x,3}},
                            {{x,2}, safe, {x,4}},

                            {{x,3}, safe, {x,4}}]}.


is_consistent_test_() ->
    ?LET({Problem, AV1, AV2, AV3},
         {problem(),
          [{1,{1,3}}, {2,{2,1}}],
          [{1,{1,3}}, {2,{2,1}}, {4,{3,2}}],
          [{1,{1,2}}, {2,{2,4}}, {4,{4,3}}]},
         [?_test(?assert(is_consistent(1, AV1, Problem))),
          ?_test(?assert(is_consistent(3, AV1, Problem))),
          ?_test(?assert(is_consistent(2, AV1, Problem))),

          ?_test(?assert(is_consistent(1, AV2, Problem))),
          ?_test(?assert(is_consistent(3, AV2, Problem))),
          ?_test(?assertNot(is_consistent(2, AV2, Problem))),
          ?_test(?assertNot(is_consistent(4, AV2, Problem))),

          ?_test(?assert(is_consistent(4, AV3, Problem)))]).

are_safe_test_() ->
    [%% 2x2 chessboard
     ?_test(?assertNot(are_safe({1,1},{2,1}))),
     ?_test(?assertNot(are_safe({1,1},{2,2}))),
     ?_test(?assertNot(are_safe({1,1},{1,2}))),

     %% 3x3 chessboard
     ?_test(?assertNot(are_safe({1,1},{3,1}))),
     ?_test(?assertNot(are_safe({1,1},{3,1}))),
     ?_test(?assert(   are_safe({1,1},{3,2}))),
     ?_test(?assertNot(are_safe({1,1},{3,3}))),
     ?_test(?assert(   are_safe({1,1},{2,3}))),
     ?_test(?assertNot(are_safe({1,1},{1,3}))),
     ?_test(?assertNot(are_safe({3,2},{2,3}))),

     %% 4x4 chessboard
     ?_test(?assertNot(are_safe({1,1},{4,1}))),
     ?_test(?assert(   are_safe({1,1},{4,2}))),
     ?_test(?assert(   are_safe({1,1},{4,3}))),
     ?_test(?assertNot(are_safe({1,1},{4,4}))),
     ?_test(?assert(   are_safe({1,1},{3,4}))),
     ?_test(?assert(   are_safe({1,1},{2,4}))),
     ?_test(?assertNot(are_safe({1,1},{1,4}))),

     ?_test(?assert(   are_safe({1,2},{2,4}))),
     ?_test(?assert(   are_safe({2,4},{4,3}))),
     ?_test(?assert(   are_safe({4,3},{3,1}))),
     ?_test(?assert(   are_safe({3,1},{1,2}))),

     ?_test(?assertNot(are_safe({2,4},{4,2}))),
     ?_test(?assertNot(are_safe({3,4},{4,3}))),

     ?_test(?assert(   are_safe({1,2},{4,3})))].

still_not_tried_test_() ->
    [?_test(?assertEqual([{4,2},{4,3},{4,4}],
                        still_not_tried({4,1}, 4))),
     ?_test(?assertEqual([{4,4},{4,1},{4,2}],
                        still_not_tried({4,3}, 4)))].

try_adjust_test_() ->
    ?LET({Problem, NG1, NG2, AV1, AV2, AV3, AV4, AV5},
         {problem(),
          sets:from_list([[{1,{1,1}}, {2,{2,3}}]]),
          sets:from_list([[{1,{1,1}}, {2,{2,3}}],
                          [{1,{1,1}}, {2,{2,4}}],
                          [{1,{1,1}}, {2,{2,1}}],
                          [{1,{1,1}}, {2,{2,2}}]]),
          [{1,{1,1}}, {2,{3,3}}],
          [{1,{1,2}}, {2,{3,3}}],
          [{1,{1,1}}, {2,{3,4}}],
          [{1,{1,1}}, {2,{2,1}}],
          [{1,{1,1}}, {2,{2,4}}]},
         [?_test(?assertEqual({ok, AV2}, try_adjust(1, AV1, NG1, Problem))),
          ?_test(?assertEqual({ok, AV3}, try_adjust(2, AV1, NG1, Problem))),
          ?_test(?assertEqual({ok, AV5}, try_adjust(2, AV4, NG1, Problem))),
          ?_test(?assertEqual(false, try_adjust(2, AV4, NG2, Problem)))]).

dependent_agents_test_() ->
    ?LET(Problem, problem(),
         [?_test(?assertEqual([2,3,4], dependent_agents(1, Problem))),
          ?_test(?assertEqual([3,4], dependent_agents(2, Problem))),
          ?_test(?assertEqual([4], dependent_agents(3, Problem))),
          ?_test(?assertEqual([], dependent_agents(4, Problem)))]).

%view_conversions_test_() ->
%    ?_test(throw(fail)).

%nogoods_test_() ->
%    ?_test(throw(fail)).

-endif.
