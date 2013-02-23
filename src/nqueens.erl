-module(nqueens).

%% API
-export([init/2,
         is_consistent/3,
         try_adjust/3,
         dependent_agents/2,
         nogoods/3]).

-include("dcsp.hrl").

%% TODO: remove this attribute
-compile([export_all]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

-spec init(aid(), problem()) -> agent_view().
init(AId, #problem{} = P) ->
    [{AId, lists:nth(AId, P#problem.initial)}].

-spec is_consistent(aid(), agent_view(), problem()) -> boolean().
is_consistent(AId, AgentView, Problem) ->
    VarView = agent_view_to_var_view(AgentView),
    lists:all(mk_check_constraint(VarView),
              concerning_constraints(AId, Problem)).

-spec try_adjust(aid(), agent_view(), problem())
    -> {ok, agent_view()} | false.
try_adjust(AId, AgentView, Problem) ->
    Current = proplists:get_value(AId, AgentView),
    lists:foldl(mk_adjust_one(AId, AgentView, Problem), false,
                still_not_tried(Current, Problem#problem.num_agents)).

-spec dependent_agents(aid(), problem()) -> [aid()].
dependent_agents(AId, Problem) ->
    GetAgents = fun({{x,A},_,{x,B}}) -> {A,B} end,
    Agents = [GetAgents(C) || C <- concerning_constraints(AId, Problem)],
    {L,R} = lists:unzip(Agents),
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

are_safe({Row1,Col1}, {Row2,Col2})
        when Row1 == Row2;
             Col1 == Col2;
             abs(Row1-Col1) == abs(Row2-Col2) ->
    false;
are_safe(_, _) ->
    true.

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

still_not_tried({Row,Col}, NumAgents) ->
    [{Row,C} || C <- lists:seq(Col+1, NumAgents) ++ lists:seq(1, Col)].

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
    ?LET({Problem, AV1, AV2},
         {problem(),
          [{1,{1,3}}, {2,{2,1}}],
          [{1,{1,3}}, {2,{2,1}}, {4,{3,2}}]},
         [?_test(?assert(is_consistent(1, AV1, Problem))),
          ?_test(?assert(is_consistent(3, AV1, Problem))),
          ?_test(?assert(is_consistent(2, AV1, Problem))),

          ?_test(?assert(is_consistent(1, AV2, Problem))),
          ?_test(?assert(is_consistent(3, AV2, Problem))),
          ?_test(?assertNot(is_consistent(2, AV2, Problem))),
          ?_test(?assertNot(is_consistent(4, AV2, Problem)))]).

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
     ?_test(?assertNot(are_safe({3,4},{4,3})))].

still_not_tried_test_() ->
    [?_test(?assertEqual([{4,2},{4,3},{4,4},{4,1}],
                        still_not_tried({4,1}, 4))),
     ?_test(?assertEqual([{4,4},{4,1},{4,2},{4,3}],
                        still_not_tried({4,3}, 4)))].

%try_adjust_test_() ->
%    ?_test(throw(fail)).

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
