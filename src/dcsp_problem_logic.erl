-module(dcsp_problem_logic).

-include_lib("dcsp/include/dcsp.hrl").

-callback init(aid(), problem())
    -> agent_view().

-callback is_consistent(aid(), agent_view(), problem())
    -> boolean().

-callback try_adjust(aid(), agent_view(), set(agent_view()), problem())
    -> {ok, agent_view()}
    | false.

-callback dependent_agents(aid(), problem())
    -> [aid()].

-callback nogoods(aid(), agent_view(), problem())
    -> [agent_view()].
