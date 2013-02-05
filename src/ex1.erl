-module(ex1).

-export([init/2]).

-include("dcsp.hrl").

init(AId, #problem{} = P) ->
    Domain = lists:nth(AId, P#problem.domains),
    Last = element(size(Domain), Domain),
    [{{x,AId}, Last}].

what_now(AId, #problem{} = P, AgentView) ->
    ok.
