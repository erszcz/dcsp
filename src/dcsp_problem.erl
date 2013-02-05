-module(dcsp_problem).

-export([from_file/1]).

-include("dcsp.hrl").

from_file(FileName) ->
    {ok, Contents} = file:consult(FileName),
    #problem{module      = proplists:get_value(module, Contents),
             num_agents  = proplists:get_value(num_agents, Contents),
             domains     = proplists:get_value(domains, Contents),
             constraints = proplists:get_value(constraints, Contents)}.
