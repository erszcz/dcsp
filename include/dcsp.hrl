-type aid() :: pos_integer().

-record(problem, {module :: atom(),
                  num_agents :: pos_integer(),
                  domains :: list(term()),
                  constraints :: list(term())}).
-type problem() :: #problem{}.

-type agent_view() :: [{aid(), term()}].
