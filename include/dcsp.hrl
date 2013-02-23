-type aid() :: pos_integer().

-type set(_ElementType) :: set().

-record(problem, {module :: atom(),
                  num_agents :: pos_integer(),
                  domains :: list(term()),
                  initial :: list(term()),
                  constraints :: list(term())}).
-type problem() :: #problem{}.

-type agent_view() :: [{aid(), term()}].
