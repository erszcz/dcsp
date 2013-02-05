-record(problem, {module :: atom(),
                  num_agents :: pos_integer(),
                  domains :: list(term()),
                  constraints :: list(term())}).
-type problem() :: #problem{}.
