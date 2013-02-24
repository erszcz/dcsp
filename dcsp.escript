#!/usr/bin/env escript
%%! -pa ebin

-export([main/1]).

main([]) ->
    io:format("usage: dcsp PROBLEM_FILE~n");
main([File]) ->
    error_logger:delete_report_handler(error_logger_tty_h),
    dcsp:start(),
    io:format("~p~n", [dcsp:solve(dcsp_problem:from_file(File))]).
