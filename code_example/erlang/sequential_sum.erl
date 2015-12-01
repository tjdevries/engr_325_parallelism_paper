-module(sequential_sum).
-export([find_sum/2, print_sum/2]).

find_sum(N, Min) ->
    if
        N == Min ->
            if
                Min == 0 ->
                    Min;
                Min > 0 ->
                    Min - 1
            end;
        N /= Min ->
            N - 1 + find_sum(N-1, Min)
    end.

print_sum(N, Min) -> 
    StartTime = erlang:system_time(10000000),
    Result = find_sum(N, Min),
    EndTime = erlang:system_time(10000000),
    io:fwrite("NumProc: ~p, Time: ~p, Sum: ~p~n", [1, (EndTime - StartTime) / 10000000, Result]).
