-module(concurrent_sum).
-export([find_sum/2, find_total_sum/2, thread_sum/3, thread_total/4]).

% Same recursive sum function as sequential
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

% Master Receiver
thread_total(OldMsg, Count, NumProc, StartTime) ->
    % Alert the user to how many threads are remaining
    % io:fwrite("Waiting for ~p threads~n", [NumProc - Count]),

    % Receive messages
    receive
        % Take any message (we are only passing one type)
        Msg ->
            % If we have enough messages sent
            if
                Count + 1 == NumProc ->
                    EndTime = erlang:system_time(10000000),
                    % Write the final sum
                    io:fwrite("NumProc: ~p, Time: ~p, Sum: ~p~n", [NumProc, (EndTime - StartTime)/10000000, Msg + OldMsg]);
                true ->
                    % Else, recursively call ourselves one closer 
                    %   to the last call
                    thread_total(OldMsg + Msg, Count + 1, NumProc, StartTime)
            end
    end.

% Recursive function to spawn NumProc threads
spawn_thread(N, 0, NumProc, Master) -> 
    % io:fwrite("Done Spawning for ~p maximum, ~p Processes and Master ID ~p~n~n", 
    %          [N, NumProc, Master]);
    NumProc, Master, N;
spawn_thread(N, ID, NumProc, Master) ->
    % Spawn a new thread, and have it run the thread sum
    spawn(concurrent_sum, thread_sum, [(ID)*N/NumProc, (ID-1)*N/NumProc + 1, Master]),
    % Recursively call while decrementing our ID
    spawn_thread(N, ID - 1, NumProc, Master).

% Find the sum for a single Erlang thread
thread_sum(N, Min, MasterID) ->
    Result = find_sum(N, Min),                  % Sequentially find that sub-sum
    % io:fwrite("Max ~p and Min ~p: Result ~p~n",
    %          [N, Min, Result]),               % Show that it was found
    MasterID ! Result.                          % SEnd a message to the Master

% Thread called to find the sum: Entry Point
find_total_sum(N, NumProc) ->
    StartTime = erlang:system_time(10000000),
    Master = spawn(concurrent_sum, thread_total, 
                   [0, 0, NumProc, StartTime]), % Spawn the master receiver
    spawn_thread(N, NumProc , NumProc, Master).  % Spawn all the worker threads
