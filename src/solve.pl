:- module(solve, [bfs_solve/3]).
:- use_module(util).

%!  bfs_solve(+InitialState:list(string), -Solution:list(string), :Successor) is semidet
%
%   Perform a breadth-first search to find the solution.
bfs_solve(InitialState, Solution, Successor) :-
    find_bullseye(InitialState, BullseyeRow, BullseyeCol),
    count_robots(InitialState, RobotCount),
    MaxRobotIndex is RobotCount - 1,
    bfs_queue([[InitialState]], BullseyeRow, BullseyeCol, ReverseSolution, MaxRobotIndex, Successor, [InitialState]),
    reverse(Solution, ReverseSolution).

%!  bfs_queue(+Queue:list, +BullseyeRow:int, +BullseyeCol:int, -Solution:list(string), +MaxRobotIndex:int, :Successor, +Visited:list is semidet
%
%   Perform the BFS search using a queue.
bfs_queue([[State|Path]|_], BullseyeRow, BullseyeCol, Path, _, _, _) :-
    game_won(State, BullseyeRow, BullseyeCol).
bfs_queue([[State|Path]|Queue], BullseyeRow, BullseyeCol, Solution, MaxRobotIndex, Successor, Visited) :-
    findall([NextState|[Input|Path]], (call(Successor, State, Input, NextState, BullseyeRow, BullseyeCol, MaxRobotIndex, Visited, Queue)), Children),
    append(Queue, Children, NewQueue),
    append(Visited, [State], NewVisited),
    bfs_queue(NewQueue, BullseyeRow, BullseyeCol, Solution, MaxRobotIndex, Successor, NewVisited).