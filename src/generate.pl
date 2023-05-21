:- module(generate, [generate_valid_board/7]).

:- use_module(util).
:- use_module(solve).

%!  generate_board(+Rows:int, +Cols:int, -Board:list(list(char))) is det
%   Generates an empty board of size Rows x Cols.
generate_board(Rows, Cols, Board) :-
    NewRows is (Rows * 2) + 1,
    NewCols is (Cols * 2) + 1,
    TopRow = ['\u250F'|TopRowRest], % ┏
    replicate('\u2501', NewCols-2, TopRowRest),% ━
    append(TopRow, ['\u2513'], NewTopRow), % ┓
    BottomRow = ['\u2517'|BottomRowRest], % ┗
    replicate('\u2501', NewCols-2, BottomRowRest), % ━
    append(BottomRow, ['\u251B'], NewButtomRow), % ┛
    MiddleRow = ['\u2503'|MiddleRowRest], % ┃
    replicate(' ', NewCols-2, MiddleRowRest),
    append(MiddleRow, ['\u2503'], NewMiddleRow), % ┃
    replicate(NewMiddleRow, NewRows-2, MiddleRows),
    append([NewTopRow], MiddleRows, BoardWithoutBottom),
    append(BoardWithoutBottom, [NewButtomRow], Board).

%   Code from ChatGPT
%   Helper predicate to replicate an element N times
replicate(_, 0, []).
replicate(X, N, [X|Rest]) :-
    N > 0,
    N1 is N - 1,
    replicate(X, N1, Rest).

%   random_wall_char(-WallChar:atom) is multi
%   Picks a random wall character from the list of wall characters.
random_wall_char(WallChar) :-
    WallChars = ['\u2501', % ━
                    '\u2503', % ┃
                    '\u250F', % ┏
                    '\u2513', % ┓
                    '\u2517', % ┗
                    '\u251B', % ┛
                    '\u2523', % ┣
                    '\u252B', % ┫
                    '\u2533', % ┳
                    '\u253B'], % ┻
    random_member(WallChar, WallChars).

add_random_walls(Board, 0, Board).

%!  add_random_walls(+Board:list(list(char)), +NumWalls:int, -NewBoard:list(list(char))) is nondet
%   Adds NumWalls random walls to the Board and returns the modified NewBoard.
add_random_walls(Board, NumWalls, NewBoard) :-
    NumWalls > 0,
    add_random_wall(Board, TempBoard),
    NewNumWalls is NumWalls - 1,
    add_random_walls(TempBoard, NewNumWalls, NewBoard).

%   add_random_wall(+Board:list(list(char)), -NewBoard:list(list(char))) is nondet
%   Adds a single random wall to the Board and returns the modified NewBoard.
add_random_wall(Board, NewBoard) :-
    random_wall_char(RandomWall),
    add_char_to_board(Board, RandomWall, wall, NewBoard).

%   Code from ChatGPT
%!  replace_in_matrix(+Matrix:list, +RowIndex:int, +ColIndex:int, ?OldValue, -NewMatrix:list, ?NewValue) is det
%   Replaces the element at position (RowIndex, ColIndex) in the given Matrix
%   with the NewValue and returns the modified NewMatrix.
%   If the OldValue at the specified position does not match, the NewMatrix
%   will be the same as the input Matrix.
replace_in_matrix(Matrix, RowIndex, ColIndex, OldValue, NewMatrix, NewValue) :-
    nth0(RowIndex, Matrix, Row),
    (nth0(ColIndex, Row, OldValue) ->
        (replace(Row, ColIndex, OldValue, NewValue, NewRow),
            replace(Matrix, RowIndex, Row, NewRow, NewMatrix));
        NewMatrix = Matrix).

%   Code from ChatGPT
%!  replace(+List:list, +Index:int, ?OldValue, ?NewValue, -NewList:list) is det
%   Replaces the element at the specified Index in the given List
%   with the NewValue and returns the modified NewList.
%   If the OldValue at the specified position does not match, the NewList
%   will be the same as the input List.
replace([_|T], 0, _, X, [X|T]).
replace([H|T], I, O, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, O, X, R).


%!  add_bullseye(+Board:list(list(char)), -Board:list(list(char))) is nondet
%
%   Add a bullseye to a random cell on the board
add_bullseye(Board, NewBoard) :-
    add_char_to_board(Board, '\u25Ce', bullseye, NewBoard). % '◎'

%!  add_robots(+Board:list(list(char)), +RobotCount:int, -NewBoard:list(list(char))) is nondet
%
%   Adds robots to random cells on the board. The amount of robots that are added is RobotCount.
add_robots(Board, RobotCount, NewBoard) :-
    MaxIndex is RobotCount - 1,
    add_robots(Board, RobotCount, MaxIndex, NewBoard).

add_robots(Board, 0, _, Board).
add_robots(Board, RobotCount, RobotIndex, NewBoard) :-
    RobotCount > 0,
    robot_unicode(RobotIndex, RobotChar),
    add_char_to_board(Board, RobotChar, robot, TempBoard),
    NewRobotCount is RobotCount - 1,
    NewRobotIndex is RobotIndex - 1,
    add_robots(TempBoard, NewRobotCount, NewRobotIndex, NewBoard).

%!  generate_valid_board(+Height:int, +Breadth:int, +MaxWalls:int, +RobotCount:int, -FinalBoard:list(list(char)), +Successor, -Solution:list(string)) is nondet
%   
%   Generates a valid board of size Height x Breadth with MaxWalls random walls
%   and RobotCount robots. The board is considered valid if all robots can reach
%   the bullseye.
generate_valid_board(Height, Breadth, MaxWalls, RobotCount, FinalBoard, Successor, FinalSolution) :-
    generate_board(Height, Breadth, Board),
    random_between(0, MaxWalls, NumRandomWalls),
    add_random_walls(Board, NumRandomWalls, NewBoard),
    add_bullseye(NewBoard, BullseyeBoard),
    add_robots(BullseyeBoard, RobotCount, RobotsBoard),
    (is_board_valid(RobotsBoard, Successor, Solution, RobotCount) 
    ->  FinalBoard = RobotsBoard, 
        FinalSolution = Solution,
        true
    ;   generate_valid_board(Height, Breadth, MaxWalls, RobotCount, FinalBoard, Successor, FinalSolution)
    ).
    
%!  random_between_parity(+Low:int, +High:int, +Parity:atom, -Value:int) is nondet
%
%   Generates a random number with the specified Parity between Low and High (inclusive).
random_between_parity(Low, High, Parity, Value) :-
    random_between(Low, High, X),
    (Parity == even
    -> (X mod 2 =:= 0 -> Value = X ; random_between_parity(Low, High, Parity, Value))
    ;  (X mod 2 =:= 1 -> Value = X ; random_between_parity(Low, High, Parity, Value))).

%!  two_random_odd(+LowX:int, +HighX:int, +LowY:int, +HighY:int, -Values:list(int)) is nondet
%
%   Generates two random odd numbers, X from between LowX and HighX and Y from between LowY and HighY (inclusive).
two_random_odd(LowX, HighX, LowY, HighY, X, Y) :-
    random_between_parity(LowX, HighX, odd, X),
    random_between_parity(LowY, HighY, odd, Y).

%!  two_random_not_both_odd(+LowX:int, +HighX:int, +LowY:int, +HighY:int, -X:int, -Y:int) is nondet
%
%   Generates two random numbers that are not both odd. X is from between LowX and HighX and Y is from between LowY and HighY (inclusive).
two_random_not_both_odd(LowX, HighX, LowY, HighY, X, Y) :-
    random_between(LowX, HighX, RandX),
    random_between(LowY, HighY, RandY),
    (   RandX mod 2 =:= 1, 
        RandY mod 2 =:= 1
    -> two_random_not_both_odd(LowX, HighX, LowY, HighY, X, Y)
    ;   X = RandX,
        Y = RandY).

%!  add_char_to_board(+Board:list(list(char)), +Char:char, +Type:atom, -NewBoard:list(list(char))) is nondet
%
%   Add a given character to a random cell in the board with the specified type of character (walls vs robots and the bullseye) for row and column.
%   The random cell will not necessarily be empty.
add_char_to_board(Board, Char, Type, NewBoard) :-
    length(Board, Rows),
    nth0(1, Board, Row),
    length(Row, Cols),
    MaxRows is Rows - 2,
    MaxCols is Cols - 2,
    (   Type == wall
    ->  two_random_not_both_odd(1, MaxRows, 1, MaxCols, RandRow, RandCol)
    ;   two_random_odd(1, MaxRows, 1, MaxCols, RandRow, RandCol)
    ),
    replace_in_matrix(Board, RandRow, RandCol, _, NewBoard, Char).

%!  is_board_valid(+Board:list(list(char)), +Successor, -Solution:list(string), +RobotCount:int) is det
%   
%   Checks if the given Board is valid by ensuring the number of robots matches RobotCount and 
%   the robot with index 0 can reach the bullseye.
is_board_valid(Board, Successor, Solution, RobotCount) :-
    maplist(row_to_string, Board, BoardStrings),
    count_robots(BoardStrings, TotalRobots),
    TotalRobots == RobotCount,
    bfs_solve(BoardStrings, Solution, Successor).
