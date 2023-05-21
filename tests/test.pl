:- use_module('src/util.pl').
:- use_module('src/animate.pl').


:- begin_tests(movement).
test(down) :-
    test_directional(d, 'testboards/downbefore.txt', 'testboards/downafter.txt', 0).

test(up) :-
    test_directional(u, 'testboards/upbefore.txt', 'testboards/upafter.txt', 0).

test(left) :-
    test_directional(l, 'testboards/leftbefore.txt', 'testboards/leftafter.txt', 0).

test(right) :-
    test_directional(r, 'testboards/rightbefore.txt', 'testboards/rightafter.txt', 0).

% Checks to make sure robots can't move through each other.
test(robot_collision) :-
    test_directional(d, 'testboards/robot_collision_before.txt', 'testboards/robot_collision_after.txt', 1).

% Checks whether a robot stays still when trying to move through a wall.
test(nomove) :-
    test_directional(l, 'testboards/bullseye_before.txt', 'testboards/bullseye_before.txt', 1).

% Checks whether a bullseye gets placed back on the board after moving on it and moving away.
test(bullseye) :-
    load_board_from_file('testboards/bullseye_before.txt', BeforeBoard),
    find_bullseye(BeforeBoard, BullseyeRow, BullseyeCol),
    move_robot(BeforeBoard, 1, d, NewBeforeBoard, BullseyeRow, BullseyeRow),
    load_board_from_file('testboards/bullseye_after.txt', AfterBoard),
    same_game_board(NewBeforeBoard, AfterBoard),
    move_robot(NewBeforeBoard, 1, u, NewAfterBoard, BullseyeRow, BullseyeCol),
    same_game_board(BeforeBoard, NewAfterBoard).

:- end_tests(movement).

same_game_board(Board1, Board2) :-
    maplist(same_row, Board1, Board2).

% Ideally I would have just use NewBeforeBoard == AfterBoard to check for equality but that does not seem to work for some reason.
% When a robot gets replaced by a space after it's been moved, Row1 and Row2 will not be equal to each other according to prolog,
% even though they clearly are, as can also be seen by the code below where we check their character code lists instead which are identical.
same_row(Row1, Row2) :-
    name(Row1, Codes1),
    name(Row2, Codes2),
    Codes1 == Codes2.

test_directional(Direction, BeforeBoardFile, AfterBoardFile, RobotIndex) :-
    load_board_from_file(BeforeBoardFile, BeforeBoard),
    find_bullseye(BeforeBoard, BullseyeRow, BullseyeCol),
    move_robot(BeforeBoard, RobotIndex, Direction, NewBeforeBoard, BullseyeRow, BullseyeCol),
    load_board_from_file(AfterBoardFile, AfterBoard),
    same_game_board(NewBeforeBoard, AfterBoard).