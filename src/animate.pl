% Animate library
:- module(animate, [animate/5, red/1, move_robot/6, move_robot_in_direction/6, draw/1]).

:- use_module(util).

%!  animate(+S, :P, :NW) is det
%
%   Animate a game starting from world *S*.
%   *P* is a predicate that outputs the picture representation of a world, and NW is a predicate that outputs the next world based on the start world
animate(S, P, NW, BullseyeRow, BullseyeCol) :-
    current_input(In),
    PF =.. [P, S, Picture],
    call(PF),
    clear(Clear),
    format("~w", [Clear]),
    draw(Picture),
    (   game_won(S, BullseyeRow, BullseyeCol)
    ->  green(Green),
        reset(Reset),
        format('~sYou won!~s~n', [Green, Reset]),
        writeln("Press Enter to exit."),
        wait_for_enter,
        halt
    ;   read_line_to_string(In, Line),
        normalize_space(atom(TrimmedLine), Line),
        NWF =.. [NW, S, TrimmedLine, Next_world, BullseyeRow, BullseyeCol],
        call(NWF),
        animate(Next_world, P, NW, BullseyeRow, BullseyeCol)
    ).


wait_for_enter :-
    get_single_char(Char),
    (Char = 13 % Enter key
    -> true;
    wait_for_enter).


%!  draw(++Picture) is det
%
%   Draws the *Picture*.
draw(text(M, Colour)) :-
    format("~w~w", [Colour, M]), nl, format(">_ ").
draw(board(Board)) :-
    maplist(writeln, Board), nl, format(">_ ").


% ANSI ESC code to clear entire screen
clear("\x1B\c").

% ANSI ESC Code to colour text red
red("\033[38;5;160m").

green("\x1B[32m").
reset("\x1B[0m").

%!  move_robot(+Board:list(string), +RobotIndex:int, +Direction:atom, -NewBoard:list(string)) is det
%
%   Move the robot with the specified index on the game board according to the
%   direction (r, l, d, or u for right, left, down, and up, respectively). The
%   new game board with the updated robot position will be unified with NewBoard.
move_robot(Board, RobotIndex, Direction, NewBoard, BullseyeRow, BullseyeCol) :-
    robot_unicode(RobotIndex, RobotChar),
    find_robot(Board, RobotChar, Row, Col),
    move_robot_in_direction(Board, Row, Col, Direction, NewRow, NewCol),
    update_board(Board, Row, Col, NewRow, NewCol, RobotChar, BullseyeRow, BullseyeCol, NewBoard).


%!  move_robot_in_direction(+Board:list(string), +Row:int, +Col:int, +Direction:atom, -NewRow:int, -NewCol:int) is det
%
%   Move the robot located at (Row, Col) in the specified direction until it
%   reaches an obstacle (a non-whitespace or bullseye character). Unify NewRow and NewCol
%   with the robot's new row and column indices, respectively.
move_robot_in_direction(Board, Row, Col, Direction, NewRow, NewCol) :-
    (   Direction == r -> DeltaRow = 0, DeltaCol = 1
    ;   Direction == l -> DeltaRow = 0, DeltaCol = -1
    ;   Direction == d -> DeltaRow = 1, DeltaCol = 0
    ;   Direction == u -> DeltaRow = -1, DeltaCol = 0
    ),
    NextRow is Row + DeltaRow,
    NextCol is Col + DeltaCol,
    (   valid_position(Board, NextRow, NextCol), \+ wall_or_robot(Board, NextRow, NextCol) ->
        move_robot_in_direction(Board, NextRow, NextCol, Direction, NewRow, NewCol)
    ;   NewRow = Row, NewCol = Col
    ).

%!  valid_position(+Board:list(string), +Row:int, +Col:int) is semidet
%
%   True if (Row, Col) is a valid position on the game board.
valid_position(Board, Row, Col) :-
    length(Board, RowCount),
    LastRow is RowCount - 2,
    between(1, LastRow, Row),
    nth0(Row, Board, Line),
    string_length(Line, ColCount),
    LastCol is ColCount - 2,
    between(1, LastCol, Col).


%!  wall_or_robot(+Board:list(string), +Row:int, +Col:int) is semidet
%
%   True if the cell at (Row, Col) on the game board contains a wall or a robot.
wall_or_robot(Board, Row, Col) :-
    nth0(Row, Board, Line),
    string_to_list_of_characters(Line, NewLine),
    nth0(Col, NewLine, Cell),
    \+ char_code(Cell, 32), % Space character
    Cell \== '\u25CE'. % Bullseye character


%!  update_board(+Board:list(string), +Row:int, +Col:int, +NewRow:int, +NewCol:int, +RobotChar:char, +BullseyeRow:int, +BullseyeCol:int, -NewBoard:list(string)) is det
%
%   Update the game board by moving the robot from (Row, Col) to (NewRow, NewCol).
%   Unify NewBoard with the updated game board.
%   If a robot is on the bullseye coordinates and moves away, the bullseye will be redrawn.
update_board(Board, Row, Col, NewRow, NewCol, RobotChar, BullseyeRow, BullseyeCol, NewBoard) :-
    update_board_aux(Board, Row, Col, NewRow, NewCol, RobotChar, NewBoardWithoutBullseye),
    (   Row == BullseyeRow, Col == BullseyeCol, 
        \+ (NewCol =:= Col, NewRow =:= Row)
    ->  nth0(BullseyeRow, Board, BullsEyeLine),
        replace_in_line(BullsEyeLine, BullseyeCol, '\u25CE', UpdatedBullsEyeLine),
        replace_in_list_of_strings(NewBoardWithoutBullseye, BullseyeRow, UpdatedBullsEyeLine, NewBoard)
    ;   NewBoard = NewBoardWithoutBullseye % In this case the bullseye will be present
    ).

%!  update_board_aux(+Board:list(string), +Row:int, +Col:int, +NewRow:int, +NewCol:int, +RobotChar:char, -NewBoard:list(string)) is det
%
%   Auxiliary predicate for update_board.
update_board_aux(Board, Row, Col, NewRow, NewCol, RobotChar, NewBoard) :-
    nth0(Row, Board, Line),
    replace_in_line(Line, Col, '\u0020', UpdatedLine),
    replace_in_list_of_strings(Board, Row, UpdatedLine, UpdatedBoard),
    nth0(NewRow, UpdatedBoard, NewLine),
    replace_in_line(NewLine, NewCol, RobotChar, UpdatedNewLine),
    replace_in_list_of_strings(UpdatedBoard, NewRow, UpdatedNewLine, NewBoard).
