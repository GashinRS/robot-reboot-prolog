:- module(util, [debug_print/2, substring/3, string_to_list_of_characters/2, list_of_characters_to_string/2, find_bullseye/3, find_robot/4,
    game_won/3, robot_unicode/2, replace_in_line/4, replace_in_list_of_strings/4, load_board_from_file/2, read_lines/2, 
    count_robots/2, print_board/1, row_to_string/2, print_solution/1, is_single_digit/1, is_valid_direction/1, nested_member/2]).

debug_print(Name, Variable) :-
    format("~w value: ~w", [Name, Variable]),
    writeln('').

%!  substring(+String: atom, ?Substring: atom, +Index: integer) is det
substring(String, Substring, Index) :-
    sub_atom(String, Index, _, _, Substring).

%   This predicate is from https://groups.google.com/g/comp.lang.prolog/c/PtG9chr9flI/m/fGwWSt-JSIsJ 
%!  string_to_list_of_characters(+String: string, ?Characters: list) is det
string_to_list_of_characters(String, Characters) :-
    name(String, Xs),
    maplist( number_to_character,
    Xs, Characters ).
    
number_to_character(Number, Character) :-
name(Character, [Number]).

%!  list_of_characters_to_string(+Characters:list(char), -String:string) is det
%
%   Convert a list of characters to a string.
list_of_characters_to_string(Characters, String) :-
    maplist(character_to_number, Characters, Xs),
    name(String, Xs).

%!  character_to_number(+Character:char, -Number:int) is det
%
%   Convert a character to its corresponding ASCII code number.
character_to_number(Character, Number) :-
    name(Character, [Number]).

%!  find_bullseye(+Board:list(string), -Row:int, -Col:int) is det
%
%   Find the bullseye (U+25CE) on the game board.
%   Unify Row and Col with the bullseye's row and column indices, respectively.
find_bullseye(Board, Row, Col) :-
    member(Line, Board),
    substring(Line, '\u25CE', Col),
    nth0(Row, Board, Line).

%!  find_robot(+Board:list(string), +RobotChar:char, -Row:int, -Col:int) is det
%
%   Find the robot with the specified Unicode character on the game board.
%   Unify Row and Col with the robot's row and column indices, respectively.
find_robot(Board, RobotChar, Row, Col) :-
    member(Line, Board),
    substring(Line, RobotChar, Col),
    nth0(Row, Board, Line).

%!  count_robots(+Board:list(string), -RobotCount:int) is det
%
%   Counts the amount of robots on the board and unifies with RobotCount.
count_robots(Board, RobotCount) :-
    count_robots_aux(Board, 0, 9, 0, RobotCount).

count_robots_aux(_, RobotIndex, MaxRobotIndex, Acc, Acc) :-
    RobotIndex > MaxRobotIndex.
count_robots_aux(Board, RobotIndex, MaxRobotIndex, Acc, RobotCount) :-
    RobotIndex =< MaxRobotIndex,
    robot_unicode(RobotIndex, RobotChar),
    (   find_robot(Board, RobotChar, _Row, _Col)
    ->  NewAcc is Acc + 1
    ;   NewAcc = Acc
    ),
    NextRobotIndex is RobotIndex + 1,
    count_robots_aux(Board, NextRobotIndex, MaxRobotIndex, NewAcc, RobotCount).


%!  game_won(+Board: list(string), +BullseyeRow: integer, +BullseyeCol: integer) is semidet
%
%   Determines whether a game is over by checking if robot 0 is at the same coordinates as the bullseye.
game_won(Board, BullseyeRow, BullseyeCol) :-
    robot_unicode(0, RobotChar),
    find_robot(Board, RobotChar, RobotRow, RobotCol),
    RobotRow = BullseyeRow,
    RobotCol = BullseyeCol.

%!  robot_unicode(+Index:int, -Unicode:char) is det
%
%   Unify the Unicode character corresponding to the robot's index.
robot_unicode(0, '\u25A3').
robot_unicode(1, '\u25A0').
robot_unicode(2, '\u25B2').
robot_unicode(3, '\u25C6').
robot_unicode(4, '\u25C7').
robot_unicode(5, '\u25C8').
robot_unicode(6, '\u25C9').
robot_unicode(7, '\u25E9').
robot_unicode(8, '\u25ED').
robot_unicode(9, '\u25F2').


%   Code from ChatGPT
%!  replace_in_line(+String:string, +Index:int, +Value:char, -NewString:string) is det
%
%   Replace the character at the specified index in the string with the given value.
%   Unify NewString with the updated string.
replace_in_line(String, Index, Value, NewString) :-
    string_to_list_of_characters(String, List),
    append(Head, [_ | Tail], List),
    length(Head, Index),
    append(Head, [Value | Tail], NewList),
    list_of_characters_to_string(NewList, NewString).

%   Code from ChatGPT
%!  replace_in_list_of_strings(+List:list(string), +Index:int, +Value:string, -NewList:list(string)) is det
%
%   Replace the string at the specified index in the list of strings with the given value.
%   Unify NewList with the updated list of strings.
replace_in_list_of_strings(List, Index, Value, NewList) :-
    append(Head, [_ | Tail], List),
    length(Head, Index),
    append(Head, [Value | Tail], NewList).

%!  load_board_from_file(+File:string, -Board:list(string)) is det
%
%   Load the game board from the specified file.
load_board_from_file(File, Board) :-
    open(File, read, Stream),
    read_lines(Stream, Board),
    close(Stream).

%   Code from ChatGPT
%!  read_lines(+Stream, -Lines:list(string)) is det
%
%   Read all lines from the given stream and store them in the Lines list.
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    ( Line = end_of_file
    -> Lines = []
    ; read_lines(Stream, RemainingLines),
      Lines = [Line | RemainingLines]
    ).

%!  print_board(+Board:list(list(char))) is det
%
%   Print out a game board
print_board(Board) :-
    maplist(row_to_string, Board, BoardStrings),
    maplist(writeln, BoardStrings).

%!  row_to_string(+Row:list(char), -RowString:string) is det
%
%   Convert a list of characters into a string.
row_to_string(Row, RowString) :-
    list_of_characters_to_string(Row, RowString).

%!  print_solution(+Solution: list(string)) is det
%
%   Print out a solution in the correct format
print_solution(Solution) :-
    atomic_list_concat(Solution, ',', Concatenated),
    upcase_atom(Concatenated, UppercaseConcatenated),
    writeln(UppercaseConcatenated).

%!  is_single_digit(+X) is semidet
%
%   Checks if X is a single digit.
is_single_digit(X) :- 
    integer(X), 
    X >= 0, 
    X =< 9.

%!  is_single_digit(+Direction:char) is semidet
%
%   Checks if the provided Direction is valid.
is_valid_direction(Direction) :-
    memberchk(Direction, [r, l, d, u]).

%   Code from ChatGPT
%!  nested_member(?Element, +List:list) is nondet
%
%   Search for Element within List, including nested sublists.
%   This predicate succeeds if Element is found within any level of nesting in List.
nested_member(X, [X|_]).
nested_member(X, [_|T]) :-
    nested_member(X, T).
nested_member(X, [H|_]) :-
    is_list(H),
    nested_member(X, H).
