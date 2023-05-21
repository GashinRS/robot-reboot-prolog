:- use_module(animate).
:- use_module(util).
:- use_module(solve).
:- use_module(generate).

:- initialization(main).

%!  next_world(?X:list(string), +I:string, -Y:list(string), +BullseyeRow:int, +BullseyeCol:int) is det
%!  next_world(+X:list(string), ?I:string, -Y:list(string), +BullseyeRow:int, +BullseyeCol:int) is det
%   
%   Based on the current world and command-line input, outputs the new world.
next_world(World, Input, NewWorld, BullseyeRow, BullseyeCol) :-
    (   atom_length(Input, 2),
        atom_codes(Input, [RobotIndexChar, DirectionCode]),
        char_type(RobotIndexChar, digit),
        number_codes(RobotIndex, [RobotIndexChar]),
        char_code(DirectionChar, DirectionCode),
        to_lower(DirectionChar, LowerDirection),
        char_code(Direction, LowerDirection),
        move_robot(World, RobotIndex, Direction, NewWorld, BullseyeRow, BullseyeCol)
    ->  true
    ;   NewWorld = World
    ).

%!  next_world_successors(+State:list(string), -NextState:list(string), -Input:string, +BullseyeRow:int, +BullseyeCol:int) is det
%
%   Based on the current world and input, outputs the new world.
next_world_successors(World, Input, NewWorld, BullseyeRow, BullseyeCol, MaxRobotIndex, Visited, Queue) :-
    % Generate the next world for each robot index and direction
    between(0, MaxRobotIndex, RobotIndex),
    member(Direction, [r, l, d, u]),
    list_of_characters_to_string([RobotIndex, Direction], Input),
    next_world(World, Input, NewWorld, BullseyeRow, BullseyeCol),
    not(nested_member(NewWorld, Queue)),
    not(member(NewWorld, Visited)).
    

%!  next_world(?N, ?Picture) is det
%   
%   True if *Picture* is the screen representation of the world *N*.
picture(Board, board(Board)).

%   Code from ChatGPT
%   Rules for parsing different arguments
parse_arguments(Args, Game, Solve, Gen, Test) :-
    (   member(Arg, Args), 
        atom_concat('--game=', GameArg, Arg) 
    ->  Game = GameArg 
    ;   Game = default_game ),
    (   member(Arg, Args), 
        atom_concat('--solve', SolveArg, Arg) 
    -> (SolveArg = '' 
        ->  Solve = stdin 
        ;   atom_concat('=', SolveFile, SolveArg), 
            Solve = SolveFile) 
    ;   Solve = default_solve ),
    (   member(Arg, Args), 
        atom_concat('--gen=', GenArg, Arg) 
    -> (atom_chars(GenArg, Chars), 
        read_from_chars(Chars, Gen)) 
    ;   Gen = default_gen ),
    (   member(Arg, Args), 
        atom_concat('--test=', TestArg, Arg),
        downcase_atom(TestArg, Lowercase)
    -> (atom_chars(Lowercase, Chars), 
        read_from_chars(Chars, Test)) 
    ;   Test = default_test ).
    

handle_solve(stdin) :-
    read_lines(user_input, Board),
    (   bfs_solve(Board, Solution, next_world_successors)
    ->  print_solution(Solution)
    ;   format("No solution found~n")
    ).

handle_game(File) :-
    writeln(File),
    load_board_from_file(File, Board),
    find_bullseye(Board, BullseyeRow, BullseyeCol),
    animate(Board, picture, next_world, BullseyeRow, BullseyeCol).

handle_gen(Gen) :-
    (   nth0(0, Gen, RobotCount),
        nth0(1, Gen, Breadth),
        nth0(2, Gen, Height),
        integer(RobotCount),
        RobotCount > 0,
        RobotCount =< 10,
        integer(Breadth),
        integer(Height)
    ->  MaxWalls is (((Height - 1) * ((2 * Breadth) -1)) +  ((Breadth - 1) * ((2 * Height) - 1))),
        generate_valid_board(Height, Breadth, MaxWalls, RobotCount, RobotsBoard, next_world_successors, Solution),
        print_board(RobotsBoard),
        print_solution(Solution)
    ;   format("Invalid input. Please use the --gen option like this: swipl main.pl -- '--gen=[{robot count},{breadth},{height}]'~n"),
        format("and ensure the robot count is between 1 and 10 (both inclusive)~n")
    ).

handle_test(Test) :-
    (   nth0(0, Test, RobotIndex),
        nth0(1, Test, Direction),
        is_single_digit(RobotIndex), 
        is_valid_direction(Direction)
    ->  read_lines(user_input, Board),
        move_robot(Board, RobotIndex, Direction, NewBoard, _, _),
        maplist(writeln, NewBoard)
    ;   format("Invalid input. Please use the --test option like this: cat input | swipl main.pl -- '--test=[{robot index},{direction}]'~n"),
        format("and ensure robot index is a single digit and direction is one of [d, u, r, l, D, U, R, L].~n")
    ).
    

main :-
    % Retrieve command-line arguments
    current_prolog_flag(argv, Argv),

    % Parse the arguments
    parse_arguments(Argv, Game, Solve, Gen, Test),

    (   Solve \= default_solve
    ->  handle_solve(Solve)
    ;   Gen \= default_gen
    ->  handle_gen(Gen)
    ;   Game \= default_game
    ->  handle_game(Game)
    ;   Test \= default_test
    ->  handle_test(Test)
    ;   format("No option provided~n"),
        format("Please use either --game, --solve, --gen or --test~n")
    ),
    halt.