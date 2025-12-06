:- use_module(library(lists)).

:- dynamic opened/2, flagged/2.

% Board description for a 6x6 Minesweeper session.
board_size(6, 6).
mine(2, 6).
mine(3, 3).
mine(5, 2).
mine(6, 5).

% Initial player knowledge: opened cells and flags placed so far.
initial_opened(1, 1).
initial_opened(1, 2).
initial_opened(1, 3).
initial_opened(2, 1).
initial_opened(2, 2).
initial_opened(2, 3).
initial_opened(3, 1).
initial_opened(3, 2).
initial_opened(4, 1).
initial_opened(4, 2).
initial_opened(4, 3).

initial_flagged(3, 3).
initial_flagged(6, 5).

reset_board :-
    retractall(opened(_, _)),
    retractall(flagged(_, _)),
    forall(initial_opened(X, Y), assertz(opened(X, Y))),
    forall(initial_flagged(X, Y), assertz(flagged(X, Y))).

:- initialization(reset_board).

% Domain helpers.
within(X, Y) :-
    board_size(W, H),
    X >= 1,
    X =< W,
    Y >= 1,
    Y =< H.

coord(X, Y) :-
    board_size(W, H),
    between(1, W, X),
    between(1, H, Y).

offset(-1, -1).
offset(-1, 0).
offset(-1, 1).
offset(0, -1).
offset(0, 1).
offset(1, -1).
offset(1, 0).
offset(1, 1).

neighbor(X, Y, NX, NY) :-
    offset(DX, DY),
    NX is X + DX,
    NY is Y + DY,
    (DX =\= 0 ; DY =\= 0),
    within(NX, NY).

% Knowledge about bombs and counts.
safe(X, Y) :-
    coord(X, Y),
    \+ mine(X, Y).

adjacent_mines(X, Y, Count) :-
    coord(X, Y),
    findall((NX, NY), (neighbor(X, Y, NX, NY), mine(NX, NY)), Mines),
    length(Mines, Count).

clue(X, Y, Count) :-
    safe(X, Y),
    adjacent_mines(X, Y, Count).

hidden(X, Y) :-
    coord(X, Y),
    \+ opened(X, Y),
    \+ flagged(X, Y).

flagged_count(X, Y, Count) :-
    coord(X, Y),
    findall((NX, NY), (neighbor(X, Y, NX, NY), flagged(NX, NY)), Flags),
    length(Flags, Count).

hidden_neighbors(X, Y, Hidden) :-
    findall((NX, NY), (neighbor(X, Y, NX, NY), hidden(NX, NY)), Hidden).

% Logical inferences a player can make.
% If all bombs around a clue are already flagged, the rest are safe moves.
safe_move(X, Y) :-
    opened(OX, OY),
    clue(OX, OY, Clue),
    flagged_count(OX, OY, Flags),
    Flags =:= Clue,
    hidden_neighbors(OX, OY, Hidden),
    member((X, Y), Hidden).

% If the remaining hidden neighbors exactly match the missing bombs, they must be flagged.
must_flag(X, Y) :-
    opened(OX, OY),
    clue(OX, OY, Clue),
    flagged_count(OX, OY, Flags),
    hidden_neighbors(OX, OY, Hidden),
    length(Hidden, HiddenCount),
    Missing is Clue - Flags,
    Missing > 0,
    Missing =:= HiddenCount,
    member((X, Y), Hidden).

% Recursive reveal of empty regions (flood fill).
reveal_area(X, Y, Cells) :-
    safe(X, Y),
    reveal_area(X, Y, [], RawCells),
    sort(RawCells, Cells).

reveal_area(X, Y, Visited, []) :-
    member((X, Y), Visited),
    !.

reveal_area(X, Y, _Visited, [(X, Y)]) :-
    clue(X, Y, Count),
    Count > 0.

reveal_area(X, Y, Visited, [(X, Y) | Cells]) :-
    clue(X, Y, 0),
    \+ member((X, Y), Visited),
    NewVisited = [(X, Y) | Visited],
    findall(SubCells,
            (neighbor(X, Y, NX, NY),
             safe(NX, NY),
             reveal_area(NX, NY, NewVisited, SubCells)),
            Nested),
    append_nested(Nested, Cells).

append_nested([], []).
append_nested([H | T], Out) :-
    append(H, Rest, Out),
    append_nested(T, Rest).

% Convenience predicate to demonstrate the knowledge base.
sample_queries :-
    clue(2, 3, Clue23),
    format('Clue at (2,3): ~w~n', [Clue23]),
    findall((X, Y), safe_move(X, Y), RawSafe),
    sort(RawSafe, SafeMoves),
    format('Cells proven safe: ~w~n', [SafeMoves]),
    findall((X, Y), must_flag(X, Y), RawFlags),
    sort(RawFlags, Flags),
    format('Cells that must be mines: ~w~n', [Flags]),
    reveal_area(1, 1, Flood),
    format('Flood reveal from (1,1): ~w~n', [Flood]).
