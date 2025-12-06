:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).

:- [minesweeper].

:- dynamic server_port/1.
server_port(3000).

:- http_handler(root(.), index_handler, []).
:- http_handler(root(state), state_handler, []).
:- http_handler(root(click), click_handler, []).
:- http_handler(root(reset), reset_handler, []).
:- http_handler(root(static), serve_static, [prefix]).

start_server :-
    server_port(Port),
    http_server(http_dispatch, [port(Port)]),
    format('Minesweeper UI running on http://localhost:~w/~n', [Port]).

stop_server :-
    server_port(Port),
    http_stop_server(Port, []),
    format('Stopped server on port ~w.~n', [Port]).

index_handler(Request) :-
    http_reply_file('static/index.html', [unsafe(true)], Request).

serve_static(Request) :-
    memberchk(path_info(RelPath), Request),
    atom_concat('static', RelPath, File),
    http_reply_file(File, [unsafe(true)], Request).

state_handler(_Request) :-
    gather_state(State),
    reply_json_dict(State).

reset_handler(_Request) :-
    reset_board,
    gather_state(State),
    reply_json_dict(State).

click_handler(Request) :-
    http_read_json_dict(Request, Dict),
    (   _{action:Action0, x:XVal, y:YVal} :< Dict
    ->  true
    ;   reply_json_dict(_{error:"missing fields"}, [status(400)]), !
    ),
    (   to_int(XVal, X), to_int(YVal, Y)
    ->  true
    ;   reply_json_dict(_{error:"invalid coordinates"}, [status(400)]), !
    ),
    (within(X, Y) -> true ; reply_json_dict(_{error:"out of bounds"}, [status(400)]), !),
    normalize_action(Action0, ActionAtom),
    (   ActionAtom == open -> handle_open(X, Y)
    ;   ActionAtom == flag -> handle_flag(X, Y)
    ;   reply_json_dict(_{error:"invalid action"}, [status(400)]), !
    ),
    gather_state(State),
    reply_json_dict(State).

normalize_action(A0, Atom) :-
    string(A0),
    atom_string(Atom, A0).
normalize_action(A0, Atom) :-
    atom(A0),
    Atom = A0.

to_int(Val, Int) :-
    integer(Val),
    Int = Val.
to_int(Val, Int) :-
    number(Val),
    \+ integer(Val),
    Int is round(Val).
to_int(Val, Int) :-
    atom(Val),
    atom_number(Val, Int).
to_int(Val, Int) :-
    string(Val),
    number_string(Int, Val).

handle_open(X, Y) :-
    retractall(flagged(X, Y)),
    (   opened(X, Y)
    ->  true
    ;   assertz(opened(X, Y))
    ).

handle_flag(X, Y) :-
    (   opened(X, Y)
    ->  true  % ignore flag on opened cell
    ;   (flagged(X, Y) -> retractall(flagged(X, Y)) ; assertz(flagged(X, Y)))
    ).

gather_state(_{
    board:_{width:W, height:H},
    mines:Mines,
    opened:Opened,
    flagged:Flagged,
    safe_moves:SafeMoves,
    must_flag:MustFlag,
    flood_from:Flood,
    cells:Cells
}) :-
    board_size(W, H),
    findall([X, Y], mine(X, Y), Mines),
    findall([X, Y], opened(X, Y), Opened),
    findall([X, Y], flagged(X, Y), Flagged),
    findall([X, Y], safe_move(X, Y), RawSafe),
    sort(RawSafe, SafeMoves),
    findall([X, Y], must_flag(X, Y), RawMust),
    sort(RawMust, MustFlag),
    reveal_area(1, 1, FloodPairs),
    findall([FX, FY], member((FX, FY), FloodPairs), Flood),
    findall(Cell, cell_dict(Cell), Cells).

cell_dict(cell{
    x:X,
    y:Y,
    mine:Mine,
    opened:Opened,
    flagged:Flagged,
    clue:ClueVal,
    safe_hint:SafeHint,
    must_flag_hint:MustFlagHint
}) :-
    coord(X, Y),
    (mine(X, Y) -> Mine = true ; Mine = false),
    (opened(X, Y) -> Opened = true ; Opened = false),
    (flagged(X, Y) -> Flagged = true ; Flagged = false),
    (clue(X, Y, C) -> ClueVal = C ; ClueVal = null),
    (safe_move(X, Y) -> SafeHint = true ; SafeHint = false),
    (must_flag(X, Y) -> MustFlagHint = true ; MustFlagHint = false).
