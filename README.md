# Prolog & ASP Minesweeper

This repo contains two small logic-programming takes on Minesweeper for the assignment: a SWI-Prolog knowledge base with recursive reasoning, and an Answer Set Programming (ASP) encoding for clingo/clasp.

## Part 1 – Prolog (minesweeper.pl)
- Fixed 6x6 board with mines at (2,6), (3,3), (5,2), (6,5) and a sample player state (opened + flagged cells). State is dynamic; `reset_board/0` restores the initial layout.
- Key predicates: `clue/3` (computed counts), `safe_move/2` (cells proven safe), `must_flag/2` (cells that must be mines), and `reveal_area/3` (recursive flood fill for empty regions).
- The `reveal_area/3` predicate is the recursive relationship: it expands zero-clue cells and gathers the reachable region.
- Convenience runner: `sample_queries/0` prints the common queries in one go.

Example REPL session (SWI-Prolog):
```prolog
?- consult(minesweeper.pl).
true.

?- clue(2,3,C).
C = 1.

?- safe_move(X,Y).
X = 1, Y = 4 ;
X = 2, Y = 4 ;
X = 3, Y = 4.

?- must_flag(X,Y).
false.

?- reveal_area(1,1,Cells).
Cells = [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,1),(3,2),(4,1),(4,2)].

?- sample_queries.
Clue at (2,3): 1
Cells proven safe: [(1,4),(2,4),(3,4)]
Cells that must be mines: []
Flood reveal from (1,1): [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,1),(3,2),(4,1),(4,2)]
true.
```
Run the bundled demo non-interactively: `swipl -s minesweeper.pl -g sample_queries -t halt`.

### Web UI (Prolog HTTP server)
Launch a tiny HTTP server and open the GUI:
- Simple one-liner (blocks):  
  `swipl -g 'asserta(server_port(3000)),[web_server],start_server,thread_get_message(stop).'`
  (Feel free to change 3000.)
- Interactive: `swipl -s web_server.pl`, then at the prompt: `?- start_server.`
- Open http://localhost:3000

The page shows the fixed board, opened/flagged cells, safe/must-flag hints, and the flood fill, all computed from `minesweeper.pl` and exposed via `/state`.
- Interactions: left-click to open a cell, right-click/middle-click to toggle a flag, “Reset board” button to restore the initial state.

## Part 2 – ASP (minesweeper.lp)
- Domain: 4x4 grid, `clue/3` facts represent opened numbers at (1,1)=0, (2,3)=2, (3,4)=1.
- Rules generate candidate `mine/2` placements; constraints enforce the clue counts.
- Optimization: `#minimize { 1@1,X,Y: mine(X,Y) }.` prefers solutions with the fewest mines while still satisfying clues.
- Multiple optimal answer sets remain (e.g., different two-mine layouts).

Sample run (shows all optimal layouts):
```
clingo minesweeper.lp --opt-mode=optN 0
```
Expect several optimal answer sets with two mines each; the printed `mine(X,Y)` atoms identify each valid placement.

## Comparison
- Prolog version works from a concrete board plus player knowledge to derive safe moves and recursive flood fills; it mirrors interactive play.
- ASP version treats mine placement as a search problem: it enumerates all boards consistent with a few given clues, then optimizes for minimal mines. This illustrates declarative constraint solving and exploration of multiple answer sets.
