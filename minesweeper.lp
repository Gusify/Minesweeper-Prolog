#const w=4.
#const h=4.

x(1..w).
y(1..h).
cell(X,Y) :- x(X), y(Y).

dx(-1;0;1).
dy(-1;0;1).

neighbor(X,Y,NX,NY) :-
    cell(X,Y),
    dx(DX), dy(DY),
    (DX, DY) != (0, 0),
    NX = X + DX, NY = Y + DY,
    NX >= 1, NX <= w,
    NY >= 1, NY <= h,
    cell(NX,NY).

% Opened clues from a partial Minesweeper game.
clue(1,1,0).
clue(2,3,2).
clue(3,4,1).

% Each cell may or may not contain a mine.
{ mine(X,Y) } :- cell(X,Y).

% Enforce numbers around the opened clues.
:- clue(X,Y,C),
   #count { NX,NY: neighbor(X,Y,NX,NY), mine(NX,NY) } != C.

% Prefer solutions with fewer total mines.
#minimize { 1@1,X,Y: mine(X,Y) }.

#show mine/2.
#show clue/3.
