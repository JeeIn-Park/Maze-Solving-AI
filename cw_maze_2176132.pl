% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    find_moves(Agents,Moves),
    agents_do_moves(Agents,Moves),
    solve_maze.
    

% Find a possible move for each agent
find_moves([],[]).
find_moves([A|As],[M|Moves]) :-
    findall(P,agent_adjacent(A,P,_),PosMoves),
    random_member(M,PosMoves),
    find_moves(As,Moves).



solve_maze_by_dfs() :-


% True if A is a possible movement direction
m(north). 
m(east).
m(south). 
m(west).

% True if p(X,Y) is on the board
on_board(p(X,Y)) :- 
    ailp_grid_size(S), X>=1, X=<S, Y>=1, Y=<S.
%   ailp_grid_size(S), between(1,S,X), between(1,S,Y)

% True if p(X1,Y1) is one step in direction M from p(X,Y) (no bounds check)
pos_step(p(X,Y), west, p(X1,Y)) :- X1 is X-1.
pos_step(p(X,Y), east, p(X1,Y)) :- X1 is X+1.
pos_step(p(X,Y), north, p(X,Y1)) :- Y1 is Y-1.
pos_step(p(X,Y), south, p(X,Y1)) :- Y1 is Y+1.

% True if NPos is one step in direction M from Pos (with bounds check)
new_pos(Pos,M,NPos) :-
    on_board(Pos), pos_step(Pos, M, NPos), on_board(NPos).

% True if a L has the same length as the number of squares on the board
complete(L) :-
    ailp_grid_size(N), N2 is N*N, length(L,N2).

turn(east, south, clockwise).
turn(south, west, clockwise).
turn(west, north, clockwise).
turn(north, east, clockwise).
turn(D1, D2, anti_clockwise) := turn(D2, D1, clockwise).

% Perform a sequence of moves creating a spiral pattern, return the moves as L

% spiral - base case
spiral(Ps,Qs,_) :- complete(Ps), !, reverse(Ps,Qs).

% spiral - recursive case
spiral([Q,P|Ps],Qs,S) :- new_pos(P,D,Q), (C=D ; turn(D,C,S)), new_pos(Q,C,R), \+ member(R,[P|Ps]), spiral([R,Q,P|Ps],Qs,S).

% spiral - wrapper 
spiral(Ps) :- my_agent(A), get_agent_position(A,P), new_pos(P,_,Q), spiral([Q,P],Ps,_).