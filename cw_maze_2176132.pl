:- dynamic agent_state/3. % agent_state(Agent, Path, Backtrack)
:- dynamic divergence_map/2. % divergence_map(Location, RightPath)
:- dynamic dead_moves/1.

% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    my_agents(Agents),
    reverse(Agents,NewAgents),
    maplist(init_agent_state,NewAgents),  % Initialize agent states
    init_map_state,
    init_dead_moves,
    solve_maze(NewAgents).

solve_maze(Agents) :-
    print_divergence_map,
    find_moves(Agents,Moves),
    agents_do_moves(Agents,Moves),
    solve_maze(Agents).

init_agent_state(Agent) :-
    retractall(agent_state(Agent,_,_)),
    get_agent_position(Agent,Pos),
    assert(agent_state(Agent,[Pos],[])).

get_agent_state(Agent,Path,Divergences) :-
    agent_state(Agent,Path,Divergences).

update_agent_state(Agent, Path,Divergences) :-
    retractall(agent_state(Agent,_,_)), % Remove the existing state
    assert(agent_state(Agent,Path,Divergences)). % Assert the new state

init_map_state() :-
    retractall(divergence_map(_,_)).

% Query divergence map
get_divergence_map(Location, RightPath) :-
    divergence_map(Location, RightPath).

% Update divergence map
update_divergence_map(Location,RightPath) :-
    retractall(divergence_map(Location,_)),
    assert(divergence_map(Location, RightPath)).

delete_divergence_map(Location,RightPath) :-
    retractall(divergence_map(Location,RightPath)).

% Print all key-value pairs in the divergence map
print_divergence_map :-
    findall(Location-RightPath, divergence_map(Location, RightPath), Pairs),
    print_pairs(Pairs).

% Helper predicate to print each key-value pair
print_pairs([]).
print_pairs([Location-RightPath | Rest]) :-
    format("Divergence at ~w leads to: ~w~n", [Location, RightPath]),
    print_pairs(Rest).

init_dead_moves :-
    retractall(dead_moves(_)),
    assert(dead_moves([])).

update_dead_moves(NewDeadMove) :-
    dead_moves(Dead),
    retractall(dead_moves(_)),
    assert(dead_moves([NewDeadMove|Dead])).

is_dead_move(Move) :-
    dead_moves(Dead),
    member(Move, Dead).

find_moves([],[]).
% find_moves(+AgentsStates,-Moves)
find_moves([Agent|Rest],[Move|Moves]) :-
    get_agent_state(Agent,Path,Divergences),
    (leave_maze(Agent) -> true
    ;otherwise         -> find_moves_dfs(Agent,Path,Divergences,FinalMove), Move=FinalMove,
                          find_moves(Rest,Moves)).

% find_moves_dfs(+Agent,+Path,+Backtrack,-Move)
find_moves_dfs(Agent,Path,Divergences,FinalMove) :-
    get_agent_position(Agent,Pos),
    findall(Next, ((map_adjacent(Pos,Next,empty);map_adjacent(Pos,Next,a(_))), \+ member(Next,Path), \+ is_dead_move(Next)), PosMoves), % \+ agent occupying
    findall(P, (member(P,PosMoves), agent_adjacent(Agent,P,empty)),ActualMoves), % among possible moves, which can move right now (there is no agent)

    format('Before call::: Agent: ~w, In:~w, PosMoves:~w ActualMoves:~w\n',[Agent,Pos,PosMoves,ActualMoves]),
    format('Agent in : ~w, PosMoves: ~w, Divergence: ~w\n',[Pos,PosMoves,Divergences]),

    (length(ActualMoves,0)  -> FinalMove=Pos 
    ;otherwise              -> choose_move(Agent,Pos,PosMoves,Path,Divergences,Move),
                                                       (my_agents(As), findall(Ag,(member(Ag,As), get_agent_position(Ag,P),P==Move),Conflict),
                                                        \+ length(Conflict,0) -> FinalMove = Pos, get_agent_state(Agent,Updated,Divs), Updated=[_|Rest],
                                                                                 update_agent_state(Agent,Rest,Divs), format("AGENT: ~w conflicts with Agent:~w, Intended:~w", [Agent,Conflict,Move])
                                                                                 % If the move conflicts with agent position, cancel the move
                                                        ;otherwise            -> FinalMove=Move)).
 

choose_move(Agent,Pos,[],Path,Divergences,Move) :-
    update_dead_moves(Pos),
    findall(Next,
            ((map_adjacent(Pos,Next,empty);map_adjacent(Pos,Next,a(_))), \+ is_dead_move(Next)),
            Backtracking),
    format('Bactracking:~w', [Backtracking]),
    Backtracking=[Move], NewPath=[Move|Path],
    (member(Pos,Divergences) -> Divergences = [_|Rest], NewDivs= Rest,
                                update_divergence_map(Pos,Move) % Sets the divergence point to the previous divergence (Since the divergence itself is a dead end)
    ;otherwise               -> NewDivs=Divergences),
    update_agent_state(Agent,NewPath,NewDivs),
    format('Path:~w, Move:~w\n\n', [NewPath,Move]).


choose_move(Agent,Pos,PosMoves,Path,Divergences,Move) :-
    (length(PosMoves,1) -> PosMoves=[Move],NewDivs=Divergences,format("F\n") % Follow the move if there is only single possible move
    ;otherwise          -> ( PosMoves = [P|Ps], 
    (map_adjacent(P,_,unknown)  -> Move = P
    ;otherwise                  -> choose_move(Agent,Pos,Ps,Path,Divergences,Move)))),
    
   % choose_move(Agent,Pos,PosMoves,Path,Divergences,Move)
        
        
        
        
        get_divergence_map(Pos,RightPath),
                            \+is_dead_move(RightPath)            -> findall(M, (member(M,PosMoves), M \= RightPath), Alter),
                                                                    Alter=[Move|_],
                                                                    NewDivs=Divergences,format("G\n")
    %                                ;otherwise                   -> (\+get_divergence_map(Pos,_) -> NewDivs=[Pos|Divergences]
    %                                                                ;otherwise                    -> NewDivs=Divergences),
    %                                                                 PosMoves=[Move|_], update_divergence_map(Pos,Move))),
    NewDivs=Divergences,
    NewPath=[Move|Path],update_agent_state(Agent,NewPath,NewDivs),
    format("Path:~w\n\n", [NewPath]).

reduce_backtrack([], _, [], []).
reduce_backtrack([Element|T], Element, [Element], T) :- !.
reduce_backtrack([H|T], Element, [H|Before], After) :-
    reduce_backtrack(T, Element, Before, After).

parse_head([Head|Rest],Head,Rest).