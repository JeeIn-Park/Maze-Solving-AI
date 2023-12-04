% State: Saves Path, Backtrack, Move info
% 1. solve_maze: initialize agent state
% 2. solve_maze(_,_) : find_moves -> returns NewState / Moves / New DivergenceMap
%  -> executes moves/ (Recursive)
% 3. find_moves(recursive) -> find_moves_dfs -> returns NewState
% 4. find_moves_dfs -> select_move
% 5. select_move -> a. Backtrack : follow the move (moves = 0)
%                -> b. multiple moves: pick one (moves >= 2)
%                -> c. finished Backtrack : update DivergenceMap, follow the path that didnt take

% Path stores only the right route (keeps elimnating dead ends)
% Backtrack is a path storing from backtracking position to the original divergence
% Divergences stores right divergences that the agent encounters

:- dynamic agent_state/4. % agent_state(Agent, Path, Backtrack)
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
    retractall(agent_state(Agent,_,_,_)),
    get_agent_position(Agent,Pos),
    assert(agent_state(Agent,[Pos],[],[])).

get_agent_state(Agent,Path,Divergences,Dead) :-
    agent_state(Agent,Path,Divergences,Dead).

update_agent_state(Agent,Path,Divergences,Dead) :-
    retractall(agent_state(Agent,_,_,_)), % Remove the existing state
    assert(agent_state(Agent,Path,Divergences,Dead)). % Assert the new state

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
    get_agent_state(Agent,Path,Divergences,Dead),
    (leave_maze(Agent) -> true
    ;otherwise         -> find_moves_dfs(Agent,Path,Divergences,FinalMove,Dead), Move=FinalMove,
                          find_moves(Rest,Moves)).

% find_moves_dfs(+Agent,+Path,+Backtrack,-Move)
find_moves_dfs(Agent,Path,Divergences,FinalMove,Dead) :-
    get_agent_position(Agent,Pos),
    findall(Next, ((map_adjacent(Pos,Next,empty);map_adjacent(Pos,Next,a(_))), \+ member(Next,Path), \+ is_dead_move(Next)), PosMoves), % \+ agent occupying
    % findall(P, (member(P,PosMoves), agent_adjacent(Agent,P,empty)),ActualMoves),
    format("\nBefore call::: Agent: ~w, In:~w, PosMoves:~w\n",[Agent,Pos,PosMoves]),
    format("Agent in : ~w, PosMoves: ~w, Divergence: ~w\n",[Pos,PosMoves,Divergences]),
    choose_move(Agent,Pos,PosMoves,Path,Divergences,Dead,Move),
    (my_agents(As), findall(Ag,(member(Ag,As), get_agent_position(Ag,P),P==Move),Conflict),
    \+ length(Conflict,0) -> FinalMove = Pos, get_agent_state(Agent,Updated,Divs,Ds), Updated=[_|Rest],
                            update_agent_state(Agent,Rest,Divs,Ds), format("AGENT: ~w conflicts with Agent:~w, Intended:~w\n", [Agent,Conflict,Move])
                                % If the move conflicts with agent position, cancel the move
    ;otherwise            -> FinalMove=Move, format("AGENT: Moved to Move:~w, Path:~w\n",[FinalMove, Path])).

choose_move(Agent,Pos,[],Path,Divergences,Dead,Move) :-
    update_dead_moves(Pos), % Global Dead positions that agents share
    NewDead=[Pos|Dead], % Local Dead positions that each agents have
    findall(Next,
            ((map_adjacent(Pos,Next,empty);map_adjacent(Pos,Next,a(_))), \+ is_dead_move(Next)),
            Backtracking),
    (length(Backtracking,0) ->  findall(P,
                                ((map_adjacent(Pos,P,empty);map_adjacent(Pos,P,a(_))), \+ member(P,Dead)),
                                OnlyMoves),
                                OnlyMoves=[Move], NewPath=[Move|Path]
                                 % If other agent has already blocked the backtracking path, backtrack via the path that the agent has not gone through
    ;otherwise              -> Backtracking=[Move], NewPath=[Move|Path]),
    (member(Pos,Divergences) -> Divergences = [_|Rest], NewDivs= Rest,
                                update_divergence_map(Pos,Move) % Sets the divergence point to the previous divergence (Since the divergence itself is a dead end)
    ;otherwise               -> NewDivs=Divergences),
    update_agent_state(Agent,NewPath,NewDivs,NewDead).

% choose_move(+Pos,+PosMoves,+Path,+Backtrack,-Move)
choose_move(Agent,Pos,PosMoves,Path,Divergences,Dead,Move) :-
    (length(PosMoves,1) -> PosMoves=[Move],NewDivs=Divergences,format("F\n") % Follow the move if there is only single possible move
    ;otherwise          -> (get_divergence_map(Pos,RightPath),
                            \+is_dead_move(RightPath)            -> random_member(Move,PosMoves),
                                                                    NewDivs=Divergences,format("G\n")
                            ;otherwise                           -> (\+get_divergence_map(Pos,_) -> NewDivs=[Pos|Divergences]
                                                                    ;otherwise                   -> NewDivs=Divergences),
                                                                     PosMoves=[Move|_], update_divergence_map(Pos,Move))),
    NewPath=[Move|Path], NewDead=Dead, update_agent_state(Agent,NewPath,NewDivs,NewDead).

reduce_backtrack([], _, [], []).
reduce_backtrack([Element|T], Element, [Element], T) :- !.
reduce_backtrack([H|T], Element, [H|Before], After) :-
    reduce_backtrack(T, Element, Before, After).

parse_head([Head|Rest],Head,Rest).