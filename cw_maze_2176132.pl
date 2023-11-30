m(north). 
m(east).
m(south). 
m(west).
m(exit).

direction(p(X,Y), west, p(X1,Y)) :- X1 is X-1.
direction(p(X,Y), east, p(X1,Y)) :- X1 is X+1.
direction(p(X,Y), north, p(X,Y1)) :- Y1 is Y-1.
direction(p(X,Y), south, p(X,Y1)) :- Y1 is Y+1.

back(east, west).
back(south, north).
back(D1, D2) :- back(D2, D1).


% initial call
solve_maze :-
    my_agents([Agent|My_agents]),
    evolve_state([entity(Agent, south)], state([], My_agents, [], []), _).


% main loop
evolve_state(Entities, State, New_state) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    next_move(Entities, State, State_1, Entities_1),
    queue_waiting_list(Entities_1, State_1, State_2, Entities_2),
    State_2 = state(Move_queue_2, Available_agents_2, Waiting_list_2, Exploered_nodes_2),
    execute_queue(Move_queue_2, [], [], Move_queue_3),
    evolve_state(Entities_2, state(Move_queue_3, Available_agents_2, Waiting_list_2, Exploered_nodes_2),New_state).


% recursive call updating entities
next_move([], State, State, []).
next_move([entity(ID, Going)|Entities], State, Updated_State, Updated_entities) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    get_agent_position(ID, Current_position),
    (   achieved(ID)
    ->  Max_energy is N*N, my_agent(Agents), exit(Agents, Position, Max_energy, [])
    ;   findall(path(New_position, Direction), 
        (agent_adjacent(ID, New_position, empty), 
        direction(Current_position, Direction, New_position),
        \+ back(Going,Direction)),
        \+ member(path(New_position, _), Exploered_nodes),
        New_paths),
        (  New_paths = [path(Path_position, Path_direction)]
        ->  next_move(Entities, state([agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, Waiting_list, Exploered_nodes), Updated_State, [entity(ID, Path_direction)|Updated_entities]) 
        ;   (   New_paths = []
            ->  next_move(Entities, state(Move_queue, [ID|Available_agents], Waiting_list, Exploered_nodes), Updated_State, Updated_entities) 
            ;   New_paths = [path(Path_position, Path_direction)| Other_paths],
            select(path(Path_position, Path_direction), New_paths, Updated_paths),
            append(Updated_paths, Waiting_list, New_waiting_list),
            append(Updated_paths, Exploered_nodes, New_Exploered_nodes),
            next_move(Entities,  state([agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, Updated_waiting_list, Updated_Exploered_nodes]), Updated_State, [entity(ID, Path_direction)|Updated_entities]
            )
        )
    ).




% if there is any available agents and if there is any nodes in waiting list, start exploring that node
queue_waiting_list(Entities, State, State, Entities) :-
    State = state(_, [], Waiting_list, _).
queue_waiting_list(Entities, State, Updated_State, Updated_entities) :-
    State = state(_, Available_agents, Waiting_list, Explored_nodes),
    Available_agents = [First_agent | Other_agents],
    Waiting_list = [path(Position, Direction) | Other_waiting_list],
    ailp_grid_size(N),
    Max_energy is N * N,
    heuristic(Position, go(Position), H),
    solve_task_as(go(Position), go(Position), Max_energy, Max_energy, [state([Position], H)], [], [], [move_queue(_, Reversed_path)]),
    reverse(Reversed_path, [_ | Path]),
    queue_waiting_list(Entities, state([agent_move_queue(entity(First_agent, Direction), [Path]) | _], Other_agents, Other_waiting_list, [Position | Explored_nodes]), Updated_State, [entity(First_agent, Direction) | Entities]).


% translate go for one html tick then execute, update availavle agent if eligable 
execute_queue([], Agent_list, Move_list, []) :-
    agents_do_moves(Agent_list, Move_list).
execute_queue(Move_queue, Agent_list, Move_list, Updated_move_queue) :-
    Move_queue = [agent_move_queue(entity(ID, Direction), [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  (   Direction = exit
        ->   leave_maze(ID)
        ;   execute_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Updated_move_queue)
        )
    ;   execute_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(entity(ID, Direction), [Path])|Updated_move_queue])
    ).


% once an agent found an exit, the maze is solved and other agents go exit stopping exploring
achieved(Agent) :-
    get_agent_position(Agent, Position),
    ailp_grid_size(N),
    Position = p(N,N),
    leave_maze(Agent).


exit([], _, _, []).
exit([], _, _, Exit_queue) :-
    execute_queue(Exit_queue, [], [], Updated_exit_queue),
    exit([], _, _, Updated_exit_queue).
exit([Agent|Agents], Exit, Max_energy, Exit_queue) :-
    get_agent_position(Agent, Position),
    heuristic(Position, go(Position), H),
    solve_task_as(go(Position), go(Position), Max_energy, Max_energy, [state([Position], H)], [], [], [move_queue(_, Reversed_path)]),
    reverse(Reversed_path, [_| Path]),
    exit(Agents, Exit, Max_energy, [agent_move_queue(entity(Agent, exit), [Path])|Exit_queue]).