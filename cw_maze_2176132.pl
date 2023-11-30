m(north). 
m(east).
m(south). 
m(west).
m(exit).

direction(p(X,Y), west, p(X1,Y)) :- X1 is X-1.
direction(p(X,Y), east, p(X1,Y)) :- X1 is X+1.
direction(p(X,Y), north, p(X,Y1)) :- Y1 is Y-1.
direction(p(X,Y), south, p(X,Y1)) :- Y1 is Y+1.

opposite_direction(north, south).
opposite_direction(south, north).
opposite_direction(east, west).
opposite_direction(west, east).

% initial call
solve_maze :-
    format('------------------solve maze------------------- ~n'),
    my_agents(My_agents),
    format('my_agents : ~w~n', [My_agents]),
    initialise_entities(My_agents, [], [], Agents, Entities),
    evolve_state(state(Entities, [], Agents, [], []), _).


initialise_entities([], Agents, Entities, Agents, Entities) :-
    format('finished').
initialise_entities([Agent|Agents], Temp_agents, Temp_entities, Updated_agents, Updated_entities) :-
    get_agent_position(Agent, Current_position),
    format('1'),
    findall(
        path(Pos, Direction), 
        (
            agent_adjacent(Agent, Pos, empty), 
            direction(Current_position, Direction, Pos)
        ), 
        New_paths
    ),
    format('2'),
    (
        New_paths = []
        ->  format('3'), initialise_entities(Agents, [Agent|Temp_agents], Temp_entities, Updated_agents, Updated_entities)
        ;   New_paths = [path(_, D)|_], format('4~n'),
            initialise_entities(Agents, Temp_agents, [entity(Agent, D)|Temp_entities], Updated_agents, Updated_entities)
    ).


% main loop
evolve_state(State, New_state) :-
    %State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    next_move(State, State_1, []),
    queue_waiting_list(State_1, State_2),
    State_2 = state(Entities_2, Move_queue_2, Available_agents_2, Waiting_list_2, Explored_nodes_2),
    execute_queue(Move_queue_2, [], [], [], Move_queue_3),
    evolve_state(state(Entities_2, Move_queue_3, Available_agents_2, Waiting_list_2, Explored_nodes_2),New_state).


% recursive call updating entities
next_move( state([], Move_queue, Available_agents, Waiting_list, Explored_nodes), Updated_State, Temp_entities) :-
    Updated_State = state(Temp_entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), !.
% recursive call updating entities
next_move(State, Updated_State, Temp_entities) :-
    format('------------------next_move------------------- ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), 
    Entities = [entity(ID, Going)|Other_entities],
    get_agent_position(ID, Current_position),
    
    (   achieve(ID)
    ->  Max_energy is N*N, my_agent(Agents), exit(Agents, p(N, N), Max_energy, [])
    ;   findall(path(New_position, Direction), 
            (agent_adjacent(ID, New_position, empty), 
            direction(Current_position, Direction, New_position),
            \+ member(path(New_position, _), Explored_nodes),
            \+ opposite_direction(Going, Direction)
            ), New_paths),
        (   New_paths = []
        ->  next_move(state(Other_entities, Move_queue, [ID|Available_agents], Waiting_list, Explored_nodes), Updated_State, Temp_entities) 
        ;   (   New_paths = [path(Path_position, Path_direction)]
            ->  next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, Waiting_list, Explored_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities]) 
            ;   New_paths = [path(Path_position, Path_direction)| Other_paths],
                append(Other_paths, Waiting_list, New_waiting_list),
                append(Other_paths, Explored_nodes, New_Explored_nodes),
                next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, New_waiting_list, New_Explored_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities])
            )
        )
    ).

% queue_waiting_list(State_1, State_2),
% if there is any available agents and if there is any nodes in waiting list, start exploring that node
queue_waiting_list(State, Updated_State) :-
    State = state(_, _, _, [], _), Updated_State = State;
    State = state(_, _, [], _, _), Updated_State = State.
queue_waiting_list(State, Updated_State) :-
    format('------------------queue_waiting_list------------------- ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    Available_agents = [First_agent | Other_agents],
    Waiting_list = [path(Position, Direction) | Other_waiting_list],
    ailp_grid_size(N),
    Max_energy is N * N,
    heuristic(Position, go(Position), H),
    solve_task_as(go(Position), go(Position), Max_energy, Max_energy, [state([Position], H)], [], [], [move_queue(_, Reversed_path)]),
    reverse(Reversed_path, [_ | Path]),
    queue_waiting_list(state([entity(First_agent, Direction) | Entities], [agent_move_queue(entity(First_agent, Direction), [Path]) | Move_queue], Other_agents, Other_waiting_list, Explored_nodes), Updated_State).


% translate go for one html tick then execute, update availavle agent if eligable 
execute_queue([], Agent_list, Move_list, Move_queue, Move_queue) :-
    agents_do_moves(Agent_list, Move_list).
execute_queue(Move_queue, Agent_list, Move_list, Temp_move_queue, Updated_move_queue) :-
        format('------------------execute_queue------------------- ~n'),
    Move_queue = [agent_move_queue(entity(ID, Direction), [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  (   Direction = exit
        ->   leave_maze(ID)
        ;   (   lookup_pos(Next_position, empty)
            ->  execute_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Temp_move_queue, Updated_move_queue)
            ;   execute_queue(Move_queue_left, Agent_list, Move_list, [agent_move_queue(entity(ID, Direction), [Next_position|Path])|Temp_move_queue], Updated_move_queue)
            )
        )
    ;   execute_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(entity(ID, Direction), [Path])|Temp_move_queue], Updated_move_queue)
    ).


% once an agent found an exit, the maze is solved and other agents go exit stopping exploring
achieve(Agent) :-
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