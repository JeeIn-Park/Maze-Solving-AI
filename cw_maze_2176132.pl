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
    format('------------------solve maze------------------- ~n'),
    my_agents(My_agents),
    My_agents = [Agent|Agents],
    format('my_agents : ~w~n', [My_agents]),
    evolve_state(state([entity(Agent, south)], [], Agents, [], []), _).


% main loop
evolve_state(State, New_state) :-
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    format('Current state : ~w', [Entities]),
    format(', ~w', [Move_queue]),
    format(', ~w', [Available_agents]),
    format(', ~w', [Waiting_list]),
    format(', ~w~n', [Exploered_nodes]),
    %%%%%%%%%%%%%%%%%%%%
    next_move(State, State_1, []),
    format('next_move ~n'),
    queue_waiting_list(State_1, State_2),
    format('queue_waiting_list ~n'),
    State_2 = state(Entities_2, Move_queue_2, Available_agents_2, Waiting_list_2, Exploered_nodes_2),
    execute_queue(Move_queue_2, [], [], Move_queue_3),
    format('execute_queue ~n'),
    evolve_state(state(Entities_2, Move_queue_3, Available_agents_2, Waiting_list_2, Exploered_nodes_2),New_state).


% recursive call updating entities
next_move(State, Updated_State, Temp_entities) :-
    State = state([], Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    Updated_State = state(Temp_entities, Move_queue, Available_agents, Waiting_list, Exploered_nodes).
next_move(State, Updated_State) :-
    format('------------------next_move------------------- ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    Entities = [entity(ID, Going)|Other_entities],
    format('next_move entity : ~w', ID),
    format(', left entities : ~w', [Other_entities]),
    format(', entity direction : ~w~n', Going),
    format('next_move state : ~w', [Move_queue]),
    format(', ~w', [Available_agents]),
    format(', ~w', [Waiting_list]),
    format(', ~w~n', [Exploered_nodes]),
    get_agent_position(ID, Current_position),
    format('Current position : ~w~n', Current_position),
    (   achieve(ID)
    ->  format('achieved ~n'),
        Max_energy is N*N, my_agent(Agents), exit(Agents, Position, Max_energy, [])
    ;   format('findall ~n'), ( findall(path(New_position, Direction), 
            (agent_adjacent(ID, New_position, empty), 
            direction(Current_position, Direction, New_position)),
            New_paths) %% need to add more conditions,,, avoiding loop
        -> (   New_paths = [path(Path_position, Path_direction)]
            ->  format('New_paths : ~w~n', [New_paths]),
                format('single path ~n'), 
                next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, Waiting_list, Exploered_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities]) 
            ;   format('New_paths : ~w~n', [New_paths]),
                format('found node ~n'), New_paths = [path(Path_position, Path_direction)| Other_paths],
                select(path(Path_position, Path_direction), New_paths, Updated_paths),
                format('add waiting list : ~w~n', [Updated_paths]),
                append(Updated_paths, Waiting_list, New_waiting_list),
                append(Updated_paths, Exploered_nodes, New_Exploered_nodes),
                format('New_waiting_list : ~w~n', [New_waiting_list]),
                format('New_Exploered_nodes : ~w~n', [New_Exploered_nodes]),
                next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, New_waiting_list, New_Exploered_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities])
            )
        ;    next_move(state(Other_entities, Move_queue, [ID|Available_agents], Waiting_list, Exploered_nodes), Updated_State, Temp_entities) 
        )
    ).


% if there is any available agents and if there is any nodes in waiting list, start exploring that node
queue_waiting_list(Entities, State, State, Entities) :-
    State = state(_, [], Waiting_list, _).
queue_waiting_list(Entities, State, Updated_State, Updated_entities) :-
    State = state(Move_queue, Available_agents, Waiting_list, Explored_nodes),
    format('queue_waiting_list entities : ~w~n', [Entities]),
    format('queue_waiting_list state : ~w', [Move_queue]),
    format(', ~w', [Available_agents]),
    format(', ~w', [Waiting_list]),
    format(', ~w~n', [Exploered_nodes]),
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