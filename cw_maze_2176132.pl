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
    format('------------------solve maze ~n'),
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
    format('Main check 0 : ~w~n', [State]),
    next_move(State, State_1, []),
    format('Main check 1 : ~w~n', [State_1]),
    State_1 = state(_, _, Available_agents_1, _, _),
    queue_waiting_list(Available_agents_1, State_1, State_2),
    format('Main check 2 : ~w~n', [State_2]),
    State_2 = state(Entities_2, Move_queue_2, Available_agents_2, Waiting_list_2, Explored_nodes_2),
    format('Move_queue before execute : ~w~n', [Move_queue_2]),
    execute_queue(Entities_2, Move_queue_2, Move_queue_2, [], [], [], [], Entities_3, Move_queue_3),
    format('Move_queue after execute  : ~w~n', [Move_queue_3]),
    evolve_state(state(Entities_3, Move_queue_3, Available_agents_2, Waiting_list_2, Explored_nodes_2),New_state).


% recursive call updating entities
next_move( state([], Move_queue, Available_agents, Waiting_list, Explored_nodes), Updated_State, Temp_entities) :-
    Updated_State = state(Temp_entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), !.
% recursive call updating entities
next_move(State, Updated_State, Temp_entities) :-
    format('------------------next_move ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), 
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes]),
    Entities = [entity(ID, Going)|Other_entities],
    get_agent_position(ID, Current_position),
    
    (   achieve(ID)
    ->  ailp_grid_size(N), Max_energy is N*N, my_agent(Agents), exit(Agents, p(N, N), Max_energy, [])
    ;   findall(path(New_position, Direction), 
            (agent_adjacent(ID, New_position, empty), 
            direction(Current_position, Direction, New_position),
            \+ member(agent_move_queue(entity(ID,_),_), Move_queue),
            \+ member(path(New_position, _), Explored_nodes),
            \+ opposite_direction(Going, Direction)
            ), New_paths),
        (   New_paths = []
        ->  (   member(agent_move_queue(entity(ID,Move_direction),_), Move_queue)
            ->  next_move(state(Other_entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), Updated_State, [entity(ID, Move_direction)|Temp_entities])
            ;   next_move(state(Other_entities, Move_queue, [ID|Available_agents], Waiting_list, Explored_nodes), Updated_State, Temp_entities)
            ) 
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
queue_waiting_list([], State, State) :- 
    format('Looked all available agents ~n').
queue_waiting_list(_, State, State) :-
    (State = state(_, _, _, [], _), 
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    format('No more waiting list ~n'),
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes]));
    (State = state(_, _, [], _, _),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    format('no more available agent ~n'),
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes])).
queue_waiting_list(Free_agents, State, Updated_State) :-
    format('------------------queue_waiting_list ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes]),
    Free_agents = [Agent | Agents],
    format('1'),
    get_agent_position(Agent, Agent_position),
    format('2'),
    Waiting_list = [path(Destination, Direction) | Waiting_list_left],
    format('3'),
    ailp_grid_size(N),
    format('4'),
    Max_energy is N * N,
    format('5'),
    (   solve_task_as(go(Destination), go(Destination), Max_energy, Max_energy, [state([Agent_position], 0)], [], [], [move_queue(_, Reversed_path)])
    ->  reverse(Reversed_path, [_ | Path]),
        format('Go Path : ~w~n', [Path]),
        select(Agent, Available_agents, Updated_agents),
        queue_waiting_list(Agents, state([entity(Agent, Direction) | Entities], [agent_move_queue(entity(Agent, Direction), Path) | Move_queue], Updated_agents, Waiting_list_left, Explored_nodes), Updated_State)
    ;   queue_waiting_list(Agents, State, Updated_State)
    ).

    %Reversed_path = [P1, P2 |_],
    %direction(P1, Move_direction, P2),
    

%execute_queue(Entities_2, Move_queue_2, [], [], [], [], Entities_3, Move_queue_3),
% translate go for one html tick then execute, update availavle agent if eligable 
execute_queue(_, _, [], Agent_list, Move_list, Entities, Move_queue, Entities, Move_queue) :-
    format('move... [Agent] : ~w  [Position] : ~w ~n', [Agent_list, Move_list]),
    agents_do_moves(Agent_list, Move_list).
execute_queue(Entities_const, Move_queue_const, Move_queue, Agent_list, Move_list, Temp_entities, Temp_move_queue, Updated_entities, Updated_move_queue) :-
    format('------------------execute_queue ~n'),
    Move_queue = [agent_move_queue(entity(ID, Direction), [Next_position|Path]) | Move_queue_left],
    get_agent_position(ID, Current_position), direction(Current_position, New_direction, Next_position),
    (   member(Next_position, Move_list)
    ->  execute_queue(Entities_const, Move_queue_const, Move_queue_left, Agent_list, Move_list, [entity(ID, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), [Next_position|Path])|Temp_move_queue], Updated_entities, Updated_move_queue)
    ;   (   lookup_pos(Next_position, empty)
        ->  (   Path = []
            ->  (   Direction = exit
                ->  format('found!!'), agent_do_moves(ID, [Next_position]), format('bye bye'), leave_maze(ID)
                ;   execute_queue(Entities_const, Move_queue_const, Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [entity(ID, New_direction)|Temp_entities], Temp_move_queue, Updated_entities, Updated_move_queue)
                )
            ;   execute_queue(Entities_const, Move_queue_const, Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [entity(ID, New_direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), Path)|Temp_move_queue], Updated_entities, Updated_move_queue)
            )
        ;   lookup_pos(Next_position, a(ID2)), member(agent_move_queue(entity(ID2, _), [Next_position2|Path2]) , Move_queue_const), get_agent_position(ID2, Current_position2), direction(Current_position2, New_direction2, Next_position2),
            (   opposite_direction(New_direction, New_direction2)
            ->  select(agent_move_queue(entity(ID2, _), _), Move_queue_left, Swaped_move_queue),
                member(entity(ID2, Direction2), Entities_const),
                execute_queue(Entities_const, Move_queue_const, Swaped_move_queue, Agent_list, Move_list, [entity(ID, Direction2), entity(ID2, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction2), Path2), agent_move_queue(entity(ID2, Direction), Path)|Temp_move_queue], Updated_entities, Updated_move_queue)
            ;   execute_queue(Entities_const, Move_queue_const, Move_queue_left, Agent_list, Move_list, [entity(ID, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), [Next_position|Path])|Temp_move_queue], Updated_entities, Updated_move_queue)
            )
        )
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
    get_agent_position(Agent, Agent_position),
    solve_task_as(go(Exit), go(Exit), Max_energy, Max_energy, [state([Agent_position], 0)], [], [], [move_queue(_, Reversed_path)]),
    reverse(Reversed_path, [_| Path]),
    exit(Agents, Exit, Max_energy, [agent_move_queue(entity(Agent, exit), Path)|Exit_queue]).
