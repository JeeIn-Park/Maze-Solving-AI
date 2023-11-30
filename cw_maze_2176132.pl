m(north). 
m(east).
m(south). 
m(west).

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
    for all entity (next_move)
    append(Free_agents, Available_agents, Arrived, New_available_agent)
    execute_move_queue
    evolve_state.


% recursive call updating entities
next_move(entity(ID, Going), State, Updated_State) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    get_agent_position(ID, Current_position),
    findall(path(New_position, Direction), 
        (agent_adjacent(ID, New_position, empty), 
        direction(Current_position, Direction, New_position),
        \+ back(Going,Direction)),
        New_paths),
    (  New_paths = [path(Path_position, _)]
    ->  Updated_State = state([agent_move_queue(entity(ID, Going), [Path_position])|Move_queue], Available_agents, Waiting_list, Exploered_nodes)
    ;   (   New_paths = []
        ->  Updated_State = state(Move_queue, [ID|Available_agents], Waiting_list, Exploered_nodes)
        ;   member(path(Step, Going), New_paths),
            Updated_State = state([agent_move_queue(entity(ID, Going), [Step])|Move_queue], Available_agents, Updated_waiting_list, Exploered_nodes),
            select(path(Step, Going), New_paths, _),
            append(New_paths, Waiting_list, Updated_waiting_list)
        )
    ).



Explored_node calculator
% when it has more than two adjacent empty cells, it's considered as node. 
% it adds adjacent cells to waiting lists when it hasn't been explored.
% after add adjacent cells to the waiting list, add the node to explored_node.

queue_waiting_list(Entities, State, State) :-
    State = state(Move_queue, [], Waiting_list, Exploered_nodes);
    State = state(Move_queue, Available_agents, [], Exploered_nodes).
queue_waiting_list(Entities, State, Updated_State)
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_nodes),
    Waiting_list = [path(Position | Direction) | Other_waiting_list],
    Available_agents = [First_agent | Other_agents],
    ailp_grid_size(N),
    Max_energy is N*N,
    heuristic(Position, go(Position), H),
    solve_task_as(go(Position), go(Position), Max_energy, Max_energy, [state([Position], H)], [], [], [move_queue(_, Reversed_path)]),
    reverse(Reversed_path, [_| Path]),
    queue_waiting_list([entity(First_agent, Direction)|Entities], 
        state([agent_move_queue(First_agent, [Path])|Move_queue], Other_agents, Other_waiting_list, [Position|Exploered_nodes]), Updated_State).


% calculate queue and add it to move queue, [agent_move_queue(ID, [Path])]
% need to get current move queue as well, return the updated move queue.
% translate move for one html tick then execute
execute_explore_queue([], Agent_list, Move_list, []) :-
    agents_do_moves(Agent_list, Move_list).
execute_explore_queue(Move_queue, Agent_list, Move_list, Updated_move_queue) :-
    Move_queue = [agent_move_queue(entity(ID, _), [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  execute_explore_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Updated_move_queue, )
    ;   execute_explore_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(ID, [Path])|Updated_move_queue])
    ).


% translate go for one html tick then execute, update availavle agent if eligable 
execute_go_queue([], Agent_list, Move_list, [], []) :-
    agents_do_moves(Agent_list, Move_list).
execute_go_queue(Move_queue, Agent_list, Move_list, Updated_move_queue, Free_agents) :-
    Move_queue = [agent_move_queue(ID, [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  execute_go_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Updated_move_queue, [ID|Free_agents])
    ;   execute_go_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(ID, [Path])|Updated_move_queue], Free_agents)
    ).



achieved(Agent, Agents) :-
    get_agent_position(Agent, Position),
    ailp_grid_size(N),
    Position = p(N,N),
    exit(Agents, Position),
    leave_maze(First_agent).



exit([]).
exit([Agent|Agents]) :-
    Agent, go, Position
    %execute_go_queue ----> if time, do it with move_queue
    leave leave_maze
    exit Agents
   