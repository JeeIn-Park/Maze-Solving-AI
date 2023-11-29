% initial call
solve_maze :-
    my_agents(My_agents),
    %reverse(My_agents, My_Agents),
    My_agents = [First_agent | Other_agents],
    solve_maze_dfs(My_Agents, Other_agents, []).



% main loop
solve_maze_dfs(State, Updated_state) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_node),


    get_agent_position(Current_agent, Current_position),
    (findall(New_position, (
            agent_adjacent(Current_agent, New_position, empty)
        ), New_cells)
    ->  append(New_cells, Waiting_list, New_waiting_list),
        New_waiting_list = [Most_recent_found | Waiting_list_left],
        % needs to pass this queue somewhere, maybe store it and pass it to another function at the end? agent_move_queue(Current_agent, [Most_recent_found]),
        queue_waiting_list(Waiting_list_left, Available_agents, Move_queue, Updated_waiting_list, Updated_move_queue),
        agents_next_move(Updated_move_queue, Next_move_queue),
        solve_maze_dfs(Current_agent, Available_agents, Updated_waiting_list, Next_move_queue)
    ;   
        queue_waiting_list(Waiting_list_left, Available_agents, Move_queue, Updated_waiting_list, Updated_move_queue),
        agents_next_move(Updated_move_queue, Next_move_queue),
        solve_maze_dfs(Current_agent, Available_agents, Updated_waiting_list, Next_move_queue)
        if there is no more way to travel, it means it reached dead end, now the agent goes waiting list.
        set another agent as a main agent... 
        for all agents, needs to check adjacent cells.
        if there is waiting list and Available_agents, allocate available agent to waiting list traveling
    )


% translate agent_move_queue for one tick then move
calculate_move_agents([], Agent_list, Move_list, [], []) :-
    agents_do_moves(Agent_list, Move_list).
calculate_move_agents(Move_queue, Agent_list, Move_list, Updated_move_queue, Free_agents) :-
    Move_queue = [agent_move_queue(ID, [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  calculate_move_agents(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Updated_move_queue, [ID|Free_agents])
    ;   calculate_move_agents(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(ID, [Path])|Updated_move_queue], Free_agents)
    ).



% calculate queue and add it to move queue, [agent_move_queue(ID, [Path])]
% need to get current move queue as well, return the updated move queue.
% if there is waiting list and if there is Available_agents, allocate available agent to waiting list traveling
queue_waiting_list([], _, Move_queue, [], Move_queue).
queue_waiting_list(Waiting_list, [], Move_queue, Waiting_list, Move_queue). 
queue_waiting_list(Waiting_list, Available_agents, Move_queue, Updated_waiting_list, Updated_move_queue) :-
    Waiting_list = [Most_recent_found | Waiting_list_left],
    Available_agents = [First_agent | Other_agents],
    calculate queue for the agent to go to the cell on the waiting list



achieved(Agent) :-
get_agent_position(Agent, Position),
ailp_grid_size(N),
Position = p(N,N),
leave_maze(First_agent).