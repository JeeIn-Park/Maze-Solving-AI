Move_queue = [agent_move_queue(ID, [Path])]

% initial call
solve_maze :-
    my_agents(My_agents),
    %reverse(My_agents, My_Agents),
    My_agents = [First_agent | Other_agents],
    solve_maze_dfs(My_Agents, Other_agents, []).


% main loop
solve_maze_dfs(Current_agent, Available_agents, Waiting_list, Move_queue,) :-
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
        if there is waiting list and Available_agents, allocate available agent to waiting list traveling
    )

% calculate queue and add it to move queue, [agent_move_queue(ID, [Path])]
% need to get current move queue as well, return the updated move queue.
% if there is waiting list and if there is Available_agents, allocate available agent to waiting list traveling
queue_waiting_list([], _, Move_queue, [], Move_queue).
queue_waiting_list(Waiting_list, [], Move_queue, Waiting_list, Move_queue). 
queue_waiting_list(Waiting_list, Available_agents, Move_queue, Updated_waiting_list, Updated_move_queue) :-
    Waiting_list = [Most_recent_found | Waiting_list_left],
    Available_agents = [First_agent | Other_agents],
    calculate queue for the agent to go to the cell on the waiting list


% Agents_next move
% from current move queue, calculate next move for all agent and execute it
% after it execute the move, calculate the next state on the main loop
[agent_move_queue(ID, [Path])]
agents_next_move(Move_queue) :- 
    for each agent_move_queue, find the next movement it should make. 
    needs to be called each time when the waiting list && available agent updated 
when it finish the move allocated, it should be free and added to the available agent



achieved() :-
leave_maze(First_agent),
when it arrived the exit, the agent reached there needs to exit.
then delete all the current move_queue.
get queue for each agent to go exit and queue the final paths