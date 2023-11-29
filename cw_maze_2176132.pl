solve_maze :-
    my_agents(My_agents),
    solve_maze_dfs()

    
    [agent_move_queue(ID, [Path])]

    get_agent_position(First_agent, Initial_position),
    dfs_find_path(Initial_position, First_agent, [], []),
    agents_follow_path(OtherAgents).



Current_agent, [Available_agents], [Waiting_list]
    get_agent_position(Current_agent, Current_position),
    (findall(New_position, (
            agent_adjacent(Current_agent, New_position, empty)
        ), New_cells)

    ->  append() if there is waiting list and Available_agents, allocate available agent to waiting list traveling
    ;   if there is no more way to travel, it means it reached dead end, now the agent goes waiting list.
    if there is waiting list and Available_agents, allocate available agent to waiting list traveling
    )


% Agents_next move
[agent_move_queue(ID, [Path])]
agents_next_move() :- 
    for each agent_move_queue, find the next movement it should make. 
    needs to be called each time when the waiting list && available agent updated 


achieved() :-
leave_maze(First_agent),
when it arrived the exit, the agent reached there needs to exit.
then delete all the current move_queue.
get queue for each agent to go exit and queue the final paths