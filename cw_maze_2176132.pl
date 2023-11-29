m(north). 
m(east).
m(south). 
m(west).
m(none).

direction(p(X,Y), west, p(X1,Y)) :- X1 is X-1.
direction(p(X,Y), east, p(X1,Y)) :- X1 is X+1.
direction(p(X,Y), north, p(X,Y1)) :- Y1 is Y-1.
direction(p(X,Y), south, p(X,Y1)) :- Y1 is Y+1.

turn(east, south, clockwise).
turn(south, west, clockwise).
turn(west, north, clockwise).
turn(north, east, clockwise).
turn(D1, D2, anti_clockwise) :- turn(D2, D1, clockwise).

back(east, west).
back(south, north).
back(D1, D2) :- back(D2, D1).

% initial call
solve_maze :-
    my_agents(My_agents),
    initialise_agents(My_agents, Entities)
    evolve_state(Entities, state([], My_agents, [], []), _).


% initialise agents
initialise_agents([], []).
initialise_agents([Agent|Agents], Entities) :-
    initialise_agents(Agents, [entity(Agent, none)|Entities]).


% main loop
evolve_state(Entities, State, New_state) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_node),

    append(Free_agents, Available_agents, Arrived, New_available_agent)

for all entity (next_move)
    execute_move_queue.

Move calculator 
% if there is waiting list and Available_agents, allocate available agent to waiting list traveling

Available_agents calculator
% after move finished, when initial queue was longer than 1(go or find command)
% maybe I can use single position move only for translator and use another function for go command

Explored_node calculator
% when it has more than two adjacent empty cells, it's considered as node. 
% it adds adjacent cells to waiting lists when it hasn't been explored.
% after add adjacent cells to the waiting list, add the node to explored_node.


next_move(Entity, State, Updated_State) :-
    State = state(Move_queue, Available_agents, Waiting_list, Exploered_node),
    get_agent_position(Entity(ID, Going), Current_position),
    findall(path(New_position, Direction), 
        (agent_adjacent(ID, New_position, empty), 
        direction(Current_position, Direction, New_position),
        \+ back(Going,Direction)),
        New_paths),
    (  New_paths = [path(Queue_position, _)]
    ->  Updated_State = state([agent_move_queue(Entity, [Queue_position])|Move_queue], Available_agents, Waiting_list, Exploered_node)
    ;   (   New_paths = []
        ->  Updated_State = state(Move_queue, [ID|Available_agents], Waiting_list, Exploered_node)
        ;   append(New_paths, Waiting_list, Updated_waiting_list),
            Updated_State = state(Move_queue, Available_agents, Updated_waiting_list, Exploered_node)
         % needs to update move_queue
        )
    ).



% translate agent_move_queue for one html tick then execute
execute_move_queue([], Agent_list, Move_list, [], []) :-
    agents_do_moves(Agent_list, Move_list).
execute_move_queue(Move_queue, Agent_list, Move_list, Updated_move_queue, Free_agents) :-
    Move_queue = [agent_move_queue(ID, [Next_position|Path]) | Move_queue_left],
    (   Path = []
    ->  execute_move_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], Updated_move_queue, [ID|Free_agents])
    ;   execute_move_queue(Move_queue_left, [ID|Agent_list], [Next_position|Move_list], [agent_move_queue(ID, [Path])|Updated_move_queue], Free_agents)
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



achieved
when it arrived the exit, the agent reached there needs to exit.
then delete all the current move_queue.
get queue for each agent to go exit and queue the final paths
achieved(Agent, Agents) :-
get_agent_position(Agent, Position),
ailp_grid_size(N),
Position = p(N,N),
exit(Agents, Position),
leave_maze(First_agent).



exit([]).
exit([Agent|Agents]) :-
    Agent, go, Position
    leave leave_maze
    exit Agents
    % if time, do it with move_queue



% Agents_next move
% from current move queue, calculate next move for all agent and execute it
% after it execute the move, calculate the next state on the main loop
[agent_move_queue(ID, [Path])]
agents_next_move(Move_queue) :- 
    for each agent_move_queue, find the next movement it should make. 
    needs to be called each time when the waiting list && available agent updated 
when it finish the move allocated, it should be free and added to the available agent