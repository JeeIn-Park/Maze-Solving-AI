% Accomplish a given task and return the cost
% solve_task(+Original_task, -Cost)
solve_task(Original_task,Cost) :-
    % check the agent for the task and get the initial state
    my_agent(A), 
    get_agent_position(A, P),
    get_agent_energy(A, E),
    ailp_grid_size(N),
    % calculate value needed to caculate the path for the task
    heuristic(P, Original_task, H),
    X is ( N * N / 4 ), ceiling(X, Max_energy),
    % New_energy is E - 10, need to deal with query energy
    format('solve task : ~w~n', Original_task),
    solve_task_as(Original_task, Original_task, E, Max_energy, [state([P], H)], [], [], Move_queue), 
    agent_do_move_queue(A, Move_queue, 0, Cost).


% Calculate the path required to achieve a Task
% solve_task_as(+Final task, +Current Task, +Input energy, +Queue(path), +Visited node, +Current_move_queue, -Move_queue)
solve_task_as(Original_task, Task, E, Max_energy, Path, Visited_node, Current_move_queue, Move_queue) :-
    Path = [state(Current_path, _) | Other_states],
    Current_path = [Current_Position|_],
    length(Current_path, Estimate_energy_consumption),
    Estimate_energy is E - Estimate_energy_consumption + 1,

    (Estimate_energy < 0 % check if it runs out of energy
    ->  (Task = find(c(_))
        ->  !, fail % run out of energy, fail to find the nearest charging station
        ;   reverse(Current_path, [Search_starting_position|_]), % run out of energy, try to find the nearest charging station
            solve_task_as(Original_task, find(c(_)), E, Max_energy, [state([Search_starting_position], 0)], [], Current_move_queue, Move_queue))
            
    ;  (achieved(Task, Current_Position), format('achieved : ~w~n', Task)% when it achieved the current goal without running out of energy
        ->  (task_achieved(Original_task, Current_Position) % check if the final goal is achieved 
            ->  !, reverse([move_queue(Task, Current_path) | Current_move_queue], Move_queue)
            ;   Current_path = [New_starting_position|_], % if it was a subgoal, start a search again from the last location
                heuristic(New_starting_position, Original_task, H),
                solve_task_as(Original_task, Original_task, Max_energy, Max_energy, [state([New_starting_position], H)], [], [move_queue(Task, Current_path) | Current_move_queue], Move_queue)
            )
            
        ;   format('finding way ~n'),
            (findall(state(New_state_element, F), ( % if it didn't achieve the goal, continue finding a path for the current goal
                    New_state_element = [New_position|Current_path],
                    map_adjacent(Current_Position, New_position, empty),
                    heuristic(New_position, Task, H),
                    length(Current_path, G),
                    F is H + G,
                    \+ member(New_position, Visited_node),
                    \+ member(state([New_position|_], _), Path)
                ), New_states) 
            ->  (merge_and_sort_by_heuristic(Other_states, New_states, Priority_queue),
                solve_task_as(Original_task, Task, E, Max_energy, Priority_queue, [Current_Position|Visited_node], Current_move_queue, Move_queue))
            ;   solve_task_as(Original_task, Task, E, Max_energy, Other_states, Visited_node, Current_move_queue, Move_queue)
            )
        )
    ).



%heuristic(+Next_position, +Task, -Heuristic)
heuristic(Next_position, Task, Heuristic) :-
    ( Task = go(Goal_position), ground(Goal_position), !, map_distance(Next_position, Goal_position, Heuristic)
    ; Heuristic = 0 ).


% merge_and_sort_by_heuristic(+Other_states,+New_states,-Priority_queue)
merge_and_sort_by_heuristic(Path,[], Path).
merge_and_sort_by_heuristic(Path, [Next | Rest], Priority_queue) :-
    insert(Path, Next, Updated),
    merge_and_sort_by_heuristic(Updated, Rest, Priority_queue).


% insert(+Path,+Next,-Priority_queue)
insert([], State, [State]).
insert([state(Path, H) | Rest], state(New_path, NH), Priority_queue) :-
    (H >= NH 
    ->  Priority_queue = [state(New_path, NH), state(Path, H) | Rest]
    ;   insert(Rest, state(New_path, NH), Result),
        Priority_queue = [state(Path, H)|Result]).


% achieved(+Task,+Pos)
achieved(go(Pos), Pos).
achieved(find(Obj), Pos) :-
    map_adjacent(Pos, _, Obj),
    ( Obj = o(ID) 
    ->  my_agent(A), \+ agent_check_oracle(A, o(ID)),  format('already visited by: ~w~n', A)
    ;   true 
    ).


% task_achieved(+Original_task, +Destination)
task_achieved(Original_task, Destination) :- 
    Original_task = find(Obj), map_adjacent(Destination,_,Obj)
    ;   Original_task = go(Destination).


% agent_do_move_queue(+A, +Move_queue, +Current_cost, -Cost).
agent_do_move_queue(_, [], Current_cost, Cost) :-
    Cost = Current_cost.
agent_do_move_queue(A, [Current_move|Rest_move_queue], Current_cost, Cost) :-
    Current_move = move_queue(Task, Reversed_path),
    reverse(Reversed_path, [_| Path]),
    length(Reversed_path, New_cost),
    agent_do_moves(A, Path),
    (Task = find(c(N)) 
    ->  agent_topup_energy(A, c(N)),
        New_current_cost is Current_cost + New_cost,
        agent_do_move_queue(A, Rest_move_queue, New_current_cost, Cost)
    ;   New_current_cost is Current_cost + New_cost,
        agent_do_move_queue(A, Rest_move_queue, New_current_cost, Cost)).