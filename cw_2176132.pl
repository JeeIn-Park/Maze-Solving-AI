% Accomplish a given Task and return the Cost
solve_task(Original_task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    heuristic(P, Original_task, H),
    get_agent_energy(A, E),
    % New_energy is E - 10, need to deal with query energy
    solve_task_as(Original_task, Original_task, A, E, [state([P], H)], [], [], Move_queue), 

    format('Ready for movement  : ~w~n', [Move_queue]),
    agent_do_move_queue(A, Move_queue, 0, Cost).


% Calculate the path required to achieve a Task
solve_task_as(Original_task, Task, A, E, Queue, Visited_node, Current_move_queue, Move_queue) :-
    Queue = [state(Current_path, _)| Other_states],
    Current_path = [Current_Position|_],
    format('Current_Position    : ~w~n', Current_Position),
    format('Queue               : ~w~n', [Queue]),
    format('Current_move_queue  : ~w~n', [Current_move_queue]),
    length(Current_path, Estimate_energy_consumption),
    Estimate_energy is E - Estimate_energy_consumption,

    ((Estimate_energy < 0 , Task \= find(c(_)))
    ->  get_agent_position(A, Search_starting_position), 
        solve_task_as(Original_task, find(c(_)), A, E, [state([Search_starting_position], 0)], [], Current_move_queue, Move_queue)
    ;   (achieved(Task, Current_Position) 
        ->  (task_achieved(Original_task, Current_Position)
            ->  reverse([move_queue(Task, Current_path)|Current_move_queue], Move_queue),
                format('Final move queue    : ~w~n', [Move_queue])
            ;   Current_path = [New_starting_position|_],
                heuristic(New_starting_position, Original_task, H),
                solve_task_as(Original_task, Original_task, A, 100, [state([New_starting_position], H)], [], [move_queue(Task, Current_path)|Current_move_queue], Move_queue))
        ;   (findall(state(New_state_element, F), (
                    New_state_element = [New_position|Current_path],
                    map_adjacent(Current_Position, New_position, empty),
                    heuristic(New_position, Task, H),
                    length(Current_path, G),
                    F is H + G,
                    \+ member(New_position, Visited_node),
                    \+ member(state([New_position|_], _), Queue)
                ), New_states)
                
            ->  (format('New_states      : ~w~n', [New_states]),
                format('remain          : ~w~n', [Other_states]),
                merge_and_sort_by_heuristic(Other_states, New_states, Priority_queue),
                format('Priority_queue  : ~w~n', [Priority_queue]),
                solve_task_as(Original_task, Task, A, E, Priority_queue, [Current_Position|Visited_node], Current_move_queue, Move_queue))
            ;   % Failure case of bagof (when bagof/3 doesn't find any new states)
                format('No new states found. Proceeding with remaining states: ~w~n', [Other_states]),
                solve_task_as(Original_task, Task, A, E, Other_states, Visited_node, Current_move_queue, Move_queue)
            )
        )
    ). % <--- The final dot, terminating the solve_task_as clause


%heuristic(+Next_position, +Task, -Heuristic)
heuristic(Next_position, Task, Heuristic) :-
    ( Task = go(Goal_position), ground(Goal_position), !, map_distance(Next_position, Goal_position, Heuristic)
    ; Heuristic = 0 ).


% merge_and_sort_by_heuristic(+Other_states,+New_states,-Priority_queue)
merge_and_sort_by_heuristic(Queue,[],Queue).
merge_and_sort_by_heuristic(Queue,[Next|Rest],Priority_queue) :-
    insert(Queue,Next,Updated),
    merge_and_sort_by_heuristic(Updated,Rest,Priority_queue).


% insert(+Queue,+Next,-Priority_queue)
insert([],State,[State]).
insert([state(Path, H)|Rest],state(New_path, NH),Priority_queue) :-
    (H >= NH -> Priority_queue=[state(New_path, NH),state(Path, H)|Rest]
    ;otherwise       -> insert(Rest,state(New_path, NH),Result),
                        Priority_queue=[state(Path, H)|Result]).


% achieved(+Task,+Pos)
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).


% task_achieved(+Original_task, +Destination)
task_achieved(Original_task, Destination) :- 
    Original_task = find(Obj), map_adjacent(Destination,_,Obj)
    ;
    Original_task = go(Destination).

% agent_do_move_queue(+A, +Move_queue, +Current_cost, -Cost).
agent_do_move_queue(_, [], Current_cost, Cost) :- 
    Cost = Current_cost.

agent_do_move_queue(A, [Current_move|Rest_move_queue], Current_cost, Cost) :-
    Current_move = move_queue(Task, Reversed_path),
    format('movement assigned : ~w~n', Task),
    reverse(Reversed_path, [_| Path]),
    length(Reversed_path, New_cost),
    format('Moving... : ~w~n', [Path]),
    agent_do_moves(A,Path),
    (   Task = find(c(N)) 
        ->  (   agent_topup_energy(A, c(N)),
                format('Agent energy topuped! : ~w~n', A),
                New_current_cost is Current_cost + New_cost,
                agent_do_move_queue(A, Rest_move_queue, New_current_cost, Cost)
            )
        ;   New_current_cost is Current_cost + New_cost,
            agent_do_move_queue(A, Rest_move_queue, New_current_cost, Cost)
    ).