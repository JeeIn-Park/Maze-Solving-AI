% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    heuristic(P, Task, H),
    solve_task_as(Task, [state([P], H)], [], [P|Path]), !,

    agent_do_moves(A,Path), 
    length(Path,Cost).


% Calculate the path required to achieve a Task
solve_task_as(Task, Queue, Visited_node, Path) :-
    Queue = [state(Current_path, _)| Other_states],
    Current_path = [Current_Position|_],
    format('Current_Position: ~w~n', Current_Position),
    format('Queue           : ~w~n', [Queue]),
    (achieved(Task, Current_Position) 
        -> reverse(Current_path, Path) 
        
        ;
        (   ( findall(state(New_state_element, F), (
                    New_state_element = [New_position|Current_path],
                    map_adjacent(Current_Position, New_position, empty),
                    heuristic(New_position, Task, H),
                    length(Current_path, G),
                    F is H + G,
                    \+ member(New_position, Visited_node),
                    \+ member(state([New_position|_], _), Queue)
                ), New_states)
                
            ->  format('New_states      : ~w~n', [New_states]),
                format('remain          : ~w~n', [Other_states]),
                append(Other_states, New_states, New_Queue),
                format('Unsorted queue  : ~w~n', [New_Queue]),
                sort_states_by_heuristic(New_Queue, Priority_queue),
                format('Priority_queue  : ~w~n', [Priority_queue]),
                solve_task_as(Task, Priority_queue, [Current_Position|Visited_node], Path)
            )
        ;   % Failure case of bagof (when bagof/3 doesn't find any new states)
            format('No new states found. Proceeding with remaining states: ~w~n', [Other_states]),
            solve_task_as(Task, Other_states, Visited_node, Path)
        )
    ).


heuristic(Next_position, Task, Heuristic) :-
    ( Task = go(Goal_position), ground(Goal_position), !, map_distance(Next_position, Goal_position, Heuristic)
    ; Heuristic = 0 ).


% Comparator predicate to compare states based on their heuristic values while preserving original order for equal values
compare_state_h(Result, state(_, H1), state(_, H2)) :-
    (   H1 < H2 ->
        Result = (<)
    ;   H1 > H2 ->
        Result = (>)
    ;   Result = (=)
    ).

% Sorting a list of states based on their heuristic values in ascending order
sort_states_by_heuristic(Unsorted_list, Sorted_list) :-
    predsort(compare_state_h, Unsorted_list, Sorted_list).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).