% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    heuristic(P, Task, H),
    solve_task_as(Task, A, [state([P], H)], [], [P|Path]), !,

    agent_do_moves(A,Path), 
    length(Path,Cost).


% Calculate the path required to achieve a Task
solve_task_as(Task, A, Queue, Visited_node, Path) :-
    Queue = [state(Current_path, _)| Other_states],
    Current_path = [Current_Position|_],
    format('Current_Position: ~w~n', Current_Position),
    format('Queue           : ~w~n', [Queue]),
    get_agent_energy(A, E),
    length(Current_path, Estimate_energy_consumption),
    Estimate_energy is E - Estimate_energy_consumption,
    (Estinate_energy < 0
    ->  solve_task(find(c(_)), _),
        agent_topup_energy(A, c(_)),
        get_agent_energy(A, Topuped_energy),
        get_agent_position(A, New_starting_position),
        solve_task_as(Task, A, [state([New_starting_position], H)], [], Path),
    ;   (achieved(Task, Current_Position),
        ->  reverse(Current_path, Path) 
        
        ;   (( findall(state(New_state_element, F), (
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
                merge_and_sort_by_heuristic(Other_states, New_states, Priority_queue),
                format('Priority_queue  : ~w~n', [Priority_queue]),
                solve_task_as(Task, A, Priority_queue, [Current_Position|Visited_node], Path)
            )
            ;   % Failure case of bagof (when bagof/3 doesn't find any new states)
                format('No new states found. Proceeding with remaining states: ~w~n', [Other_states]),
                solve_task_as(Task, A, Other_states, Visited_node, Path)
            )
        )
    ).


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

    