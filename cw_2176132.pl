% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    get_agent_energy(A, E),
    heuristic(P, Task, H),
    solve_task_as(Task, [state([P], H)], [], [P|Path], E), !,

    agent_do_moves(A,Path), 
    length(Path,Cost),
    get_agent_energy(A, E2),
    format('Energy left: ~w~n', E2).


% Calculate the path required with a* algorithm to achieve a Task
solve_task_as(Task, Queue, Visited_node, Path, Starting_energy) :-
    Queue = [state(Current_path, _)| Other_states],
    Current_path = [Current_Position|_],
    format('Current_Position: ~w~n', Current_Position),
    format('Queue           : ~w~n', [Queue]),
    (achieved(Task, Current_Position) 
    -> (reverse(Current_path, Path))
    ; (
        Starting_energy =< 0
        -> ( format('No energy left, current position: ~w~n', Current_Position),
            reverse(Current_path, [Starting_Point|_]), 
            length(Current_path, Energy_consumped),
            format('cunsumped energy: ~w~n', Energy_consumped),
            solve_task_as(find(c(_)), [state([Starting_Point], 0)], [], Charging_path, Starting_energy + Energy_consumped),
            reverse(Charging_path, [Charging_Station|Reversed_charging_path]),
            format('found charging station: ~w~n', Charging_Station),
            format('charging path to be added: ~w~n', [Charging_Station|Reversed_charging_path]),
            heuristic(Charging_Station, Task, H),
            format('searching for the path again: ~w~n', Starting_Point),
            solve_task_as(Task, [state([Charging_Station|Reversed_charging_path], H)], [], Path, 100)
            )
        ; (   ( bagof(state([New_position|Current_path], F), F^(
                    map_adjacent(Current_Position, New_position, empty),
                    heuristic(New_position, Task, H),
                    length(Current_path, G),
                    F is H + G,
                    \+ member(New_position, Visited_node),
                    \+ member(state([New_position|_], _), Queue)
                ), New_states)

            ->  format('    New_states      : ~w~n', [New_states]),
                format('    remain          : ~w~n', [Other_states]),
                append(Other_states, New_states, New_Queue),
                format('    Appended queue  : ~w~n', [New_Queue]),
                sort_queue(New_Queue, Priority_queue),
                format('    Priority_queue  : ~w~n', [Priority_queue]),
                New_Energy is Starting_energy - 1,
                solve_task_as(Task, Priority_queue, [Current_Position|Visited_node], Path, New_Energy)
            )
            ;   % Failure case of bagof (when bagof/3 doesn't find any new states)
            format('No new states found. Proceeding with remaining states: ~w~n', [Other_states]),
            solve_task_as(Task, Other_states, Visited_node, Path, Starting_energy)
            )

        )
        
    ).


heuristic(Next_position, Task, Heuristic) :-
    ( Task = go(Goal_position), ground(Goal_position), !, map_distance(Next_position, Goal_position, Heuristic)
    ; Heuristic = 0 ).


% Comparator predicate to compare states based on their F values while preserving original order for equal F values
compare_state_f(<, state(_, F), state(_, F)) :- !. % Cut to avoid backtracking when F1 and F2 are equal
compare_state_f(<, state(Index1, F), state(Index2, F)) :-
    Index1 @< Index2, !. % Preserve original order for equal F values
compare_state_f(<, _, _).
compare_state_f(>, state(_, F1), state(_, F2)) :-
    F1 > F2.
compare_state_f(=, _, _).


% Predicate to sort a list of states based on their F values
sort_queue(Queue, Priority_queue) :-
    predsort(compare_state_f, Queue, Priority_queue).


% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).