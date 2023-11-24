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
    Queue = [state(Current_path, Estimated_cost)| Other_states],
    Current_path = [Current_Position|Passed_Path],
    (achieve(Task, Current_Position) 
        -> reverse(Current_path, Path);

    otherwise 
        -> ( bagof( state([New_position|Current_path], F), F^
                ( map_adjacent(Current_Position, New_position, empty), 
                  heuristic(New_position, Task, H), length(Passed_Path, G), F is H + G,
                  \+ member(New_position, Visited_node),
                  \+ member([New_position|_], Queue)
                ),
                New_states
            ),
            append(Other_states, New_states, New_Queue),
            sort_queue(New_Queue, Priority_queue),
            solve_task_as(Task, Priority_queue, [Current_Position|Visited_node], Path)
        )
    ).
    
    

search_bf(Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (complete(Current) -> reverse([Pos|RPath], Path)
    ;otherwise -> (
        findall([NP,Pos|RPath],
        (map_adjacent(Pos, NP, empty),
        \+member(NP, Visited),
        \+member([NP|_], Rest)
        ), NewFound),
        append(Rest, NewFound, NewQueue), 
        search_bf(NewQueue, [Pos|Visited], Path))
    ).
    
    
    Queue = [Next|Rest],
    Next = [_, Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)
    ; otherwise -> ( bagof([NF, NP, Pos|RPath], 
            NF^(map_adjacent(Pos, NP, empty), heuristic(NP, Task, NH), length(Next, NG), NF is NH + NG,
                \+ member(NP, Visited),
                \+ member([NP|_], Rest)),
            Newfound),
        append(Newfound, Rest, NewQueue),
        sort_lists_by_first_element(NewQueue, PriorityQueue),
        %write(PriorityQueue),
        solve_task_as(Task, PriorityQueue, [Pos|Visited], Path)
    )).




% Comparator predicate to compare lists based on their first element
compare_first_element(<, [X1|_], [X2|_]) :-
    X1 @< X2. % Sort based on the first element in ascending order
compare_first_element(>, [X1|_], [X2|_]) :-
    X1 @> X2. % Sort based on the first element in descending order
compare_first_element(=, _, _). % If the first elements are equal, keep the order unchanged

% Predicate to sort a list of lists based on their first element
sort_lists_by_first_element(Lists, SortedLists) :-
    predsort(compare_first_element, Lists, SortedLists), write(SortedLists).




heuristic(NextPos, Goal, Heuristic) :-
    ( Goal = go(GoalPos), ground(GoalPos), !, map_distance(NextPos, GoalPos, Heuristic)  % If Goal is go(Pos), use Manhattan distance to the GoalPosition
    ; Heuristic = 0 ).


% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).