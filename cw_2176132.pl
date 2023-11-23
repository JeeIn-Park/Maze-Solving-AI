% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    solve_task_bfs(Task,[[P]],[],[P|Path]), !,

    agent_do_moves(A,Path), 
    length(Path,Cost).

% User-defined comparison predicate with an additional parameter
compare_f(Task, Order, List1, List2) :-
    (   compare_property(Task, List1, List2) -> Order = (<)
    ;   compare_property(Task, List1, List2) -> Order = (>)
    ;   Order = (=)
    ).

% Auxiliary predicate to compare elements based on a property and an extra parameter
compare_property(Task, List1, List2) :-
    % Your comparison logic here based on some_param
    % For example, comparing elements based on some property
    % This could involve some specific checks, computations, or conditions
    % Return true if Elem1 should come before Elem2 based on some_param.

% Sorting a list using predsort with the user-defined comparison predicate with an extra parameter
sort_lists_by_f(Task, Queue, PriorityQueue) :-
    predsort(compare_f(Task), Queue, PriorityQueue).


% Comparator predicate to compare lists based on their lengths
compare_f(<, List1, List2) :-
    Task = p(TaskX, TaskY),
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    length(List1, Len1),
    length(List2, Len2),
    Len1 < Len2.
compare_f(>, List1, List2) :-
    Task = p(TaskX, TaskY),
    length(List1, Len1),
    length(List2, Len2),
    Len1 > Len2.
compare_f(=, _, _). % If the lengths are equal, keep the order unchanged

% Predicate to sort a list of lists based on their lengths
sort_lists_by_f(Task, Queue, PriorityQueue) :-
    predsort(compare_f, Queue, PriorityQueue).


% Calculate the path required to achieve a Task
solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)

    ;otherwise           -> (findall([NP,Pos|RPath],
                            (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                            Newfound),
                            append(Rest,Newfound,NewQueue),
                            sort_lists_by_f(Task, NewQueue, PriorityQueue),
                            
                            Task = p(TaskX, TaskY),
                            Queue = [Next|Rest],
                            Next = [Pos|RPath],
                            NP = p(NPX, NPY),
                            abs(NPX - TaskX, AbsX), abs(NPY - TaskY, AbsY),
                            F is G + AbsX + AbsY,




        % Calculate f(n) = g(n) + h(n) for each node NP
        % Use priority queue based on f(n) to guide the search,
        % where g(n) is the cost to reach NP so far and h(n) is the heuristic cost from NP to Goal.
        % Update NewQueue with the priority queue ordering nodes based on f(n).

                      


                      solve_task_bfs(Task, PriorityQueue,[Pos|Visited],Path))).



solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
        % Calculate f(n) = g(n) + h(n) for each node NP
        % Use priority queue based on f(n) to guide the search,
        % where g(n) is the cost to reach NP so far and h(n) is the heuristic cost from NP to Goal.
        % Update NewQueue with the priority queue ordering nodes based on f(n).

                      append(Rest,Newfound,NewQueue),
                      solve_task_bfs(Task, NewQueue,[Pos|Visited],Path))).


% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).
