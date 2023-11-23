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
    ;   compare_property(Task, List2, List1) -> Order = (>)
    ;   Order = (=)
    ).

% Auxiliary predicate to compare elements based on a property and an extra parameter
compare_property(Task, List1, List2) :-
    Task = p(TaskX, TaskY),
    List1 = [p(X1, Y1)|_], List2 = [p(X2, Y2)|_],
    abs(X1 - TaskX, AbsX1), abs(Y1 - TaskY, AbsY1),
    abs(X2 - TaskX, AbsX2), abs(Y2 - TaskY, AbsY2),
    length(List1, G1), length(List2, G2),
    F1 is G1 + AbsX1 + AbsY1,
    F2 is G2 + AbsX2 + AbsY2,
    F1 < F2.
    % Your comparison logic here based on some_param
    % For example, comparing elements based on some property
    % This could involve some specific checks, computations, or conditions
    % Return true if Elem1 should come before Elem2 based on some_param.

% Sorting a list using predsort with the user-defined comparison predicate with an extra parameter
sort_lists_by_f(Task, Queue, PriorityQueue) :-
    predsort(compare_f(Task), Queue, PriorityQueue).


% Calculate the path required to achieve a Task
solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)

    ;otherwise           -> (findall([NP,Pos|RPath],
                            (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                            Newfound),
                            append(Rest,Newfound,NewQueue),
                            write(NewQueue),
                            sort_lists_by_f(Task, NewQueue, PriorityQueue),
                            write(PriorityQueue),
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
