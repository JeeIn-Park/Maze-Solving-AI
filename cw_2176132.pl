% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,p(X, Y)),

    Task = p(TaskX, TaskY),
    abs(X - TaskX, AbsX), abs(Y - TaskY, AbsY),
    solve_task_bfs(Task,[[AbsX + AbsY, p(X, Y)]],[],[p(X, Y)|Path]), !,

    agent_do_moves(A,Path), 
    length(Path,Cost).


% Comparator predicate to compare lists based on their first element
compare_first_element(<, [X1|_], [X2|_]) :-
    X1 @< X2. % Sort based on the first element in ascending order
compare_first_element(>, [X1|_], [X2|_]) :-
    X1 @> X2. % Sort based on the first element in descending order
compare_first_element(=, _, _). % If the first elements are equal, keep the order unchanged

% Predicate to sort a list of lists based on their first element
sort_lists_by_first_element(Lists, SortedLists) :-
    predsort(compare_first_element, Lists, SortedLists).


% Calculate the path required to achieve a Task
solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [F, Pos|RPath],
    Task = p(TaskX, TaskY),
    abs(X - TaskX, AbsX), abs(Y - TaskY, AbsY),
    (achieved(Task, Pos) -> reverse(Next,Path)

    ;otherwise           -> (findall([NF, NP,Pos|RPath],
                            (map_adjacent(Pos,NP,empty), (), \+ member(NP,Visited), \+ member([NP|_],Rest)),
                            Newfound),
                            append(Newfound,Rest,NewQueue),
                            %write(NewQueue),
                            sort_lists_by_f(Task, NewQueue, PriorityQueue),
                            %write(PriorityQueue),
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
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).
