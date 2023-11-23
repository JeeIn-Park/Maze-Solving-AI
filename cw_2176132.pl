% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    heuristic(P, Task, Heuristic),
    solve_task_as(Task,[[Heuristic, P]],[],[P|Path]), !,

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
solve_task_as(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [_, Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)
    ;otherwise           -> findall(List, bagof([NF, NP, Pos|RPath], 
                            NF^(map_adjacent(Pos,NP,empty), heuristic(NP, Task, NH), length(RPath, NG), NF is NH + NG, \+ member(NP,Visited), \+ member([NP|_], Rest)), 
                            List), Newfound),
                            append(Newfound,Rest,NewQueue),
                            sort_lists_by_first_element(NewQueue, PriorityQueue),
                            write(PriorityQueue),
                            solve_task_as(Task, PriorityQueue,[Pos|Visited],Path))).


heuristic(Pos, Task, Heuristic) :- 
    (Task = go(GoalPos), !, map_distance(Pos, GoalPos, Heuristic)
    ; 
    Heuristic = 0).



% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).