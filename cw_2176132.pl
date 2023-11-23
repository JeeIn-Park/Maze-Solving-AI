% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), 
    get_agent_position(A,P),
    solve_task_bfs(Task,[[P]],[],[P|Path]), !,

    agent_do_moves(A,Path), 
    length(Path,Cost).

% Calculate the path required to achieve a Task
solve_task_bfs(Task, Queue, Visited, Path) :-
    Queue = [Next|Rest],
    Next = [_, Pos|RPath],
    (achieved(Task, Pos) -> reverse([Pos|RPath],Path)

    ;otherwise           -> (findall([NP, Pos|RPath],
                            (map_adjacent(Pos,NP,empty), \+ member(NP,Visited), \+ member([NP|_], Rest)),
                            Newfound),
                            append(Newfound,Rest,NewQueue),
                            solve_task_bfs(Task, NewQueue,[Pos|Visited],Path))).


heuristic(NextPos, Goal, Heuristic) :- 
    (Goal = go(GoalPos), ground(GoalPos), !, 
    map_distance(NextPos, GoalPos, Heuristic); 
    Heuristic = 0).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task = find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task = go(Pos).