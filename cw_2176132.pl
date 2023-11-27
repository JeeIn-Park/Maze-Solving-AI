% Accomplish a given task and return the cost
% solve_task(+Original_task, -Cost)
solve_task(Original_task,Cost) :-

    % check the agent for the task and get the initial state
    my_agent(A), 
    get_agent_position(A,P),
    get_agent_energy(A, E),
    ailp_grid_size(N),

    % calculate value needed to caculate the path for the task
    heuristic(P, Original_task, H),
    X is ( N * N / 4 ), ceiling(X, Max_energy),

    % New_energy is E - 10, need to deal with query energy
    solve_task_as(Original_task, Original_task, E, Max_energy, [state([P], H)], [], [], Move_queue), 
format('Ready for movement       : ~w~n', [Move_queue]),
    agent_do_move_queue(A, Move_queue, 0, Cost).


% Calculate the path required to achieve a Task
% solve_task_as(+Final task, +Current Task, +Input energy, +Queue(path), +Visited node, +Current_move_queue, -Move_queue)
solve_task_as(Original_task, Task, E, Max_energy, Path, Visited_node, Current_move_queue, Move_queue) :-
    Path = [state(Current_path, _)| Other_states],
    Current_path = [Current_Position|_],
format('Current_Position         : ~w~n', Current_Position),
format('Path                     : ~w~n', [Path]),
format('Current_move_queue       : ~w~n', [Current_move_queue]),
    length(Current_path, Estimate_energy_consumption),
    Estimate_energy is E - Estimate_energy_consumption +1,
format('Initial energy           : ~w~n', E),
format('Estimate energy          : ~w~n', Estimate_energy),

    ((Estimate_energy < 0 , Task \= find(c(_)))
    ->  reverse(Current_path, [Search_starting_position|_]),
        solve_task_as(Original_task, find(c(_)), E, Max_energy, [state([Search_starting_position], 0)], [], Current_move_queue, Move_queue)
    ;   (achieved(Task, Current_Position) 
        ->  (task_achieved(Original_task, Current_Position)
            ->  reverse([move_queue(Task, Current_path)|Current_move_queue], Move_queue),
format('Final move Path          : ~w~n', [Move_queue])
            ;   Current_path = [New_starting_position|_],
                heuristic(New_starting_position, Original_task, H),
                solve_task_as(Original_task, Original_task, Max_energy, Max_energy, [state([New_starting_position], H)], [], [move_queue(Task, Current_path)|Current_move_queue], Move_queue))
        ;   (findall(state(New_state_element, F), (
                    New_state_element = [New_position|Current_path],
                    map_adjacent(Current_Position, New_position, empty),
                    heuristic(New_position, Task, H),
                    length(Current_path, G),
                    F is H + G,
                    \+ member(New_position, Visited_node),
                    \+ member(state([New_position|_], _), Path)
                ), New_states)
                
            ->  (
format('New_states              : ~w~n', [New_states]),
format('remain                  : ~w~n', [Other_states]),
                merge_and_sort_by_heuristic(Other_states, New_states, Priority_queue),
format('Priority_queue          : ~w~n', [Priority_queue]),
                solve_task_as(Original_task, Task, E, Max_energy, Priority_queue, [Current_Position|Visited_node], Current_move_queue, Move_queue))
            ;   % Failure case of bagof (when bagof/3 doesn't find any new states)
format('No new states found. Proceeding with remaining states: ~w~n', [Other_states]),
                solve_task_as(Original_task, Task, E, Max_energy, Other_states, Visited_node, Current_move_queue, Move_queue)
            )
        )
    ).


%heuristic(+Next_position, +Task, -Heuristic)
heuristic(Next_position, Task, Heuristic) :-
    ( Task = go(Goal_position), ground(Goal_position), !, map_distance(Next_position, Goal_position, Heuristic)
    ; Heuristic = 0 ).


% merge_and_sort_by_heuristic(+Other_states,+New_states,-Priority_queue)
merge_and_sort_by_heuristic(Path,[],Path).
merge_and_sort_by_heuristic(Path,[Next|Rest],Priority_queue) :-
    insert(Path,Next,Updated),
    merge_and_sort_by_heuristic(Updated,Rest,Priority_queue).


% insert(+Path,+Next,-Priority_queue)
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


%Empty Move Queue: Test the behavior when Move_queue is an empty list.
%Single Task Move Queue: Check the code's response when Move_queue contains a single task.
%Task Path with Multiple Tasks: Test the program with Move_queue containing multiple tasks in a sequence.
%Tasks with Varying Energies: Check how the program handles tasks with different energy requirements.
%Task Completion at Initial State: Test the scenario where the task can be achieved from the initial state.
%No Possible States: Check how the program behaves when there are no possible states to move to from the current position.
%Backtracking Due to Energy: Test a scenario where the agent backtracks due to insufficient energy.
%Task Completion at End State: Test if the program correctly handles a scenario where the task is achieved at the end state.
%Energy Depletion: Check the behavior when the agent's energy gets depleted during the task execution.
%Agent Top-Up Energy Scenario: Test scenarios where Task is of the form find(c(N)) and ensure that agent_topup_energy(A, c(N)) is invoked correctly.


solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  %memberchk(1,[]),
  solve_task_bt(Task,[[c(0,0,P),[]]],R,F,G,_NewPos,RR,BackPath),!,
  write(BackPath), % prune choice point for efficiency
  reverse(BackPath,[_Init|Path]),
  write(Path),
  query_world( agent_do_moves, [Agent,Path] ).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Agenda,ClosedSet,F,G,NewPos,RR,BackPath) :-
  achieved(Task,Agenda,ClosedSet,F,G,NewPos,RR,BackPath).

solve_task_bt(go(Target),Agenda,ClosedSet,F,G,NewPos,RR,BackPath) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,P)|RPath],
  NewAgenda = Rest,
  Bag = search(P,P1,R,C),
  (\+ setof((P1,C),P^R^Bag,PS) -> solve_task_bt(go(Target),Rest,[Current|ClosedSet],F,G,NewPos,RR,BackPath);
  	otherwise -> 
 	setof((P1,C),P^R^Bag,PS),
  	addChildren(PS,RPath,Current,NewAgenda,Target,Result),
  	NewClosedSet = [Current|ClosedSet],
  	NewestAgenda = Result,
  	solve_task_bt(go(Target),NewestAgenda,NewClosedSet,F1,G1,Pos,P|RPath,BackPath)
  	).  % backtrack search


addChildren([],RPath,Current,Result,Target,Result).
addChildren(PS,RPath,Current,NewAgenda,Target,Result) :-
	Current = [c(F,G,P)|Rest],
	(NewAgenda = [] -> true;
		otherwise -> NewAgenda = [BestAgenda|_],
	BestAgenda = [c(FP,GP,PP)|_]),
	PS = [(Pos,Cost)|Others],
	( memberchk(Pos,RPath) -> addChildren(Others,RPath,Current,NewAgenda,Target,Result)
		; otherwise -> 
	G1 is G+Cost,
	map_distance(Pos,Target,Dist),
	F1 is G1 + Dist,
	( RPath = [[]] -> Child = [c(F1,G1,Pos),P]
	; otherwise -> Child = [c(F1,G1,Pos),P|RPath]
	),
	(  memberchk(Child,NewAgenda) -> addChildren(Others,RPath,Current,NewAgenda,Target,Result)
		
	;  otherwise -> 
		( 	NewAgenda=[] -> BrandNewAgenda = [Child|NewAgenda]
		;	otherwise    -> 
			(F1 =< FP -> BrandNewAgenda = [Child|NewAgenda];
			otherwise  -> append(NewAgenda,[Child],BrandNewAgenda)
			)
		),
		addChildren(Others,RPath,Current,BrandNewAgenda,Target,Result)
	)).

achieved(go(Exit),Agenda,ClosedSet,F,G,NewPos,Check,NewCheck) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  NewCheck = [NewPos|RPath],
  ( Exit=none -> true
  ; otherwise -> NewCheck = [Exit|_]
  ).

achieved(find(O),Agenda,ClosedSet,F,G,NewPos,Check,NewCheck) :-
  Agenda = [Current|Rest],
  Current = [c(F,G,NewPos)|RPath],
  write(rpath),write(RPath),nl,
  NewCheck = [NewPos|RPath],
  write(check),write(NewCheck),nl,
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).