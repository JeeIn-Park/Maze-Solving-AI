:- dynamic achieve/1.
:- dynamic exit/1.
:- dynamic max_energy/1.
:- dynamic f/1.

m(north). 
m(east).
m(south). 
m(west).

direction(p(X,Y), west, p(X1,Y)) :- X1 is X-1.
direction(p(X,Y), east, p(X1,Y)) :- X1 is X+1.
direction(p(X,Y), north, p(X,Y1)) :- Y1 is Y-1.
direction(p(X,Y), south, p(X,Y1)) :- Y1 is Y+1.

opposite_direction(north, south).
opposite_direction(south, north).
opposite_direction(east, west).
opposite_direction(west, east).


% initial call
solve_maze :-
    retractall(achieve(_)),
    assert(achieve(n)),
    retractall(f(_)),
    assert(f(n)),
    ailp_grid_size(N),
    retractall(exit(_)),
    assert(exit(p(N,N))),
    X is ( N * N / 4 ), ceiling(X, Max_energy),
    retractall(max_energy(_)),
    assert(max_energy(Max_energy)),
    format('------------------solve maze ~n'),
    my_agents(My_agents),
    format('my_agents : ~w~n', [My_agents]),
    initialise_entities(My_agents, [], [], Agents, Entities),
    evolve_state(state(Entities, [], Agents, [], []), _).


initialise_entities([], Agents, Entities, Agents, Entities) :-
    format('finished~n').
initialise_entities([Agent|Agents], Temp_agents, Temp_entities, Updated_agents, Updated_entities) :-
    get_agent_position(Agent, Current_position),
    format('1'),
    findall(
        path(Pos, Direction), 
        (
            agent_adjacent(Agent, Pos, empty), 
            direction(Current_position, Direction, Pos)
        ), 
        New_paths
    ),
    format('2'),
    (
        New_paths = []
        ->  format('3'), initialise_entities(Agents, [Agent|Temp_agents], Temp_entities, Updated_agents, Updated_entities)
        ;   New_paths = [path(_, D)|_], format('4~n'),
            initialise_entities(Agents, Temp_agents, [entity(Agent, D)|Temp_entities], Updated_agents, Updated_entities)
    ).

get_exit_list(0, _, List, List).
get_exit_list(N, Exit, Temp_list, Updated_list) :-
    NN is N - 1,
    get_exit_list(NN, Exit, [path(Exit, m(N))|Temp_list], Updated_list).

% main loop
% State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
evolve_state(State, New_state) :-
    (  ( my_agents(Agents), Agents = [])
    -> true
    ; 
        (   achieve(y)
        ->  ( f(y)
            ->  format('finishing---------------------------------------~w~n', [State]), 
                State = state(_, M, A, W, E), 
                format('~w~n', [M]),
                execute_queue(M, State, [], [], state([], [], A, W, E), Next_state),
                evolve_state(Next_state, New_state)
            ;   my_agents(Agents), 
                format('exit state 1~n'),
                length(Agents, Agent_n),
                format('exit state 2~n'),
                exit(Exit),
                format('exit state 3~n'),
                get_exit_list(Agent_n, Exit, [], Exit_list),
                format('exit state 4 : ~w~n', [Exit_list]),
                queue_waiting_list(Agents, state([], [], Agents, Exit_list, []), Exit_state),
                format('exit state 5 : ~w~n', [Exit_state]),
                retractall(f(_)), !, 
                assert(f(y)),
                Exit_state = state(_, M, A, W, E),
                format('exit state 6~n'),
                execute_queue(M, Exit_state, [], [], state([], [], A, W, E), Next_state),
                format('exit state 7~n'),
                evolve_state(Next_state, New_state)
            )
        ;   (format('Main check 0 : ~w~n', [State]),
            next_move(State, State_1, []), 
            (   achieve(y)
            ->  evolve_state(State, New_state)
            ;   format('Main check 1 : ~w~n', [State_1]),
                State_1 = state(_, _, Available_agents_1, _, _),
                queue_waiting_list(Available_agents_1, State_1, State_2),
                format('Main check 2 : ~w~n', [State_2]),
                State_2 = state(_, Move_queue_2, Available_agents_2, Waiting_list_2, Explored_nodes_2),
                format('Move_queue before execute : ~w~n', [Move_queue_2]),
                execute_queue(Move_queue_2, State_2, [], [], state([], [], Available_agents_2, Waiting_list_2, Explored_nodes_2), State_3),
                %format('Move_queue after execute  : ~w~n', [Move_queue_3]),
                evolve_state(State_3, New_state)
            )
          )
        )
    ).



% recursive call updating entities
next_move( state([], Move_queue, Available_agents, Waiting_list, Explored_nodes), Updated_State, Temp_entities) :-
    format('no entity!'),
    Updated_State = state(Temp_entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), !.
% recursive call updating entities
next_move(State, Updated_State, Temp_entities) :-
    format('------------------next_move ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), 
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes]),
    Entities = [entity(ID, Going)|Other_entities],
    format('entity checked ~n'),
    get_agent_position(ID, Current_position),
    format('position checked ~n'),
    exit(Exit),
    format('exit checked : ~w~n', [Exit]),
    (   Current_position = Exit
    ->  format('exiting !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n'),
        retractall(achieve(_)), assert(achieve(y)), leave_maze(ID), 
        format('left ~n'),
        my_agents(Agents), 
        format('agents : ~w~n', [Agents]),
        Updated_State = state([], Move_queue, [], [], []),
        format('no more next move')
    ;   findall(path(New_position, Direction), 
            (agent_adjacent(ID, New_position, empty), 
            direction(Current_position, Direction, New_position),
            \+ member(agent_move_queue(entity(ID, _), _), Move_queue),
            \+ member(path(New_position, _), Explored_nodes),
            \+ opposite_direction(Going, Direction)
            ), New_paths),
        (   New_paths = []
        ->  (   member(agent_move_queue(entity(ID,Move_direction),_), Move_queue)
            ->  next_move(state(Other_entities, Move_queue, Available_agents, Waiting_list, Explored_nodes), Updated_State, [entity(ID, Move_direction)|Temp_entities])
            ;   next_move(state(Other_entities, Move_queue, [ID|Available_agents], Waiting_list, Explored_nodes), Updated_State, Temp_entities)
            ) 
        ;   (   New_paths = [path(Path_position, Path_direction)]
            ->  next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, Waiting_list, Explored_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities]) 
            ;   New_paths = [path(Path_position, Path_direction)| Other_paths],
                append(Other_paths, Waiting_list, New_waiting_list),
                append(Other_paths, Explored_nodes, New_Explored_nodes),
                next_move(state(Other_entities, [agent_move_queue(entity(ID, Path_direction), [Path_position])|Move_queue], Available_agents, New_waiting_list, New_Explored_nodes), Updated_State, [entity(ID, Path_direction)|Temp_entities])
            )
        )
    ).


search_nearest_node(Queue, Visited, Path, Waiting_list) :-
    Queue = [Next|Rest],
    Next = [Pos|RPath],
    ( member(path(Pos, _), Waiting_list) -> Path = Next
    ; 
        (   findall([NP,Pos|RPath],
                    (   (   map_adjacent(Pos,NP,empty) ;
                            map_adjacent(Pos, NP, a(_))),
                        \+ member(NP, Visited), 
                        \+ member([NP|_],Rest)),
                    Newfound),
            format('Newfound : ~w~n', [Newfound]),
            append(Rest,Newfound,NewQueue),
            search_nearest_node(NewQueue,[Pos|Visited],Path, Waiting_list)
        )
    ).


% queue_waiting_list(State_1, State_2),
% if there is any available agents and if there is any nodes in waiting list, start exploring that node
queue_waiting_list([], State, State) :- 
    format('Looked all available agents ~n').
queue_waiting_list(_, State, State) :-
    (State = state(_, _, _, [], _), !);
    (State = state(_, _, [], _, _), !).
queue_waiting_list(Free_agents, State, Updated_State) :-
    format('------------------queue_waiting_list ~n'),
    State = state(Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes),
    format('State >>> Etities : ~w~n Move_queue : ~w~n Available_agents : ~w~n Waiting_list : ~w~n Explored_nodes : ~w~n', [Entities, Move_queue, Available_agents, Waiting_list, Explored_nodes]),
    Free_agents = [Agent | Agents],
    get_agent_position(Agent, Agent_position),
    (   search_nearest_node([[Agent_position]], [], Reversed_path, Waiting_list)  
    ->  Reversed_path = [Destination|_],
        reverse(Reversed_path,[_|Path]),
        format('Go Path : ~w~n', [Path]),
        select(Agent, Available_agents, Updated_agents),
        select(path(Destination, Direction), Waiting_list, Updated_waiting_list),
        queue_waiting_list(Agents, state([entity(Agent, Direction) | Entities], [agent_move_queue(entity(Agent, Direction), Path) | Move_queue], Updated_agents, Updated_waiting_list, Explored_nodes), Updated_State)
    ;   queue_waiting_list(Agents, State, Updated_State)
    ).

    %Reversed_path = [P1, P2 |_],
    %direction(P1, Move_direction, P2),
    

%execute_queue(Entities_2, Move_queue_2, [], [], [], [], Entities_3, Move_queue_3),
% translate go for one html tick then execute, update availavle agent if eligable 
execute_queue([], _, _, _, state([],[],[],[],[]), State) :-
    exit(y), State = state([],[],[],[],[]).
execute_queue([], _, Agent_list, Move_list, State, State) :-
    format('move... [Agent] : ~w  [Position] : ~w ~n', [Agent_list, Move_list]),
    agents_do_moves(Agent_list, Move_list).
execute_queue(Move_queue, Const, Agent_list, Move_list, Temp_state, Updated_State) :-
    format('------------------execute_queue ~n'),
    Move_queue = [agent_move_queue(entity(ID, Direction), [Next_position|Path]) | Move_queue_left],
    format('Move_queue : ~w~n', [Move_queue]),
    Const = state(Entities_const, Move_queue_const, _, _, _), % can be optimised using shorter form
    Temp_state = state(Temp_entities, Temp_move_queue, Temp_agents, Waiting_list, Explored_nodes),
    get_agent_position(ID, Current_position), direction(Current_position, New_direction, Next_position),
    (   member(Next_position, Move_list)
    ->  execute_queue(Move_queue_left, Const, Agent_list, Move_list, state([entity(ID, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), [Next_position|Path])|Temp_move_queue], Temp_agents, Waiting_list, Explored_nodes), Updated_State)
    ;   (   lookup_pos(Next_position, empty)
        ->  (   Path = []
            ->  exit(Exit), 
                (   (Exit = Next_position, achieve(y))
                ->  format('found!!'), agent_do_moves(ID, [Next_position]), format('bye bye'), leave_maze(ID),
                    execute_queue(Move_queue_left, Const, [ID|Agent_list], [Next_position|Move_list], state([entity(ID, New_direction)|Temp_entities], Temp_move_queue, Temp_agents, Waiting_list, Explored_nodes), Updated_State)
                ;   execute_queue(Move_queue_left, Const, [ID|Agent_list], [Next_position|Move_list], state([entity(ID, New_direction)|Temp_entities], Temp_move_queue, Temp_agents, Waiting_list, Explored_nodes), Updated_State)
                )
            ;   execute_queue(Move_queue_left, Const, [ID|Agent_list], [Next_position|Move_list], state([entity(ID, New_direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), Path)|Temp_move_queue], Temp_agents, Waiting_list, Explored_nodes), Updated_State)
            )
        ;   lookup_pos(Next_position, a(ID2)),
            (   member(agent_move_queue(entity(ID2, _), [Next_position2|Path2]), Move_queue_const)
            ->   get_agent_position(ID2, Current_position2), direction(Current_position2, New_direction2, Next_position2),
                (   opposite_direction(New_direction, New_direction2)
                ->  select(agent_move_queue(entity(ID2, _), _), Move_queue_left, Swaped_move_queue),
                    member(entity(ID2, Direction2), Entities_const),
                    execute_queue(Swaped_move_queue, Const, Agent_list, Move_list, state([entity(ID, Direction2), entity(ID2, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction2), Path2), agent_move_queue(entity(ID2, Direction), Path)|Temp_move_queue], Temp_agents, Waiting_list, Explored_nodes), Updated_State)
                ;   execute_queue(Move_queue_left, Const, Agent_list, Move_list, state([entity(ID, Direction)|Temp_entities], [agent_move_queue(entity(ID, Direction), [Next_position|Path])|Temp_move_queue], Temp_agents, Waiting_list, Explored_nodes), Updated_State)
                )
            ;   select(ID2, Temp_agents, Updated_agents),
                execute_queue(Move_queue_left, Const, Agent_list, Move_list, state([entity(ID2, New_direction)|Temp_entities], [agent_move_queue(entity(ID2, New_direction), Path)|Temp_move_queue], [ID|Updated_agents], Waiting_list, Explored_nodes), Updated_State)
            )
        )
    ).
