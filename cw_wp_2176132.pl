% object(object(ID), p(x, y))
:- dynamic object/2.


assert_object([]).
assert_object([Object|Other]) :-
    assert(Object),
    assert_object(Other).


% True if link L appears on A's Wikipedia page
actor_has_link(L, A) :- 
    actor(A), wp(A, WT), wt_link(WT, L).


merge_and_sort_by_distance(Cells, [], Cells).
merge_and_sort_by_distance(Cells, [O | Os], Sorted_cells) :-
    insert_cell(Cells, O, Updated),
    merge_and_sort_by_distance(Updated, Os, Sorted_cells).


insert_cell([], State, [State]).
insert_cell([cell(Distance, Object, Reversed_path) | Rest], cell(New_distance, New_object, New_reversed_path), Sorted_cells) :-
    (Distance >= New_distance 
    ->  Sorted_cells = [cell(New_distance, New_object, New_reversed_path), cell(Distance, Object, Reversed_path) | Rest]
    ;   insert_cell(Rest, cell(New_distance, New_object, New_reversed_path), Result),
        Sorted_cells = [cell(Distance, Object, Reversed_path) | Result]).


adjacent_cell(o(ID), Pos, Task) :-
    findall(Distance-p(X,Y),
        (map_adjacent(p(X,Y), _, o(ID)),lookup_pos(p(X,Y),Obj),Obj=empty,map_distance(Pos,p(X,Y),Distance)),
        Positions),
    keysort(Positions,UniquePositions),
    UniquePositions=[_-ClosestAdjacent|_],
    Task=go(ClosestAdjacent).
adjacent_cell(c(ID), Pos, Task) :-
    findall(Distance-p(X,Y),
        (map_adjacent(p(X,Y), _, c(ID)),lookup_pos(p(X,Y),Obj),Obj=empty,map_distance(Pos,p(X,Y),Distance)),
        Positions),
    keysort(Positions,UniquePositions),
    UniquePositions=[_-ClosestAdjacent|_],
    Task=go(ClosestAdjacent).


eliminate(_, [A], A) :- 
    format('found identity : ~w~n', [A]), !.
eliminate(G, [A|As], Result) :-
    get_agent_energy(G, E),
    get_agent_position(G, P),
    ailp_grid_size(N),
    X is ( N * N / 4 ), ceiling(X, Max_energy), !, 
    format('agent energy [~w], agent position [~w], grid size [~w], max energy [~w] ~n', [E, P, N, Max_energy]),
    findall(cell(Distance, o(ID), Reversed_path),
        (object(o(ID), _), 
        adjacent_cell(o(ID), P, Task),
        solve_task_as(Task, Task, E, Max_energy, [state([P], 0)], [], [], [move_queue(Task, Reversed_path)]),
        length(Reversed_path, Distance)),
        Oracles),
    format('reachable oracles : ~w~n', [Oracles]),
    Oracles = [O | Os],
    merge_and_sort_by_distance([O], Os, Sorted_oracles),
    format('sorted oracles : ~w~n', [Sorted_oracles]),
    Sorted_oracles = [cell(Distance, o(ID), Reversed_path) | _],
    New_energy is E - Distance,
    Reversed_path = [NP | _],
    findall(cell(D, c(I), R),
        (object(c(I), _), 
        adjacent_cell(c(I), NP, Task),
        solve_task_as(Task, Task, New_energy, Max_energy, [state([NP], 0)], [], [], [move_queue(Task, R)]),
        length(R, D)),
        Stations),
    (   Stations = []
    ->  findall(cell(D, c(I), R),
        (object(c(I), _), 
        adjacent_cell(c(I), P, Task),
        solve_task_as(Task, Task, E, Max_energy, [state([P], 0)], [], [], [move_queue(Task, R)]),
        length(R, D)),
        NSs),
        NSs = [Ns | RNSs],
        merge_and_sort_by_distance([Ns], RNSs, [cell(_, c(CID), RC_path)|_]),
        reverse(RC_path, [_ | Path]),
        agent_do_moves(G, Path),
        agent_topup_energy(G, c(CID)),
        eliminate(G, [A|As], Result)
    ;   reverse(Reversed_path, [_ | Path]),
        agent_do_moves(G, Path),
        format(' --- asking ~n'),
        agent_ask_oracle(G, o(ID), link, L),
        format('--- link : ~w~n', [L]), 
        include(actor_has_link(L), [A|As], New_As),
        eliminate(G, New_As, Result)
    ).



map_reading :- 
    retractall(object(_,_)),
    ailp_grid_size(N),
    findall(object(o(ID), p(X, Y)),
        (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), o(ID))),
        Oracles),
    format('oracles : ~w~n', [Oracles]),
    assert_object(Oracles),
    findall(object(c(ID), p(X, Y)),
        (between(1, N, X), between(1, N, Y), lookup_pos(p(X, Y), c(ID))),
        Stations),
    format('stations : ~w~n', [Stations]),
    assert_object(Stations).


% Deduce the identity of the secret actor A
find_identity(A) :- 
    format('going to check agent identity... ~n'),
    map_reading,
    format('>> map reading done << ~n'),
    findall(A, actor(A), As), 
    format('possible identity list : ~w~n', [As]),
    my_agent(G),
    format('identity checking just started. ~n'),
    eliminate(G, As, A).
