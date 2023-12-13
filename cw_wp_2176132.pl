oracle.
station.
:- dynamic object/2.
:- dynamic max_energy/1.
:- dynamic query_energy/1.


assert_object([]).
assert_object([Object|Other]) :-
    assert(Object),
    assert_object(Other).


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


adjacent_empty_cell(Location, From, Nearest, Distance) :-
    findall(Distance-Destination,
        (map_adjacent(Location, Destination, empty), map_distance(From, Destination, Distance)),
        Positions),
    keysort(Positions, Sorted),
    Sorted = [_-Nearest | _].


get_oracles(oracle, Oracles) :-
    findall(ID-Location, object(o(ID), Location), Oracles).
get_stations(station, Stations) :-
    findall(ID-Location, object(c(ID), Location), Stations).


find_nearest(From, Locations, Energy, Max, Destination, ID, Path, Cost) :-
    findall(Distance-move_queue(ID, Reversed_path),
        (member(ID-Location, Locations),
        adjacent_empty_cell(Location, From, Adj_location, _),
        solve_task_as(go(Adj_location), go(Adj_location), Energy, Max, [state([From], 0)], [], [], [move_queue(go(Adj_location), Reversed_path)]),
        length(Reversed_path, Distance)),
        Move_queue),
    keysort(Move_queue, [Cost-move_queue(ID, [Destination | RPath]) | _]),
    reverse([Destination | RPath], [_|Path]).


eliminate(_, [A], A) :- 
    format('eliminate | found identity : ~w~n', [A]), !.
eliminate(G, [A|As], Result) :-
    get_agent_energy(G, E),
    get_agent_position(G, P),
    format('eliminate | agent energy [~w], agent position [~w], grid size [~w] ~n', [E, P, N]),
    get_oracles(oracle, Oracles),
    format('eliminate | oracle locations : ~w~n', [Oracles]),
    find_nearest(P, Oracles, E, Max_energy, Destination, OID, Path, Cost),
    New_energy is E - Cost - (Max_energy/10),
    format('eliminate | new energy : ~w~n', [New_energy]),
    
    get_stations(station, Station_locations),
    format('eliminate | station locations : ~w~n', [Station_locations]),
    ( find_nearest(Destination, Station_locations, New_energy, Max_energy, _, _, _, _)
    ->  agent_do_moves(G, Path),
        format('eliminate |  --- asking [~w]~n', [OID]),
        ( \+ agent_check_oracle(G, o(OID))
        ->  agent_ask_oracle(G, o(OID), link, L),
            retractall(object(o(OID),_))
        ;   retractall(object(o(OID),_))
        ),
        format('eliminate | --- link : ~w~n', [L]), 
        include(actor_has_link(L), [A|As], New_As),
        eliminate(G, New_As, Result)
    ;   find_nearest(P, Station_locations, E, Max_energy, _, CID, Charge_path, _),
        agent_do_moves(G, Charge_path),
        agent_topup_energy(G, c(CID)),
        eliminate(G, [A|As], Result)
    ).




% Deduce the identity of the secret actor A
find_identity(A) :- 
    ailp_grid_size(N),
    X is ( N * N / 4 ), ceiling(X, Max_energy),
    retractall(max_energy(_)),
    assert(max_energy(Max_energy)),
    Y is ( Max_energy / 10 ), ceiling(Y, Query_energy),
    retractall(query_energy(_)),
    assert(query_energy(Query_energy)),

    format('find_identity | going to check agent identity... ~n'),
    map_reading,
    format('find_identity | >> map reading done << ~n'),
    findall(A, actor(A), As), 
    format('find_identity | possible identity list : ~w~n', [As]),
    my_agent(G),
    format('find_identity | identity checking just started. ~n'),
    eliminate(G, As, A).
