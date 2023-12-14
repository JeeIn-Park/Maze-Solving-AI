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



find_nearest(From, oracle, Energy, Destination, ID, Path, Cost) :-
    findall(Map_distance-ID,
        ( object(o(ID), Location),
        map_distance(From, Location, Map_distance)
        ),Oracles),
    keysort(Oracles, Sorted_Oracles),
    nearest_oracle(From, Sorted_Oracles, Energy, Destination, ID, Path, Cost).

find_nearest(From, station, Energy, Destination, ID, Path, Cost) :-
    findall(Map_distance-ID,
        ( object(c(ID), Location),
        map_distance(From, Location, Map_distance)
        ),Stations),
    keysort(Stations, Sorted_stations),
    nearest_station(From, Sorted_stations, Energy, Destination, ID, Path, Cost).

find_nearest_oracle(From, Energy, ID, Cost) :-
    findall(Map_distance-ID,
        ( object(o(ID), Location),
        map_distance(From, Location, Map_distance)
        ),Oracles),
    keysort(Oracles, Sorted_Oracles),
    go_nearest_oracle(From, Sorted_Oracles, Energy, ID, Cost).


nearest_oracle(From, [_-ID|Oracles], Energy, Destination, ID, Path, Cost) :-
    object(o(ID), Location),
    max_energy(Max_energy),
    adjacent_empty_cell(Location, From, Adj_location, _),
    ( solve_task_as(go(Adj_location), go(Adj_location), Energy, Max_energy, [state([From], 0)], [], [], [move_queue(go(Adj_location), Reversed_path)])
    ->  Reversed_path = [Destination|_],
        reverse(Reversed_path, [_|Path]),
        length(Path, Cost)
    ;   nearest_oracle(From, [Oracles], Energy, Destination, ID, Path, Cost)
    ).


nearest_station(From, [_-ID|Oracles], Energy, Destination, ID, Path, Cost) :-
    object(c(ID), Location),
    max_energy(Max_energy),
    adjacent_empty_cell(Location, From, Adj_location, _),
    ( solve_task_as(go(Adj_location), go(Adj_location), Energy, Max_energy, [state([From], 0)], [], [], [move_queue(go(Adj_location), Reversed_path)])
    ->  Reversed_path = [Destination|_],
        reverse(Reversed_path, [_|Path]),
        length(Path, Cost)
    ;   nearest_station(From, [Oracles], Energy, Destination, ID, Path, Cost)
    ).

go_nearest_oracle(From, [_-ID|Oracles], Energy, ID, Cost) :-
    object(o(ID), Location),
    adjacent_empty_cell(Location, From, Adj_location, _), !,
    (solve_task(go(Adj_location), Cost) ; go_nearest_oracle(From, Oracles, Energy, ID, Cost)).
 

eliminate(_, [A], A) :- 
    format('eliminate | found identity : ~w~n', [A]), !.
eliminate(G, [A|As], Result) :-
    query_energy(Query_energy),
    get_agent_energy(G, E),
    get_agent_position(G, P),
    format('eliminate | agent energy [~w], at position [~w] ~n', [E, P]),
    (find_nearest(P, oracle, E, Destination, OID, Path, Cost)
    ->  New_energy is E - Cost - Query_energy,
        format('eliminate | new energy : ~w~n', [New_energy]),
        ( find_nearest(Destination, station, New_energy, _, _, _, _)
        ->  agent_do_moves(G, Path),
            format('eliminate |  --- asking [~w]~n', [OID]),
            ( \+ agent_check_oracle(G, o(OID))
            ->  agent_ask_oracle(G, o(OID), link, L),
                format('eliminate | --- link : ~w~n', [L]), 
                include(actor_has_link(L), [A|As], New_As),
                retractall(object(o(OID),_))
            ;   retractall(object(o(OID),_))
            ), eliminate(G, New_As, Result)
        ;   format('eliminate | will not be able to go charging station.. ~n'),
            ( find_nearest(P, station, E, _, CID, Charge_path, _)
            ->  agent_do_moves(G, Charge_path),
                agent_topup_energy(G, c(CID)),
                format('eliminate | going to charge first ~n'),
                eliminate(G, [A|As], Result)
            ;   format('eliminate | cannot go to charge station, visit oracle ~n'),
                agent_do_moves(G, Path),
                ( \+ agent_check_oracle(G, o(OID))
                ->  agent_ask_oracle(G, o(OID), link, L),
                    include(actor_has_link(L), [A|As], New_As),
                    retractall(object(o(OID),_))
                ;   retractall(object(o(OID),_))
                ), eliminate(G, New_As, Result)
            )  
        )
    ;   format('eliminate | cannot reach oracle, try charging~n'),
        find_nearest_oracle(P, E, OID, _),
        ( \+ agent_check_oracle(G, o(OID))
        ->  agent_ask_oracle(G, o(OID), link, L),
            format('eliminate | --- link : ~w~n', [L]), 
            include(actor_has_link(L), [A|As], New_As),
            retractall(object(o(OID),_))
        ;   retractall(object(o(OID),_))
        ), eliminate(G, New_As, Result)
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
