% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).


% Attempt to solve by visiting each oracle in ID order
eliminate(G, ET, As, A) :- 
    As = [A], !
    ;
    get_agent_energy(G, E),
    (E =< ET
    ->   solve_task(find(c(N)),_)
    ;    solve_task(find(o(N), _))
    ), !,
    agent_ask_oracle(G,o(N),link,L), 
    include(actor_has_link(L),As,ViableAs), 
    eliminate(ET, ViableAs, A).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    findall(A,actor(A),As), 
    my_agent(G),
    ailp_grid_size(N),
    Energy_threshold is 2 * N, 

    %my_agent(G),
    %get_agent_position(G, P),
    %get_agent_energy(G, E),

    %ailp_grid_size(N),
    %lookup_map(N, Oracles_and_charging_stations),
    %X is ( N * N / 4 ), ceiling(X, Max_energy),

    eliminate(G, Energy_threshold, As, A).

    


% Predicate to loop through values of X and Y within the specified range and collect valid results in a list
lookup_map(N, Map_list) :-
    loop_x(1, N, [], Map_list).

% Loop through values of X from 1 to N
loop_x(X, N, Result_list, Result_list) :- 
    X > N. % Base case: stop when X > N

loop_x(X, N, Accumulator, Result_list) :-
    X =< N,
    lookup_pos(p(X, 1), X_result), 
    loop_y(X, 1, N, Y_results, Valid_Y_results),
    ((X_result = c(ID); X_result = o(ID))
    ->  append([X_result | Valid_Y_results], Temp_list, New_list)
    ;   New_list = Valid_Y_results
    ),
    X_next is X + 1,
    loop_x(X_next, N, [New_list | Accumulator], Result_list).

% Loop through values of Y from 1 to N for a specific value of X
loop_y(_, Y, N, [], []) :- Y > N.
loop_y(X, Y, N, [Y_result | Rest_results], Valid_Y_results) :-
    Y =< N,
    lookup_pos(p(X, Y), Y_result),
    (   (Y_result = c(ID); Y_result = o(ID))
    ->  append([Y_result], Temp_list, New_list)
    ;   New_list = []
    ),
    Y_next is Y + 1,
    loop_y(X, Y_next, N, Rest_results, Tail),
    append(New_list, Tail, Valid_Y_results).