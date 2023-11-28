% True if link L appears on A's Wikipedia page
actor_has_link(L, T) :- 
    actor(T), wp(T, WT), wt_link(WT, L).

% Attempt to solve by visiting each oracle in ID order
eliminate(_, _, [T], _, T) :- 
    !, 
    format('found identity : ~w~n', [T]).
eliminate(G, ET, [T|Ts], Visited_oracle, Result) :-
    format(' --- agent is exploring... '),
    get_agent_energy(G, E),
    (
        (E =< ET ->
            solve_task(find(c(_)), _), 
            format(' --- charging ')
        ;
            solve_task(find(o(ID)), _), %<- I want to make sure ID is not already in Visited oracle
            format(' --- asking '),
            agent_ask_oracle(G, o(ID), link, L),
            format('--- link : ~w', [L]), % Added square brackets around L
            actor_has_link(L, T),
            format('agent was not : ~w~n', [T])
        ),
        
        eliminate(G, ET, Ts, [N|Visited_oracle], Result)
    ).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    format('going to check agent identity... ~n'),
    findall(T, actor(T), Ts), 
    format('possible identity list : ~w~n', [Ts]),
    my_agent(G),
    ailp_grid_size(N),
    Energy_threshold is 2 * N, 
    format('identity checking just started. ~n'),
    eliminate(G, Energy_threshold, Ts, [], A).
