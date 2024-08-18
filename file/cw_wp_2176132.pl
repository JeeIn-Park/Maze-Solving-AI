% True if link L appears on A's Wikipedia page
actor_has_link(L, A) :- 
    actor(A), wp(A, WT), wt_link(WT, L).

% Attempt to solve by visiting each oracle in ID order
% eliminate(Agent, Energy_threshild, Actor_list, Visited_oracle, Result) 
eliminate(_, _, [A], _, A) :- 
    format('found identity : ~w~n', [A]), !.
eliminate(G, ET, [A|As], Visited_oracle, Result) :-
    format(' --- agent is exploring... ~n'),
    get_agent_energy(G, E),
    (
        (E =< ET ->
            solve_task(find(c(_)), _), 
            format(' --- charging ~n')
        ;
            solve_task(find(o(ID)), _),
            format(' --- asking ~n'),
            agent_ask_oracle(G, o(ID), link, L),
            format('--- link : ~w~n', [L]), 
            include(actor_has_link(L), [A|As], New_As)
        ),
        eliminate(G, ET, New_As, [ID|Visited_oracle], Result)
    ).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    format('going to check agent identity... ~n'),
    findall(A, actor(A), As), 
    format('possible identity list : ~w~n', [As]),
    my_agent(G),
    ailp_grid_size(N),
    Energy_threshold is 2 * N, 
    format('identity checking just started. ~n'),
    eliminate(G, Energy_threshold, As, [], A).
