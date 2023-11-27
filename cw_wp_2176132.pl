% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).


% Check through the map order the oracles 

% Attempt to solve by visiting each oracle in ID order
eliminate(As,A,K) :- 
    As=[A], !
    ;
    solve_task(find(o(K)),_), !,
    my_agent(N),
    agent_ask_oracle(N,o(K),link,L), 
    include(actor_has_link(L),As,ViableAs), 
    K1 is K+1, 
    eliminate(ViableAs,A,K1).

% Deduce the identity of the secret actor A
find_identity(A) :- 
    findall(A,actor(A),As), eliminate(As,A,1).




    candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A) :-
    (   part_module(2)
    ->  find_identity_2(A), !
    ;   otherwise
    ->  find_identity_o(A), !
    ).

% loop through actors, and their list of links and compare
% do this until only one actor remains
find_identity_2(A):-
  agent_ask_oracle(oscar, o(1), link, L),
  bagof(X, actor(X), Xs),
  find_identity_2(A, [], Xs, L).
find_identity_2(X, [A|[]], [], Lt):-
  X = A.
find_identity_2(X, As, [], Lt):-
  agent_ask_oracle(oscar, o(1), link, L),
  find_identity_2(X, [], As, L).
find_identity_2(X, As, [B|Bs], Lt):-
  wp(B, WT),
  bagof(L, wt_link(WT, L), Ls),
  (
    memberchk(Lt, Ls) -> find_identity_2(X, [B|As], Bs, Lt)
  ; otherwise -> find_identity_2(X, As, Bs, Lt)
  ).

getPathCost(S, [], CB, CB).
getPathCost(S, [(X| Pos)|Rest], (Y, Path), FB):-
  (
    my_agent(A), \+query_world(agent_check_oracle, [A, X]) ->
      solve_task_bt(go(Pos), [[c(0,0,S), []]], _, _, _, _, _, BackPath),!,
      write(found_path),nl,
      length(Path, L1),
      length(BackPath, L2),
      (
        Y = o(-1) -> getPathCost(S, Rest, (X, BackPath), FB)
      ; L1 > L2 -> getPathCost(S, Rest, (X, BackPath), FB)
      ; otherwise -> getPathCost(S, Rest, (Y, Path), FB)
      )
  ; otherwise -> getPathCost(S, Rest, (Y, Path), FB)
  ).

getPathCCost(S, [], CB, CB).
getPathCCost(S, [(X| Pos)|Rest], (Y, Path), FB):-
  solve_task_bt(go(Pos), [[c(0,0,S), []]], _, _, _, _, _, BackPath),!,
  write(found_path),nl,
  length(Path, L1),
  length(BackPath, L2),
  (
    Y = o(-1) -> getPathCCost(S, Rest, (X, BackPath), FB)
  ; L1 > L2 -> getPathCCost(S, Rest, (X, BackPath), FB)
  ; otherwise -> getPathCCost(S, Rest, (Y, Path), FB)
  ).

add(X, Y, Z, A):-
  A is X + Y + Z.

goToNearOracle_FromCharge(S,O,C,L):-
  write('near oracle from charge'),nl,
  getPathCost(S, O, (o(-1), []), CB),
  write('Got path'),nl,
  CB = (ID,Path),
  Path = [End|Tail],
  write(CB),nl,
  my_agent(Agent),
  reverse(Path,[_Init|P]),
  query_world( agent_do_moves, [Agent,P] ),
  write('asking '),write(ID),nl,
  query_world( agent_ask_oracle, [Agent, ID, link, L]),
  write(L),nl.

% this finds nearest oracle, checks you can go there with ur
% energy, then gets a link from that oracle
goToNearOracle(S, O, C, L):-
  getPathCost(S, O, (o(-1), []), CB),
  CB = (ID,Path),
  Path = [End|Tail],
  write(CB),nl,
  getPathCCost(End, C, (o(-1), []), CB2),
  CB2 = (ID2,Path2),
  write(CB2),nl,
  length(Path, L1),
  length(Path2, L2),
  my_agent(Agent),
  query_world(agent_current_energy, [Agent, E]),
  add(L1,L2,10,LT),!,
  write(LT),nl,
  (
    LT < E -> reverse(Path,[_Init|P]),
              query_world( agent_do_moves, [Agent,P] ),
              write('asking '),write(ID),nl,
              query_world( agent_ask_oracle, [Agent, ID, link, L]),
              write(L),nl
  ; otherwise ->  getPathCost(S, C, (o(-1),[]), (CS,CSPath)),
                  reverse(CSPath,[_Init|P]),
                  query_world( agent_do_moves, [Agent,P]),
                  write('charging at '),write(c(CS)),nl,
                  query_world(agent_topup_energy, [Agent,CS]),
                  write('successfully charged'),nl,
                  CSPath = [Location|PrevPath],
                  goToNearOracle_FromCharge(Location, O, C, L)
  ).

find_identity_o(A, [A|[]], [], _, _, _).
% repeat the link function thing here
find_identity_o(X, As, [], Lt, O, C):-
  my_agent(Agent),
  query_world(agent_current_position, [Agent, S]),
  goToNearOracle(S, O, C, L),
  find_identity_o(X, [], As, L, O, C).
find_identity_o(X, As, [B|Bs], Lt, O, C):-
  wp(B, WT),
  bagof(L, wt_link(WT, L), Ls),
  (
    memberchk(Lt, Ls) -> find_identity_o(X, [B|As], Bs, Lt, O, C)
  ; otherwise -> find_identity_o(X, As, Bs, Lt, O, C)
  ).

find_identity_o(A):-
  preprocess(Oracles, Stations),
  my_agent(Agent),
  query_world(agent_current_position, [Agent, S]),
  bagof(X, actor(X), Actors),
  write('predone'),nl,
  goToNearOracle(S, Oracles, Stations, L),
  find_identity_o(A, [], Actors, L, Oracles, Stations).

flood_check([],_,_,_,Acc,Acc).
flood_check(Targets,Visited,LastRound,[],Acc,Result) :-
  append(LastRound, Visited, NewVisited),
  flood_expand(Targets,NewVisited,LastRound,[],Acc,Result).
flood_check(Targets,Visited,LastRound,Remaining,Acc,Result) :-
  Remaining = [Cell|Rest],
  flood_check_targets(Targets,Cell,Acc,NewAcc,Targets,NewTargets),
  flood_check(NewTargets,Visited,LastRound,Rest,NewAcc,Result).

flood_check_targets([],_,PAcc,PAcc,TDec,TDec).
flood_check_targets(ToCheck,Cell,PAcc,PResult,TDec,TResult) :-
  ToCheck = [Target|Rest],
  (
    map_adjacent(Cell,_,Target) -> write('Found: '),write(Target),nl,delete(TDec,Target,NewTDec), flood_check_targets(Rest,Cell,[(Target|Cell)|PAcc],PResult,NewTDec,TResult)
  ; otherwise -> flood_check_targets(Rest,Cell,PAcc,PResult,TDec,TResult)
  ).

flood_expand(Targets,Visited,[],NewArea,Acc,Result) :-
  %remove duplicates
  sort(NewArea, X),
  flood_check(Targets,Visited,X,X,Acc,Result).
flood_expand(Targets,Visited,Remaining,NewArea,Acc,Result) :-
  Remaining = [Cell|Rest],
  findall(P, (map_adjacent(Cell,P,empty), \+ memberchk(P,Visited)), Ps),
  append(Ps, NewArea, NewNewArea),
  flood_expand(Targets,Visited,Rest,NewNewArea,Acc,Result).

content_search(Targets, Results):-
  my_agent(A),
  query_world(agent_current_position, [A, P]),
  flood_check(Targets,[],[P],[P],[],Results).

is_oracle((o(_)|_)).
is_charging((c(_)|_)).
preprocess(Oracles, Charging):-
  set_prolog_stack(global, limit(100 000 000 000)),
  content_search([o(1),o(2),o(3),o(4),o(5),o(6),o(7),o(8),o(9),o(10),c(1),c(2)], Items),!,
  include(is_oracle, Items, Oracles),
  include(is_charging, Items, Charging).