-module(utils).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, append/2, delete/2, search/2, max/1]).
-import(rand,[uniform/0]).
-import(timer,[send_after/2]).

-export([shuffle_list/1]).
-export([pick_L_elements/2]).
-export([list_max_id/1]).
-export([list_max_tuple/1]).
-export([reset_peer_age/2]).
-export([index_elem/2]).
-export([remove_n_max/2]).
-export([remove_pid_list/2]).
-export([filter_list/2]).



%https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-randomly-re-arrange-list-elements

shuffle_list(List) -> [{X, Y}||{_, X, Y} <- sort([ {uniform(), N, Age} || {N, Age} <- List])].

pick_L_elements(List, L) -> sublist(shuffle_list(List), L).

list_max_id(List) -> element(1,list_max_tuple(List)).

list_max_tuple(List) -> 
        element(2,
            lists:search(fun(X) -> element(2,X) =:= lists:max([F || {_,F} <- List]) end, List)
        ).

reset_peer_age(TupleList, Q) -> append([{ID, Age} || {ID, Age} <- TupleList, ID =/= Q], [{Q,0}]).

index_elem(Elem, List) -> index_elem(Elem, List, 1).
index_elem(_, [], _) -> 0;
index_elem(Elem, [{PID,_} | _], I) when Elem =:= PID -> I;
index_elem(Elem, [_ | T], I) -> index_elem(Elem, T, I+1).


remove_n_max(List, 0) -> List;
remove_n_max(List, N) ->
    TupleToDelete = list_max_tuple(List),
    NewList = delete(TupleToDelete, List),
    remove_n_max(NewList, N-1).


remove_pid_list(Pid, List) ->
    Index = index_elem(Pid, List),
    remove_index_list(Index, List).
    
remove_index_list(Index, List) when Index =:= 0 -> List;
remove_index_list(Index, List)->
    Tuple = nth(Index, List),
    delete(Tuple, List).

%removes from the first list all the elements contained in the second
filter_list([], _) -> [];
filter_list(List, []) -> List;
filter_list(List, [H | T]) -> filter_list(delete(H, List), T).
    
