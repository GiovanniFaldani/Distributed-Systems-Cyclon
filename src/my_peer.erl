-module(my_peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, delete/2, search/2, max/1]).
-import(rand,[uniform/0]).

-export([peer_execution/6]).

peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose) ->
    receive
        {turn} ->
            if
                IsVerbose ->
                    fwrite("Node ~p \n", [self()]);
                true ->
                    pass
            end,
            
            NewPeerList = [{PID, Age + 1} || {PID, Age} <- PeerList],

            Q = list_max_elem(PeerList),
            reset_q_age(NewPeerList, Q),

            ListForQ = pick_L_elements(PeerList, L),
            Q ! {req_view, ListForQ},

            Server ! {req_q, Q, self()},
            peer_execution(Server, IsActive, NewPeerList, ViewSize, L, IsVerbose);

        {req_view, ReqList, PID} ->
            if
                IsVerbose ->
                    fwrite("Request from ~p \n", [PID]);
                true ->
                    pass
            end,

            ReplyList = pick_L_elements(PeerList, L),
            PID ! {rep_view, ReplyList},

            Server ! {rep_q, PID, self()},
            peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose);

        {rep_view, RepList} ->
            if
                IsVerbose ->
                    fwrite("Reply from Q \n");
                true ->
                    pass
            end,

            update_list(PeerList, RepList, ViewSize),
            Server ! {ud_done, self()},
            peer_execution(Server, IsActive, NewPeerList, ViewSize, L, IsVerbose)
    end.
    
    %TELL SERVER WHO IS Q
    
    %peer_execution(Server, ThisPeer, IsActive, NewPeerList, ViewSize, L, IsVerbose).
        
    %FUN 2
    %RECEIVE RESPONSE FROM Q
    
    %FUN 3
    %RECEIVE REQUEST FOR PEERLIST AS Q
    %SEND A MESSAGE TO RAND AND TELL SERVER WHO HE IS

    %FUN 4
    %RECEIVE A MESSAGE (RANDOM NOT IN ALGORITHM)
    

   

%https://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-randomly-re-arrange-list-elements

shuffle_list(List) -> [{X, Y}||{_, X, Y} <- sort([ {uniform(), N, Age} || {N, Age} <- List])].

pick_L_elements(List, L) -> sublist(shuffle_list(List), L).

list_max_elem(List) -> 
    element(1,
        element(2,
            lists:search(fun(X) -> element(2,X) =:= lists:max([F || {_,F} <- List]) end, 
            List)
        )
    ).

list_max_index(List) -> 
    element(1,
        element(2,
            search(
                fun(X) -> element(2,X) =:= max([F || {_,F} <- List]) end, 
                List
            )
        )
    ).

index_elem(Elem, List) -> index_elem(Elem, List, 1).
index_elem(Elem, [], i) -> empty.
index_elem(Elem, [{PID, Age} | T], i) when Elem =:= PID -> i.
index_elem(Elem, [_ | T], i) -> index_elem(Elem, T, i+1).



reset_q_age(List, Q) -> [  || {ID, Age} <- List]


update_list(List, CompareList, Max) ->
    [ CompareList = delete(N, CompareList) || N <- List],
    
    NumToAdd = length(CompareList),
    CurrentNum = length(List),

    if
        NumToAdd > Max - CurrentNum ->
            remove_n_max(List, N);
        true ->
            pass
    end,

    append([[List], [CompareList]]).

remove_n_max()



