-module(my_peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2]).
-import(rand,[uniform/0]).

-export([peer_execution/6]).

peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose) ->

    
    %FUN 1
    if
        IsVerbose ->
            fwrite("Node ~p \n", [self()]);
        true ->
            pass
    end,

    
    
    NewPeerList = [{PID, Age + 1} || {PID, Age} <- PeerList],

    fwrite("NewList ~p \n", [NewPeerList]),
    %FIND MAX
    Q = list_max_index(PeerList),

    fwrite("Q: ~p\n", [Q]),

    %SEND PACKET TO Q with l-1 entries
    ListForQ = pick_L_elements(PeerList, L),

    fwrite("List for q ~p\n", [ListForQ]).
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

list_max_index(List) -> 
    element(1,
        element(2,
            lists:search(fun(X) -> element(2,X) =:= lists:max([F || {_,F} <- List]) end, 
            List)
        )
    ).