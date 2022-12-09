-module(peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2]).
-import(random,[uniform/0])

-export([peer_exectuion/7]).

peer_execution(Server, ThisPeer, IsActive, PeerList, ViewSize, L, IsVerbose) ->
    receive
        %FUN 1
        {turn} ->
            if
                IsVerbose ->
                    fwrite("Node ~p \n", [self()])
                true ->
                    pass
            end,
            
            NewPeerList = [{PID, Age + 1} || {PID, Age} <- PeerList],

            %FIND MAX
            Q = list_max_index()

            %SEND PACKET TO Q with l-1 entries

            %TELL SERVER WHO IS Q
            
            peer_execution(Server, ThisPeer, IsActive, NewPeerList, ViewSize, L, IsVerbose);

        %FUN 2
        %RECEIVE RESPONSE FROM Q
        
        %FUN 3
        %RECEIVE REQUEST FOR PEERLIST AS Q
        %SEND A MESSAGE TO RAND AND TELL SERVER WHO HE IS

        %FUN 4
        %RECEIVE A MESSAGE (RANDOM NOT IN ALGORITHM)



end.


list_max_index(I, H, List) -> I

list_max([]   ) -> empty;
list_max([H|T]) -> {ok, list_max(H, T)}.

list_max(1, X, []) -> X;
list_max(X, [H|T]) when X < H -> list_max(H, T);
list_max(X, [_|T]) -> list_max(X, T).