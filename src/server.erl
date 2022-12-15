-module(server).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/2]).
-import(rand,[uniform/1, uniform/0]).
-import(my_peer, [peer_execution/7, peer_init/2]).
-import(utils, [shuffle_list/1, remove_pid_list/2, filter_list/2, index_elem/2]).

-export([run/1]).

run(N) ->
    NPeers = N,
    ViewSize = 5,
    L = 3,
    IsVerbose = true,
    MaxAge = 10,
    TotalTurns = 10,

    %list cointaing every peer's pid
    Children = create_n_peers(NPeers, [], IsVerbose),

    %list containing the starting views per every peer
    ListOfPidLists = [create_start_list(Children, MaxAge, ViewSize, CurrentPeer) || CurrentPeer <- Children],

    %List with one list per peer containing its total discoveries
    ServerList = [convert_peer_list(TupleList) || TupleList <- ListOfPidLists],
    initialize_n_peers(NPeers, Children, ListOfPidLists, ViewSize, L, TotalTurns),

    %-1 if made it to the end, otherwise equal of the rounds done
    TurnSinceInactive = [ -1 ||  _ <- Children],

    %List with one list per peer containing its view every turn
    ViewsPerTurn = [[FirstView] || FirstView <- ServerList],

    %List with one list per peer containing the evolution of its total discoveries
    DiscoveredPerTurn = [[length(FirstView) + 1] || FirstView <- ServerList],
    CurrentTurns = [ 0 ||  _ <- Children],

    start_n_peers(Children),
    server_body(ServerList, Children, TotalTurns, ViewsPerTurn, TurnSinceInactive, DiscoveredPerTurn, CurrentTurns).


server_body(ServerList, Children, TotalTurns, ViewsPerTurn, TurnSinceInactive, DiscoveredPerTurn, CurrentTurns) ->
    receive
        {up_done, PID, NewPeerList} ->
            fwrite(" Update from ~p:  \n", [PID]),

            %retrieve old list
            Index = index_pid(PID, Children),
            OldList = nth(Index, ServerList),

            %increase turn i
            ThisTurn = nth(Index, CurrentTurns),
            NewCurrentTurns = update_list_indexed_elem(CurrentTurns, Index, ThisTurn + 1),

            %convert list from list of tuples
            ConvertedList = convert_peer_list(NewPeerList),

            %update views list
            ListViewsPID = nth(Index, ViewsPerTurn),
            NewListViewsPid = append(ListViewsPID, [ConvertedList]),
            NewViewsPerTurn = update_list_indexed_elem(ViewsPerTurn, Index, NewListViewsPid),

            %keeps only new items
            NewPeers = filter_list(ConvertedList, OldList),

            %update list
            ListUpdated = append(OldList, NewPeers),
            NewListOfLists = update_list_indexed_elem(ServerList, Index, ListUpdated),

            %update discovery list
            DiscoveredByPID = nth(Index, ViewsPerTurn),
            NewTotal = length(ListUpdated),
            NewDiscoveredByPID = append(DiscoveredByPID, NewTotal),
            NewDiscoveredPerTurn = update_list_indexed_elem(DiscoveredPerTurn, Index, NewDiscoveredByPID),

            HasEnded = check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, length(Children)),

            if
                HasEnded ->
                    fwrite("All Done \n"),
                    [Peer ! {stop} || Peer <- Children],
                    exit(normal);
                true ->
                    pass
            end,
            server_body(NewListOfLists, Children, TotalTurns, NewViewsPerTurn, TurnSinceInactive, NewDiscoveredPerTurn, NewCurrentTurns);

        {bye, PID} ->
            %update inactive
            Index = index_pid(PID, Children),
            ThisTurn = nth(Index, CurrentTurns),
            NewTurnSinceInactive = update_list_indexed_elem(TurnSinceInactive, Index, ThisTurn),

            server_body(ServerList, Children, TotalTurns, ViewsPerTurn, NewTurnSinceInactive, DiscoveredPerTurn, CurrentTurns)
    end.


create_n_peers(0, Children, _) -> Children;
create_n_peers(N, Children, IsVerbose) ->
    Child = spawn(my_peer, peer_init, [self(), IsVerbose]),
    create_n_peers(N - 1, append(Children, [Child]), IsVerbose).


initialize_n_peers(N, Children, ListOfPidLists, ViewSize, L, TotalTurns) -> initialize_n_peers(0, N, Children, ListOfPidLists, ViewSize, L, TotalTurns).
initialize_n_peers(N, N, _, _, _, _, _) -> empty;
initialize_n_peers(I, N, Children, ListOfPidLists, ViewSize, L, TotalTurns) ->
    CurrentPeer = nth(I + 1, Children),
    CurrentList = nth(I + 1, ListOfPidLists),
    CurrentPeer ! {init, CurrentList, ViewSize, L, TotalTurns},
    initialize_n_peers(I + 1, N, Children, ListOfPidLists, ViewSize, L, TotalTurns). 


create_start_list(PidList, MaxAge, ViewSize, CurrentPeer) ->
    List = [ {Pid, uniform(MaxAge)} || Pid <- PidList],
    CleanList = remove_pid_list(CurrentPeer, List),
    sublist(shuffle_list(CleanList), ViewSize).


start_n_peers(Children) -> [Peer ! {turn, 0} || Peer <- Children].


update_list_indexed_elem(List, Index, Elem) -> update_list_indexed_elem(List, Index, Elem, 1, []).  
update_list_indexed_elem(List, _, _, I, NewList) when I =:= length(List) + 1 -> NewList;
update_list_indexed_elem(List, Index, Elem, I, NewList) when I =:= Index -> update_list_indexed_elem(List, Index, Elem, I + 1, append(NewList, [Elem]));
update_list_indexed_elem(List, Index, Elem, I, NewList) -> update_list_indexed_elem(List, Index, Elem, I + 1, append(NewList, [nth(I, List)])).


index_pid(Elem, List) -> index_pid(Elem, List, 1).
index_pid(_, [], _) -> 0;
index_pid(Elem, [PID | _], I) when Elem =:= PID -> I;
index_pid(Elem, [_ | T], I) -> index_pid(Elem, T, I+1).


convert_peer_list(PeerList) -> [ Pid || {Pid , _ } <- PeerList].


check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, N) -> check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, N, 1).
check_if_end(_, _, _, N, I) when I =:=N + 1 -> true;
check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, N, I) ->
    IsActive = nth(I, TurnSinceInactive),
    CurrentTurn = nth(I, CurrentTurns),

    if
        IsActive =/= -1 ->
            check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, N, I + 1);
        CurrentTurn =:= TotalTurns ->
            check_if_end(TotalTurns, TurnSinceInactive, CurrentTurns, N, I + 1);
        true ->
            false
    end.
    
    

