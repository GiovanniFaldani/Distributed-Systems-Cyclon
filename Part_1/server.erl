-module(server).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/2, member/2]).
-import(rand,[uniform/1, uniform/0]).
-import(my_peer, [peer_execution/7, peer_init/2]).
-import(utils, [shuffle_list/1, pick_L_elements/2, remove_pid_list/2, filter_list/2, index_elem/2, remove_duplicates/1]).

-export([run/1]).

% environment setup for the server
run(N) ->
    NPeers = N,
    ViewSize = 5,
    L = 3,
    IsVerbose = true,
    MaxAge = 10,
    TurnDuration = 5000, %ms
    TotalTurns = 10,

    %list containing every peer's pid
    Children = create_n_peers(NPeers, [], IsVerbose),

    %list containing the starting views per every peer
    ListOfPidLists = [create_start_list(Children, MaxAge, ViewSize, CurrentPeer) || CurrentPeer <- Children],

    % Pick 50% of nodes that won't shut down during runtime
    PermaList = [Child || Child <- Children, uniform() < 0.5],

    %List with one list per peer containing its total discoveries
    ServerList = [convert_peer_list(TupleList) || TupleList <- ListOfPidLists],
    initialize_n_peers(NPeers, Children, ListOfPidLists, ViewSize, L, PermaList, TurnDuration, TotalTurns),

    %-1 if made it to the end, otherwise equal of the rounds done
    TurnSinceInactive = [ -1 ||  _ <- Children],

    %List with one list per peer containing its view every turn
    ViewsPerTurn = [[FirstView] || FirstView <- ServerList],

    %List with one list per peer containing the evolution of its total discoveries
    DiscoveredPerTurn = [[length(FirstView) + 1] || FirstView <- ServerList],
    CurrentTurns = [ 0 ||  _ <- Children],

    % initialize list of nodes that have been shut down
    ShutDownList = [],

    start_n_peers(Children),
    server_body(ServerList, Children, TotalTurns, ViewsPerTurn, PermaList, ShutDownList, TurnSinceInactive, DiscoveredPerTurn, CurrentTurns).

% main server loop
server_body(ServerList, Children, TotalTurns, ViewsPerTurn, PermaList, ShutDownList, TurnSinceInactive, DiscoveredPerTurn, CurrentTurns) ->
    receive
        {up_done, PID, NewPeerList} -> % receive peer info at the end of a turn and start the next one
            fwrite(" Update from ~p  \n", [PID]),

            %retrieve old list
            Index = index_pid(PID, Children),
            OldList = nth(Index, ServerList),

            %increase turn i
            ThisTurn = nth(Index, CurrentTurns),
            NewCurrentTurns = update_list_indexed_elem(CurrentTurns, Index, ThisTurn + 1),

            % convert list from list of tuples, get all the peers seen in this turn
            ConvertedList = convert_peer_list(NewPeerList), %gets the List of PIDs of nodes in view

            % update views list
            ListViewsPID = nth(Index, ViewsPerTurn),
            NewListViewsPid = append(ListViewsPID, [ConvertedList]),
            NewViewsPerTurn = update_list_indexed_elem(ViewsPerTurn, Index, NewListViewsPid),

            % keeps only new peers that weren't ever seen before
            NewPeers = filter_list(ConvertedList, OldList),

            % update list of total peers seen by PID
            ListUpdated = remove_duplicates(append(OldList, NewPeers)),
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
                    % compute discovery proportion of all nodes
                    N = length(Children),
                    DiscoveryProps = [length(List)/N || List <- NewListOfLists],
                    fwrite("Discovery proportion of PIDs: ~p\n", [Children]),
                    fwrite("~p\n", [DiscoveryProps]),

                    % compute churn resilience of all nodes that exited
                    %fwrite("ViewsPerTurn: ~p\n", [ViewsPerTurn]),
                    AvgChrunRes = avg_churn_resilience(ShutDownList, Children, TurnSinceInactive, ViewsPerTurn),
                    fwrite("Average Churn Resilience: ~p\n", [AvgChrunRes]),

                    [Peer ! {stop} || Peer <- Children],
                    exit(normal);
                true ->
                    pass
            end,
            server_body(NewListOfLists, Children, TotalTurns, NewViewsPerTurn, PermaList, ShutDownList, TurnSinceInactive, NewDiscoveredPerTurn, NewCurrentTurns);

        {bye, PID} ->
            %update inactive
            Index = index_pid(PID, Children),
            ThisTurn = nth(Index, CurrentTurns),
            NewTurnSinceInactive = update_list_indexed_elem(TurnSinceInactive, Index, ThisTurn),

            % TODO begin check for exit node churn resilience (At every turn, drop a subset of nodes, not random chance for every node)
            NewShutDownList = append(ShutDownList, [PID]),

            server_body(ServerList, Children, TotalTurns, ViewsPerTurn, PermaList, NewShutDownList, NewTurnSinceInactive, DiscoveredPerTurn, CurrentTurns)
    end.

% creates N peer objects
create_n_peers(0, Children, _) -> Children;
create_n_peers(N, Children, IsVerbose) ->
    Child = spawn(my_peer, peer_init, [self(), IsVerbose]),
    create_n_peers(N - 1, append(Children, [Child]), IsVerbose).

% initializes parameters for N peer objects
initialize_n_peers(N, Children, ListOfPidLists, ViewSize, L, PermaList, TurnDuration, TotalTurns) -> 
    initialize_n_peers(0, N, Children, ListOfPidLists, ViewSize, L, PermaList, TurnDuration, TotalTurns).
initialize_n_peers(N, N, _, _, _, _, _,_,_) -> empty;
initialize_n_peers(I, N, Children, ListOfPidLists, ViewSize, L, PermaList, TurnDuration, TotalTurns) ->
    CurrentPeer = nth(I + 1, Children),
    CurrentList = nth(I + 1, ListOfPidLists),
    CurrentPeer ! {init, CurrentList, ViewSize, L, PermaList, TurnDuration, TotalTurns},
    initialize_n_peers(I + 1, N, Children, ListOfPidLists, ViewSize, L, PermaList, TurnDuration, TotalTurns). 

% creates a Tuple List of {PID, Age} for a peer
create_start_list(PidList, MaxAge, ViewSize, CurrentPeer) ->
    List = [ {Pid, uniform(MaxAge)} || Pid <- PidList],
    CleanList = remove_pid_list(CurrentPeer, List),
    sublist(shuffle_list(CleanList), ViewSize).

% begins the first turn in the Cyclon algorithm
start_n_peers(Children) -> [Peer ! {turn, 0} || Peer <- Children].

% change the Tuple at the specified index of a list with the Elem Tuple
update_list_indexed_elem(List, Index, Elem) -> update_list_indexed_elem(List, Index, Elem, 1, []).  
update_list_indexed_elem(List, _, _, I, NewList) when I =:= length(List) + 1 -> NewList;
update_list_indexed_elem(List, Index, Elem, I, NewList) when I =:= Index -> update_list_indexed_elem(List, Index, Elem, I + 1, append(NewList, [Elem]));
update_list_indexed_elem(List, Index, Elem, I, NewList) -> update_list_indexed_elem(List, Index, Elem, I + 1, append(NewList, [nth(I, List)])).

% returns the index in a Tuple List of the Tuple identified by ID = Elem
index_pid(Elem, List) -> index_pid(Elem, List, 1).
index_pid(_, [], _) -> 0;
index_pid(Elem, [PID | _], I) when Elem =:= PID -> I;
index_pid(Elem, [_ | T], I) -> index_pid(Elem, T, I+1).

% returns a list of PIDs in the Tuple List
convert_peer_list(PeerList) -> [ Pid || {Pid , _ } <- PeerList].

% true if a peer has finished all its turns
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

%functions to compute average churn resilience
turns_to_get_out(PID, Turn, ViewList) ->
    if 
        Turn =< length(ViewList) ->
            IsInView = member(PID, nth(Turn, ViewList)),
            if 
                IsInView ->
                turns_to_get_out(PID, Turn+1, ViewList);
            true ->
                Turn
            end;
        true ->
            0
    end.

compute_churn_resilience_node(_, _, []) -> empty;
compute_churn_resilience_node(PID, Turn, ViewsPerTurn) -> 
    N = length(ViewsPerTurn),
    Thresh = 0.75 * N,
    compute_churn_resilience_node(PID, Turn, ViewsPerTurn, 0, N, Thresh).

compute_churn_resilience_node(_, _, [], I, N, Thresh) -> I / N;
compute_churn_resilience_node(PID, Turn, [H|T], I, N, Thresh) ->
    compute_churn_resilience_node(PID, Turn, T, I + turns_to_get_out(PID, Turn, H), N, Thresh).

avg_churn_resilience([], _, _, _) -> empty;
avg_churn_resilience(ShutDownList, Children, TurnSinceInactive, ViewsPerTurn) -> avg_churn_resilience(ShutDownList, Children, TurnSinceInactive, ViewsPerTurn, 0, length(ShutDownList)).

avg_churn_resilience([], _, _, _, Accumulator, Length) -> Accumulator / Length;
avg_churn_resilience([H|T], Children, TurnSinceInactive, ViewsPerTurn, Accumulator, Length) ->
    Index = index_pid(H, Children),
    Turn = nth(Index, TurnSinceInactive),
    Churn_res_current = compute_churn_resilience_node(H, Turn, ViewsPerTurn),
    avg_churn_resilience([H|T], Children, TurnSinceInactive, ViewsPerTurn, Accumulator + Churn_res_current, Length).