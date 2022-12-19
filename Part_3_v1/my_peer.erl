-module(my_peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, delete/2, search/2, max/1, member/2]).
-import(rand,[uniform/0]).
-import(timer,[send_after/2]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, reset_peer_age/2, 
    list_max_tuple/1, index_elem/2, remove_n_max/2, remove_pid_list/2, filter_list/2, filter_tuple_list/2, remove_duplicates/1]).
-import(nega_byzantine, [check_old_lists/2]).


-export([peer_execution/11]).
-export([peer_init/2]).

% initialize a peer node 
peer_init(Server, IsVerbose) ->
     if
        IsVerbose ->
            fwrite("~p: Created\n", [self()]);
        true ->
            pass
    end,

    receive
        {init, PeerList, ViewSize, L, PermaList, TurnDuration, TotalTurns} ->
            if
                IsVerbose ->
                    fwrite("~p: Initialized\n", [self()]);
                true ->
                    pass
            end,

            peer_execution(Server, PeerList, ViewSize, L, PermaList, IsVerbose, 0, TurnDuration, TotalTurns, false, [])
    end.

% main loop for the peer nodes
peer_execution(Server, PeerList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone, OldQLists) ->
    receive
        {turn, OldQ} ->
            if
                IsVerbose ->
                    fwrite("~p: Turn ~p\n", [self(), Turn]);
                true ->
                    pass
            end,

            % remove node if it's unresponsive
            ListAfterQ = remove_old_q(OldQ, PeerList, IsDone),
            Server ! {up_done, self(), ListAfterQ},
            
            % finish loop if all turns are done
            if
                Turn =:= TotalTurns ->
                    peer_execution(Server, PeerList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone, OldQLists);
                true->
                    pass
            end,

            
            % check if it exits the active node set (churn simulation)
            Probability = uniform(),
            IsSafe = member(self(), PermaList),
            if
                (Probability < 0.1) and not (IsSafe) -> % unsafe nodes can shut down with 10% prob
                    Server ! {bye, self()},
                    fwrite("~p: Bye \n", [self()]),
                    exit(self(), normal);
                true ->
                    pass
            end,

            % 1 - INCREASE BY ONE THE AGE OF ALL NODES
            NewPeerList = [{PID, Age + 1} || {PID, Age} <- ListAfterQ], % update age (BULLET 2)
           
            % 2 -SELECT OLDER NODE Q AND L-1 OTHER RANDOM ENTRIES OF THE TABLE
            Q = list_max_id(NewPeerList),
            ListForQ = pick_L_elements(NewPeerList, L),

            % 3 - RESET AGE OF Q TO 0
            reset_peer_age(NewPeerList, Q),

            % 4 - SEND L-1 NODES IN VIEW TO Q
            Q ! {req_view, ListForQ, self()},
            send_after(TurnDuration, {turn, Q}),

            peer_execution(Server, NewPeerList, ViewSize, L, PermaList, IsVerbose, Turn + 1, TurnDuration, TotalTurns, false, OldQLists);

        {req_view, ReqList, PID} ->
            if
                IsVerbose ->
                    fwrite("~p: Request from ~p \n", [self(), PID]);
                true ->
                    pass
            end,

            % 5 - Q SENDS BACK A SUBSET OF L-1 NODES IN ITS VIEW

            ReplyList = pick_L_elements(PeerList, L),
            PID ! {rep_view, ReplyList, ReqList},
            peer_execution(Server, PeerList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone, OldQLists);

        {rep_view, RepList, ReqList} ->
            if
                IsVerbose ->
                    fwrite("~p: Reply from Q \n", [self()]);
                true ->
                    pass
            end,

            % 6 AND 7 - RECEIVE ENTRIES FROM Q AND UPDATE OWN LIST
            check_old_lists(RepList, OldQLists),

            NewPeerList = update_list(PeerList, RepList, ReqList, ViewSize),

            TossCoin = uniform(),
            if
                TossCoin < 0.5 ->
                    send_rand_message(NewPeerList, self());
                true ->
                    pass
            end,

            peer_execution(Server, NewPeerList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, true, OldQLists);

        {ciao, SenderId} ->
            if
                IsVerbose ->
                    fwrite("~p: Message from ~p\n", [self(), SenderId]);
                true ->
                    pass
            end,

            peer_execution(Server, PeerList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone, OldQLists);
        {stop} ->
            fwrite("~p: Bye \n", [self()]),
            exit(normal)

    end. 

% updates the Tuple List to remove entries already in view or pointing to self
update_list(List, CompareList, ReqList, Max) ->
    % reset age of all nodes in the list received from Q
    ZeroedList = [{ID, 0} || {ID, _} <- CompareList],

    %remove elements in common and get new nodes (includes self)
    FilteredList = filter_tuple_list(ZeroedList, List), 

    %add removal of self()
    ListCleaned = remove_pid_list(self(), FilteredList),

    % save new elements over the elements that were originally sent
    NumToAdd = length(ListCleaned),
    CurrentNum = length(List),
    if
        NumToAdd > Max - CurrentNum ->
            %ListFreed = remove_n_max(List, NumToAdd - Max + CurrentNum);
            ListFreed = filter_tuple_list_first_N(List, ReqList, NumToAdd - Max + CurrentNum);
        true ->
            ListFreed = List
    end,
    append([ListFreed, ListCleaned]).

% send a hello message to a random node in the List
send_rand_message(List, ThisID) ->
    RandomId = element(1, nth(1, shuffle_list(List))),
    RandomId ! {ciao, ThisID}.

% delete unresponsive peer from List
remove_old_q(OldQ, List, false) -> delete({OldQ, 0}, List);
remove_old_q(_, List, _) -> List.

% removes from the first list the first N elements of the second list
filter_tuple_list_first_N([], _, _) -> empty;
filter_tuple_list_first_N(TupleList, _, 0) -> TupleList;
filter_tuple_list_first_N(TupleList, [H|T], N) ->
    filter_tuple_list_first_N([{ID, Age} || {ID, Age} <- TupleList, element(1,H) =/= ID], T, N-1).