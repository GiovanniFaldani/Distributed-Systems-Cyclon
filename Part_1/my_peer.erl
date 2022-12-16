-module(my_peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, delete/2, search/2, max/1]).
-import(rand,[uniform/0]).
-import(timer,[send_after/2]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, reset_peer_age/2, 
    list_max_tuple/1, index_elem/2, remove_n_max/2, remove_pid_list/2, filter_list/2]).


-export([peer_execution/9]).
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
        {init, PeerList, ViewSize, L, TurnDuration, TotalTurns} ->
            if
                IsVerbose ->
                    fwrite("~p: Initialized\n", [self()]);
                true ->
                    pass
            end,

            peer_execution(Server, PeerList, ViewSize, L, IsVerbose, 0, TurnDuration, TotalTurns, true)
    end.

% main loop for the peer nodes
peer_execution(Server, PeerList, ViewSize, L, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone) ->
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
            
            if
                Turn =:= TotalTurns ->
                    peer_execution(Server, PeerList, ViewSize, L, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);
                true->
                    pass
            end,

            
            % check if he stops
            Probability = uniform(),
            if
                 Probability < 0.1 -> % 10% probability to shut down (churn)
                    Server ! {bye, self()},
                    fwrite("~p: Bye \n", [self()]),
                    exit(self(), normal);
                true ->
                    pass
            end,

            NewPeerList = [{PID, Age + 1} || {PID, Age} <- ListAfterQ], % update age (BULLET 2)
           
            Q = list_max_id(NewPeerList),
            reset_peer_age(NewPeerList, Q),

            ListForQ = pick_L_elements(NewPeerList, L),

            Q ! {req_view, ListForQ, self()},
            send_after(TurnDuration, {turn, Q}),

            peer_execution(Server, NewPeerList, ViewSize, L, IsVerbose, Turn + 1, TurnDuration, TotalTurns, false);

        {req_view, ReqList, PID} ->
            if
                IsVerbose ->
                    fwrite("~p: Request from ~p \n", [self(), PID]);
                true ->
                    pass
            end,

            ReplyList = pick_L_elements(PeerList, L),
            PID ! {rep_view, ReplyList},
            peer_execution(Server, PeerList, ViewSize, L, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);

        {rep_view, RepList} ->
            if
                IsVerbose ->
                    fwrite("~p: Reply from Q \n", [self()]);
                true ->
                    pass
            end,

            NewPeerList = update_list(PeerList, RepList, ViewSize),

            TossCoin = uniform(),
            if
                TossCoin > 0.5 ->
                    send_rand_message(NewPeerList, self());
                true ->
                    pass
            end,

            peer_execution(Server, NewPeerList, ViewSize, L, IsVerbose, Turn, TurnDuration, TotalTurns, true);

        {ciao, SenderId} ->
            if
                IsVerbose ->
                    fwrite("~p: Message from ~p\n", [self(), SenderId]);
                true ->
                    pass
            end,

            peer_execution(Server, PeerList, ViewSize, L, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);
        {stop} ->
            exit(normal)

    end. 

% updates the Tuple List to remove entries alread in view or pointing to self
update_list(List, CompareList, Max) ->
    FilteredList = filter_list(CompareList, List),
    ListCleaned = remove_pid_list(self(), FilteredList),

    %add removal of self()
    NumToAdd = length(ListCleaned),
    CurrentNum = length(List),
    if
        NumToAdd > Max - CurrentNum ->
            ListFreed = remove_n_max(List, NumToAdd - Max + CurrentNum);
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
