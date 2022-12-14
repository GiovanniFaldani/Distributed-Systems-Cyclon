-module(my_peer).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, delete/2, search/2, max/1]).
-import(rand,[uniform/0]).
-import(timer,[send_after/2]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, reset_peer_age/2, 
    list_max_tuple/1, index_elem/2, update_list/3, remove_n_max/2]).


-export([peer_execution/9]).
-export([peer_init/2]).

peer_init(Server, IsVerbose) ->
     if
        IsVerbose ->
            fwrite("~p: Created\n", [self()]);
        true ->
            pass
    end,

    receive
        {init, IsActive, PeerList, ViewSize, L, Turn, IsDone} ->
            if
                IsVerbose ->
                    fwrite("~p: Initialized\n", [self()]);
                true ->
                    pass
            end,

            peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose, 0, Turn, IsDone)
    end.


peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose, Timer, Turn, IsDone) ->
    receive
        {turn, OldQ} ->
            if
                IsVerbose ->
                    fwrite("~p: New Turn\n", [self()]);
                true ->
                    pass
            end,
            
            if
                not IsDone ->
                    PeerList = delete({OldQ, 0}, PeerList);
                true -> 
                    pass
            end,

            if
                Turn =:= 10 ->
                    exit(normal);
                true->
                    puss
            end,

            NewPeerList = [{PID, Age + 1} || {PID, Age} <- PeerList],

            Q = list_max_id(PeerList),
            reset_peer_age(NewPeerList, Q),

            ListForQ = pick_L_elements(PeerList, L),
            Q ! {req_view, ListForQ},
            
            {_, Timer} = send_after(10000, {turn, Q}),

            peer_execution(Server, IsActive, NewPeerList, ViewSize, L, IsVerbose, Timer, 1, false);

        {req_view, ReqList, PID} ->
            if
                IsVerbose ->
                    fwrite("~p: Request from ~p \n", [self(), PID]);
                true ->
                    pass
            end,

            ReplyList = pick_L_elements(PeerList, L),
            PID ! {rep_view, ReplyList},
            peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose, Timer, Turn, IsDone);

        {rep_view, RepList} ->
            if
                IsVerbose ->
                    fwrite("~p: Reply from Q \n", [self()]);
                true ->
                    pass
            end,

            NewPeerList = update_list(PeerList, RepList, ViewSize),
            Server ! {ud_done, self()},

            TossCoin = uniform(),
            if
                TossCoin > 0.5 ->
                    send_rand_message(NewPeerList, self());
                true ->
                    pass
            end,

            peer_execution(Server, IsActive, NewPeerList, ViewSize, L, IsVerbose, Timer, Turn, true);

        {ciao, SenderId} ->
            if
                IsVerbose ->
                    fwrite("~p: Message from ~p\n", [self(), SenderId]);
                true ->
                    pass
            end,

            peer_execution(Server, IsActive, PeerList, ViewSize, L, IsVerbose, Timer, Turn, IsDone)
    end. 

send_rand_message(List, ThisID) ->
    RandomId = element(1, nth(1, shuffle_list(List))),
    RandomId ! {ciao, ThisID}.