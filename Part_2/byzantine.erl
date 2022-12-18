-module(byzantine).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/1, delete/2, search/2, max/1, member/2]).
-import(rand,[uniform/0]).
-import(timer,[send_after/2]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, reset_peer_age/2, 
    list_max_tuple/1, index_elem/2, remove_n_max/2, remove_pid_list/2, filter_list/2, filter_tuple_list/2, remove_duplicates/1]).

-export([byz_execution/10]).
-export([byz_init/2]).

byz_init(Server, IsVerbose) ->
     if
        IsVerbose ->
            fwrite("BYZ ~p: Created\n", [self()]);
        true ->
            pass
    end,

    receive
        {init, ByzList, ViewSize, L, PermaList, TurnDuration, TotalTurns} ->
            if
                IsVerbose ->
                    fwrite("BYZ ~p: Initialized\n", [self()]);
                true ->
                    pass
            end,

            byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, 0, TurnDuration, TotalTurns, false)
    end.

% main loop for the peer nodes
byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone) ->
    receive
        {turn, _} ->
            if
                IsVerbose ->
                    fwrite("BYZ ~p: Byzantine Turn ~p\n", [self(), Turn]);
                true ->
                    pass
            end,
            
            % finish loop if all turns are done
            if
                Turn =:= TotalTurns ->
                    byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);
                true->
                    pass
            end,

            byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn + 1, TurnDuration, TotalTurns, false);

        {req_view, ReqList, PID} ->
            if
                IsVerbose ->
                    fwrite("BYZ ~p: Request from ~p \n", [self(), PID]);
                true ->
                    pass
            end,

            % BYZ SENDS BACK THE SET OF BYZANTINE NODES
            PID ! {rep_view, pick_L_elements(ByzList, L), ReqList},
            byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);

        {rep_view, _, _} ->
            if
                IsVerbose ->
                    fwrite("BYZ ~p: Reply from Q \n", [self()]);
                true ->
                    pass
            end,

            byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, true);

        {ciao, SenderId} ->
            if
                IsVerbose ->
                    fwrite("BYZ ~p: Message from ~p\n", [self(), SenderId]);
                true ->
                    pass
            end,

            byz_execution(Server, ByzList, ViewSize, L, PermaList, IsVerbose, Turn, TurnDuration, TotalTurns, IsDone);

        {stop} ->
            fwrite("BYZ ~p: Bye \n", [self()]),
            exit(normal)

    end. 
