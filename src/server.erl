-module(server).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/2]).
-import(rand,[uniform/1, uniform/0]).
-import(my_peer, [peer_execution/9, peer_init/2]).
-import(utils, [shuffle_list/1]).

-export([run/1]).

run(N) ->
    NPeers = 10,
    ViewSize = 5,
    L = 3,
    IsVerbose = true,
    MaxAge = 10,

    Children = create_n_peers(NPeers, [], IsVerbose),
    ListOfPidLists[create_start_list(NPeers, Children, MaxAge, ViewSize, CurrentPeer) || CurrentPeer <- Children],
    [Node ! {init, 0} || Node <- Children].


create_n_peers(0, Children, _) -> Children;
create_n_peers(N, Children, IsVerbose) ->
    Child = spawn(peer_init(self(), IsVerbose)),
    create_n_peers(N - 1, append(Children, [Child]), IsVerbose).


create_peer(Max, MaxAge, ViewSize, L) ->
    PeerList = create_start_list(Children, MaxAge, ViewSize),
    fwrite("List: ~p\n", [PeerList]),
    spawn(my_peer, peer_execution, [self(), true, PeerList, ViewSize, L, true, 4, 0, true]).


create_start_list(N, PidList, MaxAge, ViewSize, CurrentPeer) ->
    List = [ {Pid, uniform(MaxAge)} || Pid <- PidList],
    
    sublist(shuffle_list(List), ViewSize).

    