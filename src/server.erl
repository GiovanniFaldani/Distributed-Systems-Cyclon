-module(server).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2]).
-import(rand,[uniform/0]).
-import(my_peer, [peer_execution/6]).

-export([run/0]).

run() ->
    PeerList = [{1, 2}, {2, 1}, {3, 2}, {4, 3}, {5, 2}],
    peer_execution(1, 1, PeerList, 3, 3, 4).

