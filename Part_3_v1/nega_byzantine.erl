-module(nega_byzantine).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/2, delete/2, search/2, max/1, member/2]).
-import(rand,[uniform/0]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, list_max_tuple/1, index_elem/2, remove_pid_list/2, filter_list/2, filter_tuple_list/2, remove_duplicates/1]).

-export([check_old_lists/2]).


check_old_lists(RepList, OldQLists) ->
    Results = [ filter_list(RepList, List) =:= [] || List <- OldQLists],
    WhereDuplicate = index_true(Results),
    update_oldList(RepList, OldQLists, WhereDuplicate).


%appends element if whereduplicate != 0. Otherwise removes head and appends viewReceived
update_oldList(RepList, [], _) -> {true, [RepList]};
update_oldList(RepList, OldQLists, WhereDuplicate) when WhereDuplicate =:= 0 ->
    CleanOldQList = behead_if_long(OldQLists, 5),
    
    CurrentQLists = append(CleanOldQList, [RepList]),
    {true, CurrentQLists};
update_oldList(_, OldQLists, WhereDuplicate) ->
    ToAppend = nth(WhereDuplicate, OldQLists),
    CleanOldQList = delete(ToAppend, OldQLists),
    CurrentQLists = append(CleanOldQList, [ToAppend]),
    {false, CurrentQLists}.


% gets the index of the true element
index_true(List) -> index_true(List, 1).
index_true([], _) -> 0;
index_true([true | _], I) -> I;
index_true([_ | T], I) -> index_true(T, I+1).

behead_if_long(List, Len) when length(List) < Len -> List;
behead_if_long(List, _) ->
    Head = nth(1, List),
    delete(Head , List).