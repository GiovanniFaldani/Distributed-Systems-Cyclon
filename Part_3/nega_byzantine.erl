-module(nega_byzantine).

-import(io,[fwrite/2, fwrite/1]).
-import(lists,[sum/1,nth/2, sort/1, sublist/2, append/2, delete/2, search/2, max/1, member/2]).
-import(rand,[uniform/0]).
-import(utils, [shuffle_list/1, pick_L_elements/2, list_max_id/1, list_max_tuple/1, index_elem/2, remove_pid_list/2, filter_list/2, filter_tuple_list/2, remove_duplicates/1]).

-export([convalidate_view/2, convalidate_view_byzantine/2]).


%checks if he convalidated lists like that, if equal returns false
convalidate_view(ViewReceived, ViewsConvalidated) -> 
    Results = [ filter_list(ViewReceived, View) =:= [] || View <- ViewsConvalidated],
    WhereDuplicate = index_true(Results),
    update_convalidated(ViewReceived, ViewsConvalidated, WhereDuplicate).

%checks if he convalidated lists like that, if equal returns false
convalidate_view_byzantine([], _) -> false;
convalidate_view_byzantine([{Pid, _} | T], ByzList) -> 
    Result  = search(Pid, ByzList),
    if
        Result =:= false -> convalidate_view_byzantine(T, ByzList);
        true -> true
    end.


% gets the index of the true element
index_true(List) -> index_true(List, 1).
index_true([], _) -> 0;
index_true([true | _], I) -> I;
index_true([_ | T], I) -> index_true(T, I+1).

%appends element if whereduplicate != 0. Otherwise removes head and appends viewReceived
update_convalidated(ViewReceived, [], _) -> {true, [ViewReceived]};
update_convalidated(ViewReceived, ViewsConvalidated, WhereDuplicate) when WhereDuplicate =:= 0 ->
    Head = nth(1, ViewsConvalidated),
    FreedViewsConvalidated = delete(Head , ViewsConvalidated),
    NewViewsConvalidated = append(FreedViewsConvalidated, [ViewReceived]),
    {true, NewViewsConvalidated};
update_convalidated(_, ViewsConvalidated, WhereDuplicate) ->
    ToAppend = nth(WhereDuplicate, ViewsConvalidated),
    FreedViewsConvalidated = delete(ToAppend, ViewsConvalidated),
    NewViewsConvalidated = append(FreedViewsConvalidated, [ToAppend]),
    {false, NewViewsConvalidated}.


    

