-module(terst).
-compile(export_all).

-include("terse.hrl").

countdown(0) ->
   ok;
countdown(N) ->
   io:format("~w~n", [N]),
   _(N - 1).


foo(N) ->
   {ok, N + 1}.

bar(N) ->
   countdown({ok, ?_} = foo(N)).

find_by_traversing_lookups(N, L, Ct) ->
   case {value, {N, ?_}} = lists:keysearch(N, 1, L) of
      1 -> Ct;
      Next -> _(Next, L, Ct + 1)
   end.
      
