-module(terse).

-compile(export_all).

-record(self_recurse, {infunction = '_'}).
-record(patterned_return, {inblock = false}).

-include("terse.hrl").

parse_transform(Forms) ->
   parse_transform(Forms, []).

parse_transform(Forms, Options) ->
   io:format("Options ===~n~p~n~n", [Options]),
   io:format("Forms     =============================~n~p~n~n", [Forms]),
   io:format("End Forms =============================~n", []),
   Fs = pmr_forms(tr_forms(Forms)),
   io:format("AFTER   ===~n~p~n", [Fs]),
   Fs.

tr_forms(Forms) ->
   parsett:transform(fun self_recurse/2, Forms, #self_recurse{}).

self_recurse(Form = {function, _, Fun, _, _}, State) ->
   {descend, Form, State#self_recurse{infunction = Fun}};
self_recurse({call, L, {var, VL, '_'}, Subforms},
             State = #self_recurse{infunction = Fun}) ->
   {descend, {call, L, {atom, VL, Fun}, Subforms}, State}.

pmr_forms(Forms) ->
   parsett:transform(fun patterned_return/2, Forms, #patterned_return{}).
   
patterned_return({match, L, Left, Right},
                 State = #patterned_return{inblock = false}) ->
   {descend,
    case contains_var(?question_atom, Left) of
       true ->
          {block, L,
           [{match, L, Left, Right},
            {var, L, ?question_atom}]};
       _ -> {match, L, Left, Right}
    end,
    State#patterned_return{inblock = true}};
patterned_return({match, L, Left, Right},
                State = #patterned_return{inblock = true}) ->
   {descend,
    {match, L, Left, Right},
    State#patterned_return{inblock = false}}.


contains_var(Atom, Forms) ->
   F = fun ({var, _, Var}, _) when Var =:= Atom ->
             {done, true};
           (_, true) ->
             {done, true};
           (_, false) ->
             {descend, false}
       end,
   FD = fun (Form, State) ->
              R = F(Form, State),
              io:format("fun ({~w, ~w, ...}, ~w) -> ~w~n", [element(1, Form),
                                                            element(2, Form),
                                                            State, R]),
              R
        end,
   M = fun (C1, C2) when C1 =:= true; C2 =:= true -> true;
           (false, false) -> false
       end,
   MD = fun (S1, S2) ->
              A = M(S1, S2),
              io:format("   merge ~w and ~w -> ~w~n", [S1, S2, A]),
              A
        end,
   parsett:scan(FD, M, Forms, false).
