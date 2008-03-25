-module(parsett).

-compile(export_all).

scan(F, M, Forms) ->
   scan(F, M, Forms, []).

%% Scan(Fun, MergeFun, Forms, InitState)
%% Fun accepts form and state
%% MergeFun merges two states
scan(_, _, [], State) ->
   State;
scan(F, M, [Node|Forms], State) ->
   M(scan(F, M, Node, State), scan(F, M, Forms, State));
scan(F, M, Node, InState) ->
   {Direct, State} = case catch F(Node, InState) of
                           {'EXIT', {function_clause, _}} ->
                                 {descend, InState};
                           Result -> Result
                        end,
   case Direct of
      done ->
         State;
      prune ->
         State;
      descend ->
         case Node of
            {Form, _, What, SubForms} when Form == 'case';
                                           Form == 'fun';
                                           Form == call ->
               M(State,
                 M(scan(F, M, What, State),
                   scan(F, M, SubForms, State)));
            {clause, _, Args, Guards, Exprs} ->
               M(State,
                 M(scan(F, M, Args, State),
                   M(scan(F, M, Guards, State),
                     scan(F, M, Exprs, State))));
            {Form, _, First, Second}  when Form == cons;
                                           Form == match ->
               M(State,
                 M(scan(F, M, First, State),
                   scan(F, M, Second, State)));
            {function, _, _Fun, _Arity, SubForms} ->
               M(State,
                 scan(F, M, SubForms, State));
            {remote, _, _Mod, _Fun, Args} ->
               M(State,
                 scan(F, M, Args, State));
            {tuple, _, Es} ->
               M(State,
                 scan(F, M, Es, State));
            {block, _, Subforms} ->
               M(State,
                 scan(F, M, Subforms, State));
            _ ->
               State
         end
   end.

%% transform(Fun, Forms)
%% Fun accepts two arguments, Form and state
%% returns specifier, new form and new state
transform(F, Forms) ->
   transform(F, Forms, [], []).

transform(F, Forms, InitState) ->
   transform(F, Forms, [], InitState).

transform(_, [], Out, _State) ->
   lists:reverse(Out);
transform(F, In, [], State) when not is_list(In) ->
   [Thing] = transform(F, [In], [], State),
   Thing;
transform(F, [In|Forms], Out, InState) ->
   {Direct, Node, State} = case catch F(In, InState) of
                              {'EXIT', {function_clause, _}} ->
                                 {descend, In, InState};
                              Result -> Result
                           end,
   case Direct of
      done ->
         Node;
      prune ->
         transform(F, Forms, [Node|Out], InState);
      descend ->
         case Node of
            {Form, L, What, SubForms} when Form == 'case';
                                           Form == 'fun';
                                           Form == call ->
               transform(F,
                         Forms,
                         [{Form, L,
                           transform(F, What, [], State),
                           transform(F, SubForms, [], State)}|Out],
                         InState);
            {clause, L, Args, Guards, Exprs} ->
               transform(F,
                         Forms,
                         [{clause, L,
                           transform(F, Args, [], State),
                           transform(F, Guards, [], State),
                           transform(F, Exprs, [], State)}|Out],
                         InState);
            {Form, L, First, Second}  when Form == cons;
                                           Form == match ->
               transform(F, Forms,
                         [{Form, L,
                           transform(F, First, [], State),
                           transform(F, Second, [], State)}|Out],
                         InState);
            {function, L, Fun, Arity, SubForms} ->
               transform(F, Forms,
                         [{function, L, Fun, Arity,
                           transform(F, SubForms, [], State)}|Out],
                         InState);
            {remote, L, Mod, Fun, Args} ->
               %% Mod and Fun could be descendable expressions?
               transform(F, Forms,
                         [{remote, L, Mod, Fun,
                           transform(F, Args, [], State)}|Out],
                         InState);
            {tuple, L, Es} ->
               transform(F, Forms,
                         [{tuple, L,
                           transform(F, Es, [], State)}|Out],
                         InState);
            {block, L, Subforms} ->
               transform(F, Forms,
                         [{block, L,
                           transform(F, Subforms, [], State)}|Out],
                         InState);
            _ ->
               transform(F, Forms,
                         [Node|Out], InState)
         end
   end.
