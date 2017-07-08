-module (math_functions).
-export ([even/1,odd/1,filter/2, collect1/1, collect2/1]).

even(X) ->
  X rem 2 =:= 0.

odd(X) ->
  X rem 2 =:= 1.

filter(Func, L) ->
  [X || X <- L, Func(X) ].

collect1(L) ->
  {
    filter(fun(X) -> even(X) end, L),
    filter(fun(X) -> odd(X) end, L)
  }.

collect2(L) ->
  odd_and_even_acc(L, [], []).

odd_and_even_acc([H|T], Evens, Odds) ->
  case even(H) of
    true -> odd_and_even_acc(T, [H|Evens], Odds);
    false -> odd_and_even_acc(T, Evens, [H|Odds])
  end;

odd_and_even_acc([], Evens, Odds) ->
  {Evens, Odds}.
