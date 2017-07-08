-module (geometry).
-export ([area/1, test/0]).

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius}) -> 3.14159 * Radius * Radius;
area({circum, rectangle, Width, Height}) ->
  2*(Width+Height);
area({square, Side}) -> Side * Side.

test() ->
  50 = area({rectangle, 10, 5}),
  144 = area({square, 12}),
  30 = area({circum, rectangle, 10, 5}),
  test_worked.