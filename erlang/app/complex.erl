-module(complex).
-export([
    pow/2,
    mult/2,
    sum/2,
    diff/2,
    pol2cart/1,
    cart2pol/1,
    nth_root_of_unity/1
]).

nth_root_of_unity(N) ->
    pol2cart({1,-2*math:pi() / N}).

pow({X,Y},IntExp) ->
    {R,Theta} = cart2pol({X,Y}),
    pol2cart({R,Theta*IntExp}).

pol2cart({R,Theta}) ->
    {R*math:cos(Theta),R*math:sin(Theta)}.


cart2pol({X,Y}) when X == 0 andalso Y == 0->
    {0.0,0.0};
cart2pol({X,Y}) when X == 0 andalso Y < 0->
    {-Y,-math:pi()/2};
cart2pol({X,Y}) when X == 0 andalso Y > 0 ->
    {Y,math:pi()/2};
cart2pol({X,Y}) when X < 0 andalso Y < 0 ->
    {math:sqrt(X*X+Y*Y),math:atan(Y/X)-math:pi()};
cart2pol({X,Y}) when X < 0 andalso Y >= 0 ->
    {math:sqrt(X*X+Y*Y),math:atan(Y/X)+math:pi()};
cart2pol({X,Y}) ->
    {math:sqrt(X*X+Y*Y),math:atan(Y/X)}.


mult({X1,Y1}, {X2,Y2}) ->
    {X1*X2-Y1*Y2,X1*Y2+X2*Y1}.

sum({X1,Y1},{X2,Y2}) ->
    {X1+X2,Y1+Y2}.
diff({X1,Y1},{X2,Y2}) ->
    {X1-X2,Y1-Y2}.