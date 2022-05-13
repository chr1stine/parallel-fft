-module(fft).
-import(complex,[
    pow/2,
    sum/2,
    diff/2,
    mult/2,
    nth_root_of_unity/1
]).
-import(utils,[rearrange/1,split_to_lists/2]).
-import(lists,[reverse/1]).
-export([sequential/1,parallel/2,tasks/7,task/6]).

sequential(X) ->
    Wn = nth_root_of_unity(length(X)),
    Wn2 = nth_root_of_unity(length(X)/2),
    Y = sequential(0,length(X)/2,Wn,Wn2,X,[],[]),
    Y.
parallel(X,T) ->
    Wn = nth_root_of_unity(length(X)),
    Wn2 = nth_root_of_unity(length(X)/2),
    PartsOfX = split_to_lists(T,X),
    Y = parallel_loop(PartsOfX,0,length(X)/2,T,[],[],Wn,Wn2),
    Y.

parallel_loop(_,K,Stop,_,EvenY,OddY,_,_) when K == Stop->
    reverse(EvenY)++reverse(OddY);
parallel_loop(PartsOfX,K,Stop,T,EvenY,OddY,Wn,Wn2) ->
    
    Wkn = pow(Wn,K),
    EvenSum = {0,0},
    OddSum = {0,0},
    {NewEvenSum,NewOddSum} = tasks(PartsOfX,K,0,T,Wn2,EvenSum,OddSum),

    NewEvenY = sum(NewEvenSum,mult(NewOddSum,Wkn)),
    NewOddY = diff(NewEvenSum,mult(NewOddSum,Wkn)),

    parallel_loop(PartsOfX,K+1,Stop,T,[NewEvenY|EvenY],[NewOddY|OddY],Wn,Wn2).

tasks([],_,Ti,T,_,TotalEvenSum,TotalOddSum) when Ti == 2*T->
    {TotalEvenSum, TotalOddSum};
tasks([],K,Ti,T,Wn2,TotalEvenSum,TotalOddSum)-> 
    receive
        {EvenSum,OddSum} ->
            tasks([],K,Ti+1,T,Wn2,sum(TotalEvenSum,EvenSum),sum(TotalOddSum,OddSum))
    end;
tasks(PartsOfX,K,Ti,T,Wn2,TotalEvenSum,TotalOddSum) ->
    [Part|RestParts] = PartsOfX,
    Nfrom = Ti*length(Part)/2,
    Nto = (Ti+1)*length(Part)/2,
    spawn(fft,task,[
        self(),
        Part,
        K,
        Nfrom,
        Nto,
        Wn2
    ]),
    tasks(RestParts,K,Ti+1,T,Wn2,TotalEvenSum,TotalOddSum).

task(JoinerPID,Part,K,Nfrom,Nto,Wn2)->
    {EvenSum,OddSum} = accumulate_sums(
        Part,K,Nfrom,Nto,Wn2,{0,0},{0,0}
    ),
    JoinerPID ! {EvenSum,OddSum}.



sequential(K,Stop,Wn,Wn2,X,Y1,Y2) when K < Stop ->
    Wkn = pow(Wn,K),
    {EvenSum,OddSum} = accumulate_sums(X,K,0,Stop,Wn2,{0,0},{0,0}),
    NewEven = sum(EvenSum,mult(OddSum,Wkn)),
    NewY1 = [NewEven|Y1],
    NewOdd = diff(EvenSum,mult(OddSum,Wkn)),
    NewY2 = [NewOdd|Y2],
    sequential(K+1,Stop,Wn,Wn2,X,NewY1,NewY2);
sequential(_,_,_,_,_,Y1,Y2) ->
    reverse(Y1)++reverse(Y2).

accumulate_sums(X,K,N,Stop,Wn2,EvenSum,OddSum) when N < Stop ->
    Wkn2 = pow(Wn2,K*N),
    [Even,Odd|TailX] = X,
    NewEvenSum = sum(EvenSum,mult({Even,0},Wkn2)),
    NewOddSum = sum(OddSum,mult({Odd,0},Wkn2)),
    accumulate_sums(TailX,K,N+1,Stop,Wn2,NewEvenSum,NewOddSum);

accumulate_sums(_,_,_,_,_,EvenSum,OddSum) ->
    {EvenSum,OddSum}.
