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
-export([sequential/1,parallel/2,join_tasks/4,task1/8]).

sequential(X) ->
    Wn = nth_root_of_unity(length(X)),
    Wn2 = nth_root_of_unity(length(X)/2),
    Y = sequential(0,length(X)/2,Wn,Wn2,X,[],[]),
    Y.
parallel(X,Threads) ->
    Wn = nth_root_of_unity(length(X)),
    Wn2 = nth_root_of_unity(length(X)/2),
    Y = parallel_inner(X,Threads,length(X)/2,0,Wn,Wn2),
    Y.

parallel_inner(_,_,Stop,K,_,_) when K == Stop ->
    receive
        Y ->
            Y
        end;
parallel_inner(X,Threads,_,_,Wn,Wn2) ->
    JoinerPID = spawn(
        fft,
        join_tasks,
        [self(),[],[],Threads]
    ),
    PartsOfX = split_to_lists(Threads,X),
    Y = spawn_tasks(JoinerPID,PartsOfX,0,Threads,Wn,Wn2),
    Y.

join_tasks(SpawnerPID,Y1,Y2,0) ->
    SpawnerPID ! reverse(Y1)++reverse(Y2);
join_tasks(SpawnerPID,Y1,Y2,N) ->
    receive
        {NewY1,NewY2} ->
            join_tasks(SpawnerPID,NewY1++Y1,NewY2++Y2,N-1)
        end.
   

spawn_tasks(_,[],_,_,_,_) ->
    receive
        ResultPart ->
            ResultPart
        end;
spawn_tasks(JoinerPID,PartsOfX,TaskIndex,Threads,Wn,Wn2) ->
    [Part|Rest] = PartsOfX,
    spawn(fft,task1,[
        JoinerPID,
        Part,
        TaskIndex*length(Part)/2,
        (TaskIndex+1)*length(Part)/2,
        Wn,
        Wn2,
        [],
        []
    ]),
    spawn_tasks(JoinerPID,Rest,TaskIndex+1,Threads,Wn,Wn2).

task1(JoinerPID,X,K,Stop,Wn,Wn2,Y1,Y2) when K < Stop ->
    {EvenSum,OddSum} = accumulate_sums(X,K,0,length(X)/2,Wn2,{0,0},{0,0}),
    Wkn = pow(Wn,K),
    task1(JoinerPID,X,K+1,Stop,Wn,Wn2,
        [sum(EvenSum,mult(OddSum,Wkn))|Y1],
        [diff(EvenSum,mult(OddSum,Wkn))|Y2]);
task1(JoinerPID,_,_,_,_,_,Y1,Y2) ->
    JoinerPID ! {Y1,Y2}.

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
