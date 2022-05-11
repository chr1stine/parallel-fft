-module(main).
-import(utils,[
    pretty_print/1,
    read_array/1]).
-import(fft,[
    sequential/1,
    parallel/2
]).
-export([run/2]).

run(InputFileName,T) ->
    X = read_array(InputFileName),
    io:format("~nfft sequential:~n"),
    Y_seq = sequential(X),
    pretty_print(Y_seq),
    io:format("~nfft parallel:~n"),
    if length(X) == T ->
        io:format("not implemented for array of length ~w",[T]),
        io:format("~n");
    true ->
        Y_par = parallel(X,T),
        pretty_print(Y_par),
        io:format("~n")
    end.