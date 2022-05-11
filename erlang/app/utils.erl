-module(utils).
-import(io,[format/2,format/1]).
-import(lists,[map/2,reverse/1,nth/2,droplast/1,last/1]).
-import(math,[pow/2,log2/1]).
-import(file,[open/2,read_line/1]).
-export([
    read_array/1,
    rearrange/1,
    binary_to_decimal/1,
    decimal_to_binary/1,
    pretty_print/1,
    split_to_lists/2
]).

split_to_lists(N,Lists) ->
    split(Lists,[],round(length(Lists)/N)).

split([],Dst,_) ->
    lists:reverse(Dst);
split(Src,Dst,Length) ->
    {Part,RestSrc} = lists:split(Length,Src),
    split(RestSrc,[Part|Dst],Length).

pretty_print_map({Re,Im}) ->
    format("(~.2f,~.2f)~n",[Re,Im]);
pretty_print_map(ok) ->
    format("").
pretty_print(Y) ->
    format("~n"),
    lists:map(fun(X) -> pretty_print_map(X) end,Y).

binary_to_decimal(Binary) ->
    binary_to_decimal(reverse(Binary),0,0).
binary_to_decimal([],Decimal,_) ->
    Decimal;
binary_to_decimal(Binary,Decimal,I) ->
    [H|T] = Binary,
    if
        H > 47 ->
            binary_to_decimal
    (T,round((H-48)*pow(2,I))+Decimal,I+1);
        true ->
            binary_to_decimal
    (T,round(H*pow(2,I))+Decimal,I+1)
    end.

decimal_to_binary(Decimal) ->
    decimal_to_binary(Decimal,[]).
decimal_to_binary(Decimal,Binary) when Decimal == 0 ->
    Binary;
decimal_to_binary(Decimal,Binary) ->
    decimal_to_binary(Decimal div 2,[(Decimal rem 2)|Binary]).

bit_reverse(Binary,Max) when length(Binary) < Max ->
    bit_reverse([0|Binary],Max);
bit_reverse(BitReversed,_) ->
    reverse(BitReversed).

rearrange(Src) ->
    rearrange(Src,[],[],0).
rearrange(Src,DstEven,DstOdd,Index) 
  when Index < length(Src)/2 ->
    BinaryForm = decimal_to_binary(Index),
    ReversedIndex = binary_to_decimal(
        bit_reverse(BinaryForm,log2(length(Src)))
    ),
    Element = nth(ReversedIndex+1,Src),
    rearrange(Src,[Element|DstEven],DstOdd,Index+1);
rearrange(Src,DstEven,DstOdd,Index) 
  when Index < length(Src) ->
    BinaryForm = decimal_to_binary(Index),
    ReversedIndex = binary_to_decimal(
        bit_reverse(BinaryForm,log2(length(Src)))
    ),
    Element = nth(ReversedIndex+1,Src),
    rearrange(Src,DstEven,[Element|DstOdd],Index+1);
rearrange(_,DstEven,DstOdd,_) ->
    reverse(DstEven)++reverse(DstOdd).


%%% utility functions
% rearrange_indices(X) ->

%     lists:reverse(decimal_to_binary(X)).
% rearrange_indices(X,Xeven,Xodd) when length(X) == 1 ->
%     [E|_] = X,
%     Xodd++[E|Xeven];
% rearrange_indices(X,Xeven,Xodd) when length(X) == 2 ->
%     [E,O|_] = X,
%     [O|Xodd]++[E|Xeven];
% rearrange_indices(X,Xeven,Xodd) -> 
%     [E,O|RestOfX] = X,
%     rearrange_indices(RestOfX,[E|Xeven],[O|Xodd]).


read_array(FileName) ->
    {ok, File} = open(FileName,[read]),
    {ok, Line} = read_line(File),
    Length = parse_number(Line),
    Vector = reverse(read_lines(File,[],Length)),
    % считывать в цикле ровно столько раз
    Vector.

read_lines(_,ListSoFar,N) when N == 0 ->
    ListSoFar;
read_lines(File,ListSoFar,N) when N == 1 ->
    {ok, Line} = read_line(File),
    Number = parse_number(Line),
    read_lines(File,[Number|ListSoFar],0);
read_lines(File,ListSoFar,N) ->
    {ok, Line} = read_line(File),
    Number = parse_number(droplast(Line)),
    read_lines(File,[Number|ListSoFar],N-1).


%% integers only for now
%% optionally can be negative and followed by newline
parse_number(Line) ->    % 10 is a newline code
    IsFollowedByNewLine = last(Line) == 10,
    if IsFollowedByNewLine ->
            Trimmed = droplast(Line);
        true ->
            Trimmed = Line
    end,
    list_to_integer(Trimmed).