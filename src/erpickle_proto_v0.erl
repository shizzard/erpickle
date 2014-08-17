-module(erpickle_proto_v0).

-export([encode/1, decode/1]).


%% ---------------------------------------------------------
%% API functions

encode(Data) ->
    E = enc(Data, 0),
    <<E/binary, ".">>.

decode(Data) ->
    case binary:last(Data) of
        46    -> {Decoded, _} = dec({Data, []}), Decoded;
        _Else -> {error, badpickle}
    end.


%% ---------------------------------------------------------
%% internal functions

%% Encoding
% integers
enc(Data, _Depth) when is_integer(Data) ->
    I = integer_to_binary(Data),
    <<"I", I/binary, 10>>;
% floats
enc(Data, _Depth) when is_float(Data) ->
    F = float_to_binary(Data, [{decimals, 12}, compact]),
    <<"F", F/binary, 10>>;
% booleans
enc(true, _Depth) ->
    <<"I01", 10>>;
enc(false, _Depth) ->
    <<"I00", 10>>;
% atoms are serialized as strings
enc(Data, Depth) when is_atom(Data) ->
    enc(atom_to_binary(Data, utf8), Depth);
% strings are supported only as binaries!
enc(Data, Depth) when is_binary(Data) ->
    EscapedData = re:replace(Data, <<"'">>, <<"\\\\'">>, [{return, binary}]),
    BinDepth = integer_to_binary(Depth),
    <<"S'", EscapedData/binary, "'", 10, "p", BinDepth/binary, 10>>;
% tuples
enc(Data, Depth) when is_tuple(Data) ->
    BinDepth = integer_to_binary(Depth),
    List = tuple_to_list(Data),
    T = binary:list_to_bin([ enc(L, Depth + 1) || L <- List ]),
    <<"(", T/binary, "tp", BinDepth/binary, 10>>;
% lists
enc([], Depth) ->
    BinDepth = integer_to_binary(Depth),
    <<"(lp", BinDepth/binary, 10>>;
enc(Data, Depth) when is_list(Data) ->
    BinDepth = integer_to_binary(Depth),
    <<"a", L/binary>> = binary:list_to_bin([ [<<"a">>, enc(D, Depth + 1)] || D <- Data ]),
    <<"(lp", BinDepth/binary, 10, L/binary, "a">>;
% catch-all for all other data types
enc(_Data, _Depth) ->
    {error, unsupported}.

%% Decoding
% integers
dec({<<"I", I/binary>>, _Tail}) ->
    [TrimmedI, Tail] = binary:split(I, <<10>>),
    {binary_to_integer(TrimmedI), Tail};
% floats
dec({<<"F", F/binary>>, _Tail}) ->
    [TrimmedF, Tail] = binary:split(F, <<10>>),
    {binary_to_float(TrimmedF), Tail};
% strings
dec({<<"S", S/binary>>, _Tail}) ->
    {match, [Group, String]} = re:run(S, <<"[\"'](.*(?!\\\\').)[\"']">>,
                                [{capture, all, binary}]),
    [_, Tail] = binary:split(S, Group),
    {re:replace(String, <<"\\\\'">>, <<"'">>, [{return, binary}]), Tail};
% lists
dec({<<"(lp", L/binary>>, _Tail}) ->
    io:format("L = ~p~n", [L]),
    [_Depth, Tail] = binary:split(L, <<10>>),
    {RL, NTail} = dec_list(Tail, [], []),
    io:format("RL = ~p~nNTail = ~p~n~n", [RL, NTail]),
    {lists:reverse(RL), NTail};
% tuples
dec({<<"(", T/binary>>, _Tail}) ->
    dec_tuple(T, []);
dec(D) ->
    io:format("D = ~p~n", [D]),
    {error, unsupported}.

%% list deserializing helper
%% FIXME: deserializing of nested lists doesn't work
dec_list(<<46>>, _Last, Acc) ->
    {Acc, <<>>};
dec_list(<<"a", Tail/binary>>, [], []) ->
    {[], Tail};
dec_list(<<"a", Tail/binary>>, clean, Acc) ->
    {Acc, Tail};
dec_list(<<"a", Tail/binary>>, Last, Acc) ->
    dec_list(Tail, clean, [Last | Acc]);
dec_list(Data, _Last, Acc) ->
    {Decoded, Tail} = dec({Data, []}),
    dec_list(Tail, Decoded, Acc).

%% tuple deserializing helper
dec_tuple(<<"tp", Tail/binary>>, Acc) ->
    [_Depth, NTail] = binary:split(Tail, <<10>>),
    {list_to_tuple(lists:reverse(Acc)), NTail};
dec_tuple(Data, Acc) ->
    {Decoded, Tail} = dec({Data, []}),
    dec_tuple(Tail, [Decoded | Acc]).
