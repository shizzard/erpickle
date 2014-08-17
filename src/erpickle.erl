-module(erpickle).

-export([encode/1, encode/2, decode/1]).


%% ---------------------------------------------------------
%% API functions

encode(Data) ->
    encode(Data, 0).

encode(Data, 0) ->
    erpickle_proto_v0:encode(Data);
encode(_Data, _BadProtocolVersion) ->
    {error, badprotoversion}.

decode(<<128, _Data/binary>>) ->
    % erpickle_proto_v1:decode(Data);
    {error, badprotoversion};
decode(Data) ->
    erpickle_proto_v0:decode(Data).
