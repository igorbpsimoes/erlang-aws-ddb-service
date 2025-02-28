-module(test_client).
-export([set_kv/2, get_kv/1]).

-include("kv_pb.hrl").

-define(HOST, "localhost").
-define(PORT, 8080).
-define(SOCKET_TIMEOUT, 5000).

set_kv(Key, Value) ->
  ReqEnvelope = build_req_envelope(
    set_request_t,
    #set_request{ req = #data{key = Key, value = Value}}
  ),
  do_request(ReqEnvelope).

get_kv(Key) ->
  ReqEnvelope = build_req_envelope(get_request_t, #get_request{key = Key}),
  do_request(ReqEnvelope).

build_req_envelope(Type, Request) ->
  SetReq = case Type of
    set_request_t -> Request;
    _ -> undefined
  end,

  GetReq = case Type of
    get_request_t -> Request;
    _ -> undefined
  end,

  #req_envelope{type = Type, set_req = SetReq, get_req = GetReq}.

do_request(Envelope) ->
    Sock = connect(),
    EncBin = encode_response(Envelope),
    ok = gen_tcp:send(Sock, EncBin),
    receive_response(Sock).

connect() ->
    {ok, Sock} = gen_tcp:connect(?HOST, ?PORT,
                                [binary, {active, false}, {packet, 0}]),
    Sock.

encode_response(Envelope) ->
  EncBin = kv_pb:encode_msg(Envelope, req_envelope),
  EncBin.

receive_response(Sock) ->
  case gen_tcp:recv(Sock, 0, ?SOCKET_TIMEOUT) of
      {ok, RespBin} ->
          gen_tcp:close(Sock),
          decode_response(RespBin);
      {error, closed} ->
          io:format("Server closed connection~n", []),
          {error, closed};
      {error, Reason} ->
          io:format("Recv error: ~p~n", [Reason]),
          {error, Reason}
  end.

decode_response(RespBin) ->
  case  kv_pb:decode_msg(RespBin, req_envelope) of
      Envelope when is_record(Envelope, req_envelope) ->
        Envelope;

      {error, Reason, Stacktrace} ->
          io:format("Protobuf decode error: ~p, Stacktrace: ~p~n", [Reason, Stacktrace]),
          {error, Reason, Stacktrace};
      Error ->
          io:format("Decode error: ~p~n", [Error]),
          Error
  end.
