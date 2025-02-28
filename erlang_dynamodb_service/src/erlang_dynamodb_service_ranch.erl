-module(erlang_dynamodb_service_ranch).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

-include("kv_pb.hrl").

-define(SOCKET_TIMEOUT, 5000).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    %% If you want manual recv, set the socket in passive mode.
    ok = Transport:setopts(Socket, [{active, false}]),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    %% Synchronously receive data with a 5s timeout.
    case Transport:recv(Socket, 0, ?SOCKET_TIMEOUT) of
        {ok, Bin} ->
            handle_request(Bin, Socket, Transport),
            loop(Socket, Transport);
        {error, closed} ->
            io:format("Client disconnected~n", []),
            ok;
        {error, Reason} ->
            io:format("Recv error: ~p~n", [Reason]),
            Transport:close(Socket)
    end.

handle_request(Bin, Socket, Transport) ->
    Decoded = kv_pb:decode_msg(Bin, req_envelope),
    case Decoded of
        Envelope when is_record(Envelope, req_envelope) ->
            ResponseEnvelope = process_envelope(Envelope),
            EncBin = kv_pb:encode_msg(ResponseEnvelope, req_envelope, []),
            Transport:send(Socket, EncBin);
        {error, Reason} ->
            io:format("Protobuf decode error with reason: ~p~n", [Reason]),
            Transport:send(Socket, <<"DecodeError">>);
        Error ->
            io:format("Protobuf decode error: ~p~n", [Error]),
            Transport:send(Socket, <<"DecodeError">>)
    end.

process_envelope(#req_envelope{type = set_request_t, set_req = SetReq}) ->
    #set_request{req = #data{key=Key, value=Val}} = SetReq,
    ok = erlang_dynamodb_service_ddb:put_item(Key, Val),
    #req_envelope{
       type = set_response_t,
       set_resp = #set_response{error = ok}
    };

process_envelope(#req_envelope{type = get_request_t, get_req = GetReq}) ->
    #get_request{key=Key} = GetReq,
    case erlang_dynamodb_service_ddb:get_item(Key) of
        {ok, Val} ->
            #req_envelope{
              type = get_response_t,
              get_resp = #get_response{
                  error = ok,
                  req = #data{key=Key, value=Val}
              }
            };
        not_found ->
            #req_envelope{
              type = get_response_t,
              get_resp = #get_response{error = not_found}
            };
        {error, too_large} ->
            #req_envelope{
              type = set_response_t,
              get_resp = #get_response{error = internal}
            };
        {error, _} ->
            #req_envelope{
              type = set_response_t,
              get_resp = #get_response{error = internal}
            }
    end.
