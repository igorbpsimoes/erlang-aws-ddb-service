-module(gpb_tcp_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

-include("kv_pb.hrl").

-define(SOCKET_TIMEOUT, 5000).
-define(MAX_PACKET_SIZE, 6143).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    %% If you want manual recv, set the socket in passive mode.
    ok = Transport:setopts(Socket, [{active, false}, {buffer, 8192}, {recbuf, 8192}]),
    loop(Socket, Transport, <<>>).

loop(Socket, Transport, Buffer) ->
    case Transport:recv(Socket, 0, ?SOCKET_TIMEOUT) of
        {ok, Bin} ->
            NewBuffer = <<Buffer/binary, Bin/binary>>,
            case decode_and_process(NewBuffer, Socket, Transport) of
                {ok, EncBin} ->
                    Transport:send(Socket, EncBin),
                    loop(Socket, Transport, <<>>);
                {error, Reason, Error} ->
                    io:format("Error processing data: ~p~n", [Reason]),
                    Transport:send(Socket, Error),
                    loop(Socket, Transport, <<>>)
            end;

        {error, closed} ->
            io:format("Client disconnected~n", []),
            ok;
        {error, Reason} ->
            io:format("Recv error: ~p~n", [Reason]),
            Transport:close(Socket)
    end.

decode_and_process(Buffer, Socket, Transport) ->
    case gpb:decode_packet(uint32, Buffer, [{packet_size, ?MAX_PACKET_SIZE}]) of
        {ok, PacketBin, _Rest} ->
            decode_msg(PacketBin);
        {more, _TotalSizeNeeded} ->
            loop(Socket, Transport, Buffer);
        {error, Reason} ->
            {error, Reason, <<"PacketFrameError">>}
    end.

decode_msg(MsgBin) ->
    case kv_pb:decode_msg(MsgBin, req_envelope, []) of
        Envelope when is_record(Envelope, req_envelope) ->
            ResponseEnvelope = process_envelope(Envelope),
            EncBin = kv_pb:encode_msg(ResponseEnvelope, req_envelope, []),
            {ok, EncBin};

        {error, DecodeReason} ->
            {error, DecodeReason, <<"ProtobufDecodeError">>}
    end.

process_envelope(#req_envelope{type = set_request_t, set_req = SetReq}) ->
    #set_request{req = #data{key=Key, value=Val}} = SetReq,

    case ddb_client:put_item(Key, Val) of
        ok ->
            #req_envelope{
              type = set_response_t,
              set_resp = #set_response{error = ok}
            };
        {error, _} ->
            #req_envelope{
              type = set_response_t,
              set_resp = #set_response{error = internal}
            }
    end;

process_envelope(#req_envelope{type = get_request_t, get_req = GetReq}) ->
    #get_request{key=Key} = GetReq,
    case ddb_client:get_item(Key) of
        {ok, Val} ->
            #req_envelope{
              type = get_response_t,
              get_resp = #get_response{
                  error = ok,
                  req = #data{key=Key, value=Val}
              }
            };
        {error, not_found} ->
            #req_envelope{
              type = get_response_t,
              get_resp = #get_response{error = not_found}
            };
        {error, _} ->
            #req_envelope{
              type = set_response_t,
              get_resp = #get_response{error = internal}
            }
    end.
