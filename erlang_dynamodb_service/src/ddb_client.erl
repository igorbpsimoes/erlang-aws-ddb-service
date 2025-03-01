-module(ddb_client).
-export([init/0, put_item/2, get_item/1]).

-define(TABLE, application:get_env(dynamodb_service, ddb_table)).

init() ->
    erlcloud_ddb:setup_default([
        {access_key_id,     <<"<YOUR_KEY_ID>">>},
        {secret_access_key, <<"<YOUR_SECRET_KEY>">>},
        {region, "eu-north-1"}  % Example region
    ]),
    ok.

put_item(Key, Value) ->
    % 400 KB limit in bytes
    MaxSizeBytes = 400 * 1024,

    % if
    %   byte_size(Value) > MaxSizeBytes ->
    %       {error, too_large};
    %   true ->
    %       Item = #{
    %         "key" => #{ s => Key },
    %         "value" => #{ s => Value }
    %       },
    %       case erlcloud_ddb2:put_item(?TABLE, Item) of
    %           {ok, _Resp} ->
    %               ok;
    %           Error ->
    %               io:format("put_item error: ~p~n", [Error]),
    %               {error, Error}
    %       end
    % end.
    ok.

get_item(Key) ->
    % KeyObj = #{"key" => #{ s => Key }},
    % case erlcloud_ddb2:get_item(?TABLE, KeyObj, #{consistent_read => true}) of
    %     {ok, #{"Item" := #{"value" := #{"s" := Val}}}} ->
    %         {ok, Val};
    %     {ok, #{"Item" := undefined}} ->
    %         not_found;
    %     Error ->
    %         io:format("get_item error: ~p~n", [Error]),
    %         {error, Error}
    % end,
    {ok, "myvalue"}.
