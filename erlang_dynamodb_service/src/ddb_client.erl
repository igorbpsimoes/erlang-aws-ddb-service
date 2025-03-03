-module(ddb_client).
-export([init/0, put_item/2, get_item/1]).

-define(TABLE, element(2, application:get_env(dynamodb_service, ddb_table))).
-define(MAX_SIZE_BYTES, 400 * 1024). % 400 KB limit in bytes

init() ->
    ok.

put_item(Key, Value) ->
    if
      byte_size(Value) > ?MAX_SIZE_BYTES ->
          {error, too_large};
    true ->
        case ercloud_put_item(Key, Value) of
          {ok, _Resp} ->
            ok;
          Error ->
            io:format("put_item error: ~p~n", [Error]),
            {error, Error}
        end
    end.

get_item(Key) ->
    case ercloud_get_item(Key) of
        {ok, #{"Item" := #{"value" := #{"s" := Val}}}} ->
            {ok, Val};
        {ok, #{"Item" := undefined}} ->
            not_found;
        Error ->
            io:format("get_item error: ~p~n", [Error]),
            {error, Error}
    end.

ercloud_put_item(Key, Value) ->
    Item = [{<<"Key">>, Key}, {<<"DataValue">>, Value}],
    erlcloud_ddb2:put_item(binary_table_name(), Item).

ercloud_get_item(Key) ->
    ItemKey = [{<<"Key">>, {s, Key}}],
    DdbOpts = [
        {projection_expression, <<"DataValue">>},
        consistent_read,
        {return_consumed_capacity, total}
    ],

    erlcloud_ddb2:get_item(
        binary_table_name(),
        ItemKey,
        DdbOpts
    ).

binary_table_name() ->
    list_to_binary(?TABLE).
