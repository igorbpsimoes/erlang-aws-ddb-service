-module(ddb_client).
-export([put_item/2, get_item/1]).

-define(TABLE, element(2, application:get_env(dynamodb_service, ddb_table))).
-define(ERLCLOUD_PROFILE, element(2, application:get_env(dynamodb_service, erlcloud_profile))).
-define(MAX_SIZE_BYTES, 400 * 1024). % 400 KB limit in bytes

put_item(Key, Value) ->
    if
      byte_size(Value) > ?MAX_SIZE_BYTES ->
          {error, too_large};
    true ->
        DdbItem = to_erlcloud_ddb_set_item(Key, Value),

        case ercloud_ddb_put_item(DdbItem) of
          {ok, _Resp} ->
            ok;
          Error ->
            io:format("put_item error: ~p~n", [Error]),
            {error, Error}
        end
    end.

get_item(Key) ->
    DdbItem = to_erlcloud_ddb_get_item(Key),

    case ercloud_ddb_get_item(DdbItem) of
        {ok, []} ->
            {error, not_found};
        {ok, Item} ->
            {ok, from_erlcloud_item(Item)};
        Error ->
            io:format("get_item error: ~p~n", [Error]),
            {error, Error}
    end.

to_erlcloud_ddb_set_item(Key, Value) ->
    lists:append(
        to_erlcloud_ddb_get_item(Key),
        [{<<"DataValue">>, {s, Value}}]
    ).

to_erlcloud_ddb_get_item(Key) ->
    [{<<"Key">>, {s, Key}}].

ercloud_ddb_put_item(DdbSetItem) ->
    erlcloud_ddb2:put_item(
        binary_table_name(),
        DdbSetItem,
        [],
        ddb_config()
    ).

ercloud_ddb_get_item(DdbGetItem) ->
    DdbOpts = [
        {projection_expression, <<"DataValue">>},
        consistent_read,
        {return_consumed_capacity, total}
    ],

    erlcloud_ddb2:get_item(
        binary_table_name(),
        DdbGetItem,
        DdbOpts,
        ddb_config()
    ).

from_erlcloud_item(Item) ->
    case lists:keyfind(<<"DataValue">>, 1, Item) of
        {_, Val} ->
            Val;
        false ->
            io:format("from_erlcloud_item error: ~p~n", ["Could not find DataValue"]),
            undefined
    end.

ddb_config() ->
    {ok, Conf} = erlcloud_aws:profile(atom_profile_name()),
    Conf.

atom_profile_name() ->
    list_to_atom(?ERLCLOUD_PROFILE).

binary_table_name() ->
    list_to_binary(?TABLE).
