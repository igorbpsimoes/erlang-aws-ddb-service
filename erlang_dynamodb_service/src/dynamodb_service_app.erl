%%%-------------------------------------------------------------------
%% @doc dynamodb_service public API
%% @end
%%%-------------------------------------------------------------------

-module(dynamodb_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dynamodb_service_sup:start_link().

stop(_State) ->
    ranch:stop_listener(my_listener),
    ok.

%% internal functions
