%%%-------------------------------------------------------------------
%% @doc erlang_dynamodb_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_dynamodb_service_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(PORT, 8080).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    % "one_for_one" strategy means if the listener dies,
    % only that child restarts (not all children).
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Single child: the Ranch listener.
    RanchRef = my_listener, % a unique atom or reference for child ID
    TransportOpts = #{
        socket_opts => [
            {port, ?PORT},
            {nodelay, true},
            {backlog, 1024}
        ],
        num_acceptors => 10,
        max_connections => 1024
    },

    RanchChild = #{
        id => RanchRef,
        start => {ranch, start_listener,
                [RanchRef, ranch_tcp, TransportOpts,
                erlang_dynamodb_service_ranch, []]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },

    ChildSpecs = [RanchChild],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
