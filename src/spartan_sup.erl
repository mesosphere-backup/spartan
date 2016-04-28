-module(spartan_sup).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("spartan.hrl").

-include_lib("dns/include/dns_terms.hrl").
-include_lib("dns/include/dns_records.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ZkRecordServer = {spartan_zk_record_server,
                      {spartan_zk_record_server, start_link, []},
                       permanent, 5000, worker,
                       [spartan_zk_record_server]},
    ConfigLoaderServer =
        #{
            id => spartan_config_loader_server,
            start => {spartan_config_loader_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [spartan_config_loader_server]
        },

    HandlerSup = #{
        id => spartan_handler_sup,
        start => {spartan_handler_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [spartan_handler_sup]
    },

    WatchdogSup = #{
        id => spartan_watchdog,
        start => {spartan_watchdog, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [spartan_watchdog]
    },
    %% Configure metrics.
    spartan_metrics:setup(),

    %% Setup ready.spartan zone / record
    ok = spartan_zone_setup(),

    %% Systemd Sup intentionally goes last
    Children = [HandlerSup, ZkRecordServer, ConfigLoaderServer, WatchdogSup],
    Children1 = maybe_add_udp_servers(Children),

    %% The top level sup should never die.
    {ok, { {one_for_all, 10000, 1}, Children1} }.

%%====================================================================
%% Internal functions
%%====================================================================


%% @private
maybe_add_udp_servers(Children) ->
    case spartan_config:udp_enabled() of
        true ->
            udp_servers() ++ Children;
        false ->
            Children
    end.

udp_servers() ->
    Addresses = spartan_app:bind_ips(),
    lists:map(fun udp_server/1, Addresses).



udp_server(Address) ->
    #{
        id => {spartan_udp_server, Address},
        start => {spartan_udp_server, start_link, [Address]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [spartan_udp_server]
    }.

spartan_zone_setup() ->
    SOA = #dns_rr{
        name = <<"spartan">>,
        type = ?DNS_TYPE_SOA,
        ttl = 5
    },
    RR = #dns_rr{
        name = <<"ready.spartan">>,
        type = ?DNS_TYPE_A,
        ttl = 5,
        data = #dns_rrdata_a{ip = {127, 0, 0, 1}}
    },
    Records = [SOA, RR],
    Sha = crypto:hash(sha, term_to_binary(Records)),
    case catch erldns_zone_cache:put_zone({<<"spartan">>, Sha, Records}) of
        ok ->
            ok;
        Else ->
            {error, Else}
    end.