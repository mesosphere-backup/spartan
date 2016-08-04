-module(spartan_app).
-author("Christopher Meiklejohn <christopher.meiklejohn@gmail.com>").

-behaviour(application).

-include("spartan.hrl").

-include_lib("dns/include/dns_terms.hrl").
-include_lib("dns/include/dns_records.hrl").
-define(TCP_LISTENER_NAME, spartan_tcp_listener).

-define(COMPILE_OPTIONS,
        [verbose,
         report_errors,
         report_warnings,
         no_error_module_mismatch,
         {source, undefined}]).

%% Application callbacks
-export([start/2, stop/1, wait_for_reqid/2]).

%% API
-export([parse_ipv4_address/1]).

-export([parse_port_upstream_resolvers/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    maybe_load_json_config(), %% Maybe load the relevant DCOS configuration
    Ret = spartan_sup:start_link(),
    maybe_start_tcp_listener(),
    Ret.

%%--------------------------------------------------------------------
stop(_State) ->
    ranch:stop_listener(?TCP_LISTENER_NAME),
    ok.


%% @doc Parse an IP Address
parse_ipv4_address(Value) when is_binary(Value) ->
    parse_ipv4_address(binary_to_list(Value));
parse_ipv4_address(Value) ->
    {ok, IP} = inet:parse_ipv4_address(Value),
    IP.

%%====================================================================
%% Internal functions
%%====================================================================



%% @doc Wait for a response.
wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, ok} ->
            ok;
        {ReqID, ok, Val} ->
            {ok, Val}
    after Timeout ->
        {error, timeout}
    end.

%% @private
maybe_start_tcp_listener() ->
    case spartan_config:tcp_enabled() of
        true ->
            IPs = spartan_config:bind_ips(),
            lists:foreach(fun start_tcp_listener/1, IPs);
        false ->
            ok
    end.

start_tcp_listener(IP) ->
    Port = spartan_config:tcp_port(),
    Acceptors = 100,
    Options = [{port, Port}, {ip, IP}],
    {ok, _} = ranch:start_listener({?TCP_LISTENER_NAME, IP},
        Acceptors,
        ranch_tcp,
        Options,
        spartan_tcp_handler,
        []).
% A normal configuration would look something like:
%{
%   "upstream_resolvers": ["169.254.169.253"],
%   "udp_port": 53,
%   "tcp_port": 53
%}

maybe_load_json_config() ->
    case file:read_file("/opt/mesosphere/etc/spartan.json") of
        {ok, FileBin} ->
            load_json_config(FileBin);
        _ ->
            ok
    end.

load_json_config(FileBin) ->
    ConfigMap = jsx:decode(FileBin, [return_maps]),
    ConfigTuples = maps:to_list(ConfigMap),
    lists:foreach(fun process_config_tuple/1, ConfigTuples).

process_config_tuple({<<"upstream_resolvers">>, UpstreamResolvers}) ->
    UpstreamResolverIPs = lists:map(fun parse_ipv4_address/1, UpstreamResolvers),
    UpstreamResolverPorts = lists:map(fun parse_port_upstream_resolvers/1, UpstreamResolvers),
    ConfigValue = [{UpstreamResolverIP, UpstreamResolverPort} || UpstreamResolverIP <- UpstreamResolverIPs, UpstreamResolverPort <-
        UpstreamResolverPorts],
    io:fwrite("Ip Environment", []),
    application:set_env(?APP, upstream_resolvers, ConfigValue);
process_config_tuple({Key, Value}) when is_binary(Value) ->
    application:set_env(?APP, binary_to_atom(Key, utf8), binary_to_list(Value));
process_config_tuple({Key, Value}) ->
    application:set_env(?APP, binary_to_atom(Key, utf8), Value).

parse_port_upstream_resolvers(Value) ->
    IpSplit = binary:split(Value, <<":">>),
    case length(IpSplit) of
        2 -> list_to_integer(binary_to_list(lists:last(IpSplit)));
        1 -> 53
    end.