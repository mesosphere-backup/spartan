%%%-------------------------------------------------------------------
%%% @author grubio
%%% @copyright (C) 2016, <STRATIO>
%%% @doc
%%%
%%% @end
%%% Created : 03. ago 2016 14:12
%%%-------------------------------------------------------------------
-module(spartan_port_parse_test).
-author("grubio").

-include_lib("eunit/include/eunit.hrl").

port_include_test() ->
  Port = spartan_app:parse_port_upstream_resolvers(<<"127.0.0.1:8600">>),
  ?assert(Port == 8600).

port_not_include_test() ->
  Port = spartan_app:parse_port_upstream_resolvers(<<"127.0.0.1">>),
  ?assert(Port == 53).