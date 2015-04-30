-module(tcp_broadcast_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

start() ->
  application:start(tcp_broadcast).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, Port} = application:get_env(port),
  tcp_broadcast_sup:start_link(Port).

stop(_State) ->
  ok.
