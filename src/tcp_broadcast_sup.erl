-module(tcp_broadcast_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
  {ok, { 
    {one_for_one, 10, 10}, [
      {tcp_acceptor, {tcp_acceptor, start_link, [Port]}, permanent, 2000, worker, [tcp_acceptor]},     
      {tcp_pool, {tcp_pool, start_link, []}, permanent, infinity, supervisor, [tcp_pool]}]}}.

