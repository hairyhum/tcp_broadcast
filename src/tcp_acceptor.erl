-module(tcp_acceptor).

-export([start_link/1, init/1]).

start_link(Port) ->
  {ok, spawn_link(?MODULE, init, [Port])}.

init(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
  accept_loop(Listen).

accept_loop(Listen) ->
  {ok, Accept} = gen_tcp:accept(Listen),
  {ok, Pid} = tcp_pool:start_handler(Accept),
  ok = gen_tcp:controlling_process(Accept, Pid),
  tcp_handler:start_receive(Pid),
  accept_loop(Listen).