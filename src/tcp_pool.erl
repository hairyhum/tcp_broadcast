-module(tcp_pool).
-behaviour(supervisor).

-export([
  start_link/0, 
  start_handler/1,
  broadcast/1]).
-export([init/1]).


start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
    {simple_one_for_one, 1000, 10}, [
      {socket, {tcp_handler, start_link, []}, temporary, 1000, worker, [tcp_handler]}]}}.

start_handler(Accept) ->
  supervisor:start_child(?MODULE, [Accept]).

broadcast({Message, Sender}) when is_pid(Sender) ->
  [ tcp_handler:reply(Pid, Message) 
    || {_Id, Pid, _, _} <- supervisor:which_children(?MODULE), 
    is_pid(Pid), Pid =/= Sender ].