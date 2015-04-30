%% common_test suite for tcp_broadcast

-module(tcp_broadcast_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> [].

all() ->
    [ {exports, Functions} | _ ] = ?MODULE:module_info(),
    [ FName || {FName, _} <- lists:filter(
                               fun ({module_info,_}) -> false;
                                   ({all,_}) -> false;
                                   ({init_per_suite,1}) -> false;
                                   ({end_per_suite,1}) -> false;
                                   ({_,1}) -> true;
                                   ({_,_}) -> false
                               end, Functions)].

init_per_suite(Config) ->
    application:start(tcp_broadcast),
    Config.

end_per_suite(_Config) ->
    application:stop(tcp_broadcast),
    ok.


test_tcp_broadcast() ->
    [{userdata,[{doc,"Testing the tcp_broadcast application."}]}].

test_tcp_broadcast(_Config) ->
    {ok, Port} = application:get_env(tcp_broadcast, port),
    {ok, Listener} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    {ok, Listener1} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    {ok, Sender} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    
    ok = gen_tcp:send(Sender, <<"Foo\n">>),
    {ok, <<"Foo\n">>} = gen_tcp:recv(Listener, 0),
    {ok, <<"Foo\n">>} = gen_tcp:recv(Listener1, 0).

test_tcp_broadcast_sequences() ->
    [{userdata,[{doc,"Testing the tcp_broadcast with skipping sequences."}]}].

test_tcp_broadcast_sequences(_Config) ->
    {ok, Port} = application:get_env(tcp_broadcast, port),
    {ok, Listener} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    {ok, Listener1} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    {ok, Sender} = gen_tcp:connect("localhost", Port, [{active, false}, binary]),
    
    ok = gen_tcp:send(Sender, <<"Foo\n\\Bar">>),
    {ok, <<"Foo\n">>} = gen_tcp:recv(Listener, 0),
    {ok, <<"Foo\n">>} = gen_tcp:recv(Listener1, 0),

    ok = gen_tcp:send(Listener, <<"Ccombobreaker\n">>),
    {ok, <<"Ccombobreaker\n">>} = gen_tcp:recv(Sender, 0),
    {ok, <<"Ccombobreaker\n">>} = gen_tcp:recv(Listener1, 0),    

    % Breaker should not work. Because from other client.
    ok = gen_tcp:send(Sender, <<"Baz\nFuzz">>),
    {ok, <<"Fuzz">>} = gen_tcp:recv(Listener, 0),
    {ok, <<"Fuzz">>} = gen_tcp:recv(Listener1, 0).
