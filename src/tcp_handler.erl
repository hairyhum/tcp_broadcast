-module(tcp_handler).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
  start_receive/1, 
  reply/2
]).

-record(state, {accept, sequence = <<>>}).

%% API functions

start_receive(Pid) ->
  ok = gen_server:cast(Pid, start_receive).

reply(Pid, Data) ->
  ok = gen_server:cast(Pid, {reply, Data}).

start_link(Accept) ->
  gen_server:start_link(?MODULE, [Accept], []).

%% gen_server callbacks

init([Accept]) ->
  {ok, #state{ accept = Accept, sequence = <<>> }}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast({reply, Data}, #state{ accept = Accept } = State) ->
  gen_tcp:send(Accept, Data),
  {noreply, State};
handle_cast(start_receive, #state{ accept = Accept } = State) ->
  next_receive(Accept),
  {noreply, State}.

handle_info({tcp, _Socket, Data}, #state{ accept = Accept, sequence = Sequence } = State) -> 
  {ok, NewSequence} = process_data(Data, Sequence),
  next_receive(Accept),
  {noreply, State#state{ sequence = NewSequence }};
handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};
handle_info(_Message, State) ->
  {noreply, State}.

terminate(_, #state{ accept = Accept }) -> 
  gen_tcp:close(Accept),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Private functions

next_receive(Accept) ->
  inet:setopts(Accept, [{active, once}]).

-spec process_data(binary(), binary()) -> {ok, binary()}.
process_data(Data, Sequence) ->
  {Sending, NewSequence} = parse_sequence(Data, <<>>, Sequence),
  tcp_pool:broadcast({Sending, self()}),
  {ok, NewSequence}.

-spec parse_sequence(binary(), binary(), binary()) -> {iolist(), binary()}.
parse_sequence(<<>>, Sending, Sequence) ->
  {Sending, Sequence};
parse_sequence(<<Char:1/binary, Data/binary>>, Sending, Sequence) ->
  NewSequence = case terminated_sequence(Sequence) of
    true -> Char;
    false -> <<Sequence/binary, Char/binary>>
  end,
  NewSending = case escaped_sequence(NewSequence) of
    true -> Sending;
    false -> <<Sending/binary, Char/binary>>
  end,
  parse_sequence(Data, NewSending, NewSequence).

-spec terminated_sequence(binary()) -> boolean().
terminated_sequence(<<>>) -> false;
terminated_sequence(Sequence) ->
   binary:last(Sequence) == $\n.

-spec escaped_sequence(binary()) -> boolean().
escaped_sequence(<<$\\, _/binary>>) -> true;
escaped_sequence(_) -> false. 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_sequence_test() ->
  ?assertMatch({<<"first sequence\nsecond sequence\nfouth \\sequence">>,<<"fouth \\sequence">>},
    parse_sequence(<<"first sequence\nsecond sequence\n\\third sequence escaped\nfouth \\sequence">>, <<>>, <<"foo">>)),
  ?assertMatch({<<"continues">>,<<"unterminated sequence continues">>},
    parse_sequence(<<"continues">>, <<>>, <<"unterminated sequence ">>)),
  ?assertMatch({<<"continues and terminates\n">>,<<"unterminated sequence continues and terminates\n">>},
    parse_sequence(<<"continues and terminates\n">>, <<>>, <<"unterminated sequence ">>)),
  ?assertMatch({<<"start a new sequence">>,<<"start a new sequence">>},
    parse_sequence(<<"start a new sequence">>, <<>>, <<"terminated sequence\n">>)),
  ?assertMatch({<<"">>,<<"\\ escaped sequence skipped">>},
    parse_sequence(<<"skipped">>, <<>>, <<"\\ escaped sequence ">>)),
  ?assertMatch({<<"skipped">>,<<"skipped">>},
    parse_sequence(<<"sequence\nskipped">>, <<>>, <<"\\ escaped terminated ">>)).

-endif.
