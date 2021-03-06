#!/usr/bin/env sh
APP_NAME = tcp_broadcast
PWD=`pwd`
export PATH := /opt/bin:$(PATH)

all: compile console

compile:
	./rebar compile

clean:
	./rebar clean

background:
	erl -noshell -boot start_sasl -pa ebin -s tcp_broadcast_app start

console:
	erl -boot start_sasl -pa ebin -s tcp_broadcast_app start

test: compile
	./rebar eunit
	./rebar ct

build_plt:
	ERL_LIBS=$(PWD)/deps dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc -r deps
analyze: compile 
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r -I deps --verbose  ebin

