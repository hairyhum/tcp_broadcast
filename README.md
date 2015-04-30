# TCP broadcast server

Require erlang 16B or higher.

Application using rebar for building and testing with `eunit` and `common test`.

Port configuration located in `src/tcp_broadcast.app.src` in `env` section.


Quick start:

`make` - compile and run application in erlang shell

### Make tasks:

`make compile` - compile application with rebar

`make console` - start application in erlang shell

`make background` - start application without shell

`make test` - run tests (eunit and common test)

Application structure:

```
tcp_broadcast_app
|
--- tcp_broadcast_sup
    |
    --- tcp_acceptor (listen for new connections)
    |
    --- tcp_pool (pool of connection handlers)
        |
        --- tcp_handler (handle requests, send messages)
        |
        --- tcp_handler
        |
        --- ...
```


