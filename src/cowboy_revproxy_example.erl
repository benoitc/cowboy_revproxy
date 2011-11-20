-module(cowboy_revproxy_example).
-export([start/0, proxy/1]).

proxy(_Data) ->
    {remote, {"www.google.com", 80}}.

start() ->
    application:start(cowboy),
    application:start(cowboy_revproxy),
        cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_revproxy, [{proxy, {?MODULE, proxy}}]).
