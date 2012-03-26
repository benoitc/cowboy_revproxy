%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of cowboy_revproxy released under the MIT license.
%%% See the NOTICE for more information.


-module(cowboy_revproxy).
-behaviour(cowboy_protocol).


-export([start_link/4]).
-export([init/4]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("cowboy/include/http.hrl").

%% proxy state
-record(stproxy, {
        listener :: pid(),
        socket :: inet:socket(),
        transport :: module(),
        handler :: {module(), any()} | {module(), any(), any()},
        timeout :: timeout(),
        buffer = <<>> :: binary(),
        remote :: any(),
        remote_socket :: inet:socket(),
        remote_transport :: module()
    }).

%% HTTP protocol state
-record(state, {
	listener :: pid(),
	socket :: inet:socket(),
	transport :: module(),
	dispatch :: cowboy_dispatcher:dispatch_rules(),
	handler :: {module(), any()},
	onrequest :: undefined | fun((#http_req{}) -> #http_req{}),
	urldecode :: {fun((binary(), T) -> binary()), T},
	req_empty_lines = 0 :: integer(),
	max_empty_lines :: integer(),
	req_keepalive = 1 :: integer(),
	max_keepalive :: integer(),
	max_line_length :: integer(),
	timeout :: timeout(),
	buffer = <<>> :: binary(),
	hibernate = false :: boolean(),
	loop_timeout = infinity :: timeout(),
	loop_timeout_ref :: undefined | reference()
}).

%% @doc Start a revproxy process
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok | none().
init(ListenerPid, Socket, Transport, Opts) ->
    Handler = proplists:get_value(proxy, Opts),
    Timeout = proplists:get_value(timeout, Opts, 5000),
    ok = cowboy:accept_ack(ListenerPid),
    wait_request(#stproxy{listener=ListenerPid, socket=Socket,
            transport=Transport, handler=Handler, timeout=Timeout}).

-spec wait_request(#stproxy{}) -> ok | none().
wait_request(State=#stproxy{socket=Socket, transport=Transport, timeout=T,
        handler=Handler, buffer=Buffer}) ->

    case Transport:recv(Socket, 0, T) of
        {ok, Data} ->
            Buffer1 = << Buffer/binary, Data/binary >>,
            case call_handler(Handler, Buffer1) of
                stop ->
                    terminate(State);
                {stop, Reply} ->
                Transport:send(Socket, Reply),
                    terminate(State);
                {http, Dispatch} ->
                    #stproxy{listener=Listener} = State,
                    cowboy_http_protocol:parse_request(#state{listener=Listener,
                            socket=Socket, transport=Transport,
                            dispatch=Dispatch, buffer = Buffer1});
                {remote, Remote} ->
                    start_proxy_loop(State#stproxy{buffer=Buffer1, remote=Remote});
                [{remote, Remote}, {data, Data}] ->
                    start_proxy_loop(State#stproxy{buffer=Data, remote=Remote});
                [{remote, Remote}, {data, Data}, {reply, Reply}] ->
                    Transport:send(Socket, Reply),
                    start_proxy_loop(State#stproxy{buffer=Data, remote=Remote});
                _ ->
                    wait_request(State#stproxy{buffer=Buffer1})
            end;
        {error, _Reason} ->
            terminate(State)
    end.


start_proxy_loop(State=#stproxy{remote=Remote, buffer=Buffer}) ->
    case remote_connect(Remote) of
        {Transport, {ok, Socket}} ->
            Transport:send(Socket, Buffer),
            proxy_loop(State#stproxy{remote_socket=Socket,
                    remote_transport=Transport, buffer= <<>> });
        {error, _Error} ->
            terminate(State)
    end.

proxy_loop(State=#stproxy{socket=From, transport=TFrom,
        remote_socket=To, remote_transport=TTo}) ->
    TFrom:setopts(From, [{packet, 0}, {active, once}]),
    TTo:setopts(To, [{packet, 0}, {active, once}]),

    receive
        {_, From, Data} ->
            TTo:send(To, Data),
            proxy_loop(State);
        {_, To, Data} ->
            TFrom:send(From, Data),
            proxy_loop(State);
        {tcp_closed, To} ->
            terminate(State);
        {tcp_closed, From} ->
            remote_terminate(State);
        _ ->
            terminate_all(State)
    end.


call_handler({M, F}, Data) ->
    M:F(Data);
call_handler({M, F, A}, Data) ->
    erlang:apply(M, F, [Data | A]).

-spec terminate(#stproxy{}) -> ok.
terminate(#stproxy{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.


remote_connect({Ip, Port}) ->
    {cowboy_tcp_transport, gen_tcp:connect(Ip, Port, [binary,
                {packet, 0}, {delay_send, true}])};
remote_connect({ssl, Ip, Port, Opts}) ->
    Opts1 = parse_ssl_options(Opts),
    {cowboy_ssl_transport, ssl:connect(Ip, Port, [binary, {packet, 0},
                {delay_send, true} | Opts1])}.

remote_terminate(#stproxy{remote_socket=Socket,
        remote_transport=Transport}) ->
    Transport:close(Socket),
    ok.

terminate_all(State) ->
    remote_terminate(State),
    terminate(State).

-spec parse_ssl_options([ssl:ssloption()]) -> any().
parse_ssl_options(Opts) ->
    parse_ssl_options(Opts, []).

%%% cowboy ssupport only certfile, keyfile & password
parse_ssl_options([{certfile, _}=KV|Rest], Acc) ->
    parse_ssl_options(Rest, [KV|Acc]);
parse_ssl_options([{keyfile, _}=KV|Rest], Acc) ->
    parse_ssl_options(Rest, [KV|Acc]);
parse_ssl_options([{password, _}=KV|Rest], Acc) ->
    parse_ssl_options(Rest, [KV|Acc]);
parse_ssl_options([_KV|Rest], Acc) ->
    parse_ssl_options(Rest, Acc).

