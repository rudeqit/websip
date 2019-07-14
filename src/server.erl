-module(server).
-behavior(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("websip.hrl").

-record(state, {socket :: port(),
                accept_maps = maps:new() :: map(),
                packet :: binary(),
                request = #request{} %% type?
        }).

%%% module API

start_link(Socket) ->
    io:format("Server started ~n"),
    gen_server:start_link(?MODULE, [Socket], []).

%%% gen server API

init([Socket]) ->
    gen_server:cast(self(), listen),
    {ok, #state{socket = Socket}}.

handle_call(_Any, _From, State) ->
    {reply, ok, State}.


handle_cast(listen, State) ->
    ListenSocket = State#state.socket,
    {ok, AcceptSock} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSock, [{active, once}]),
    {noreply, State#state{socket=AcceptSock}};

handle_cast({receiverd, Packet}, State) ->
    AcceptSock = State#state.socket,
    Response = get_response(State, Packet),
    gen_tcp:send(AcceptSock, Response),
    gen_tcp:close(AcceptSock),
    websip_sup:start_listener(),
    {stop, normal, State}.


handle_info({tcp, _ClientSocket, Packet}, State) ->
    NewState = State#state{packet = Packet},
    gen_server:cast(self(), {receiverd, Packet}),
    {noreply, NewState};

handle_info({tcp_closed, _ClientSocket}, State) ->
    AcceptSock = State#state.socket,
    gen_tcp:close(AcceptSock),
    websip_sup:start_listener(),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%% inner function

get_response(State, Packet) ->
    AcceptSock = State#state.socket,
    inet:setopts(AcceptSock, [{active, once}]),
    case websip:parse_packet(Packet) of 
        {ok, get, Packet} -> get_page();
        {ok, post, Phone} -> post(Phone);
        {error, _Reason} -> get_error_page()
    end.


get_page() ->
    {ok, Binary} = file:read_file("priv/www/index.html"),
    Size = erlang:byte_size(Binary),
    BinSize = erlang:integer_to_binary(Size), 
    HTTP = <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n">>,

    <<HTTP/binary, Binary/binary>>.

get_error_page() ->
    Size = erlang:byte_size(<<"error">>),
    BinSize = erlang:integer_to_binary(Size),
    <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\nerror">>.

post(Phone) ->
    Size = erlang:byte_size(Phone),
    BinSize = erlang:integer_to_binary(Size),
    <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n", Phone/binary>>.% sip_call(Phone);