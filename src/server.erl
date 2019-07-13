-module(server).
-behavior(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket :: port(),
                accept_maps = maps:new() :: map(),
                packet :: binary()
        }).

%%% module API

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).



% something(Any) ->
%     gen_server:call(?MODULE, {something, Any}).


%%% gen server API

init([Socket]) ->
    gen_server:cast(self(), listen),
    {ok, #state{socket = Socket}}.

handle_call({something, _Something}, _From, State) ->   % or (smthg_get, _From, State)
    {reply, ok, State}.


handle_cast(listen, State) ->
    ListenSocket = State#state.socket,
    {ok, AcceptSock} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSock, [{packet, line}, {active, once}]),

    % io:format("Socket ~p session started ~n", AcceptSock),

    {noreply, State#state{socket=AcceptSock}};

handle_cast(receiverd, State) ->
    AcceptSock = State#state.socket,
    Response = <<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!">>,
    gen_tcp:send(AcceptSock, Response),
    gen_tcp:close(AcceptSock),
    websip_sup:start_listener(),
    {stop, normal, State}.

% handle_cast(_Any, State) ->
%     {noreply, State}.


handle_info({tcp, _ClientSocket, Packet}, State) ->
    State#state{packet = Packet},
    gen_server:cast(self(), receiverd),
    {noreply, State};

handle_info({tcp_closed, _ClientSocket}, State) ->
    AcceptSock = State#state.socket,
    gen_tcp:close(AcceptSock),
    {stop, normal, State}.

% handle_info(_Request, State) ->
%     {noreply, State};

% handle_info(_Request, State) ->
%     {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%% inner function
