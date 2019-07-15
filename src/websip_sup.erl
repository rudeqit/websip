%%%-------------------------------------------------------------------
%% @doc websip top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(websip_sup).

-behaviour(supervisor).

-export([start_link/0, start_listener/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, IP} = application:get_env(websip, webserv_ip),
    {ok, Port} = application:get_env(websip, webserv_port),
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 0}, {ip, IP}]),    
    spawn_link(fun initial_listeners/0),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},
    ChildSpecs = [#{id => socket,
                   start => {server, start_link, [ListenSock]},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker,
                   modules => [server]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

start_listener() ->
    supervisor:start_child(?MODULE, []).

initial_listeners() ->
    [start_listener() || _ <- lists:seq(1, 5)],
    ok.