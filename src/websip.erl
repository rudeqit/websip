-module(websip).

-include("websip.hrl").

-export([get_page/0, get_error_page/0, post/1, parse_packet/1]).

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

parse_packet(Packet) ->
    case string:split(Packet, "/") of
        [<<"POST ">> | _T] -> 
            case string:split(Packet, "\r\n\r\n") of 
                [_ | Post] when Post =/= [] ->
                    [_ | Phone] = string:split(Post, "phone="), 
                    % {ok, post, unicode:characters_to_list(Phone)};
                    [BinPhone | _] = Phone,
                    sip_invite(unicode:characters_to_list(Phone)),
                    {ok, post, BinPhone};
                _ -> {error, badrequest}
            end;
        [<<"GET ">> | _T] -> {ok, get, Packet};
        _ -> {error, badrequest}
    end.

sip_invite(Phone) ->
    {ok, Domain} = application:get_env(websip, pbx_domain),
    {ok, Client1_phone} = application:get_env(websip, sip_client),
    Client1 = string:concat(Client1_phone, Domain),

    {ok, Port} = application:get_env(websip, sip_udp_port),
    {ok, Port_rez} = application:get_env(websip, sip_udp_port_reserve),
    Sip_listen = "<" ++ Port ++ ">" ++ "," ++ "<" ++ Port_rez ++ ";transport=udp>", % TODO avoid string concatenation
    nksip:start_link(client1, 
        #{sip_from => Client1, 
          plugins => [nksip_uac_auto_auth], 
          sip_listen => Sip_listen
        }),

    PBX_addr = string:concat("sip:", Domain),
    {ok, Sip_pass} = application:get_env(websip, sip_pass),
    nksip_uac:register(client1, PBX_addr,
        [{sip_pass, Sip_pass}, contact, {meta, ["contact"]}]),
    
    Client2 = "sip:" ++ Phone ++ "@" ++ Domain, % TODO avoid string concatenation
    nksip_uac:invite(client1, Client2,
        [{add, "x-nk-op", ok}, {add, "x-nk-prov", true},
         {add, "x-nk-sleep", 8000},
         auto_2xx_ack,
         {sip_pass, Sip_pass}
        ]).
    
    % nksip_uac:bye(DlgId, []).