-module(websip).

-include("websip.hrl").

-export([get_page/0, get_error_page/0, post/1, parse_packet/1]).

-define(DOMAIN, "192.168.1.5").

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
    Client1 = string:concat("sip:1002@", ?DOMAIN),
    nksip:start_link(client1, 
        #{sip_from => Client1, 
          plugins => [nksip_uac_auto_auth], 
          sip_listen => "<sip:all:7894>, 
          <sip:all:7895;transport=udp>"
        }),

    PBX_ID = string:concat("sip:", ?DOMAIN),
    nksip_uac:register(client1, PBX_ID,
        [{sip_pass, "45678"}, contact, {meta, ["contact"]}]),
    
    Client2 = "sip:" ++ Phone ++ "@" ++ ?DOMAIN,
    % Client2 = string:concat("sip:1001@", ?DOMAIN),
    nksip_uac:invite(client1, Client2,
        [{add, "x-nk-op", ok}, {add, "x-nk-prov", true},
         {add, "x-nk-sleep", 8000},
         auto_2xx_ack,
         {sip_pass, "45678"} %, {route, "<sip:192.168.2.2;lr>"}
        ]).
    
    % nksip_uac:bye(DlgId, []).