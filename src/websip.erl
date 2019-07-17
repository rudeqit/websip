-module(websip).

-include("websip.hrl").

-export([get_page/0, get_error_page/1, post/1, parse_packet/1]).

get_page() ->
    {ok, Binary} = file:read_file("priv/www/index.html"),
    Size = erlang:byte_size(Binary),
    BinSize = erlang:integer_to_binary(Size), 
    HTTP = <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n">>,

    <<HTTP/binary, Binary/binary>>.

get_error_page({Class, Reason, _StackTrace}) ->
    BinClass = erlang:atom_to_binary(Class, utf8),
    BinReason = erlang:atom_to_binary(Reason, utf8),
    % BinST = erlang:term_to_binary(StackTrace),
    Size = erlang:byte_size(BinClass) + erlang:byte_size(BinReason) + erlang:byte_size(<<"{, }">>),
    BinSize = erlang:integer_to_binary(Size),
    <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n", "{", BinClass/binary, ", ", BinReason/binary, "}">>.

post(Phone) ->
    Size = erlang:byte_size(Phone),
    BinSize = erlang:integer_to_binary(Size),
    case sip_invite(unicode:characters_to_list(Phone)) of
        {ok, succes} ->
            % {ok, post, Phone};
            <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n", Phone/binary>>;
        {error, R, S} ->
            get_error_page({error, R, S})
    end.

parse_packet(Packet) ->
    case string:split(Packet, "/") of
        [<<"POST ">> | _T] -> 
            case string:split(Packet, "\r\n\r\n") of 
                [_ | Post] when Post =/= [] ->
                    [_ | Phone] = string:split(Post, "phone="), 
                    % {ok, post, unicode:characters_to_list(Phone)};
                    [BinPhone | _] = Phone,
                    {ok, post, BinPhone};
                _ -> {error, badrequest}
            end;
        [<<"GET ">> | _T] -> {ok, get, Packet};
        _ -> {error, badrequest}
    end.

sip_invite(Phone) ->
    try made_call(Phone) of
        ok -> 
            {ok, succes}
    catch
        Class:Reason ->
            {Class, Reason, erlang:get_stacktrace()}
    end.

made_call(Phone) ->
    {ok, Domain} = application:get_env(websip, pbx_domain),
    {ok, Client1_phone} = application:get_env(websip, sip_client),
    Client1 = string:concat(Client1_phone, Domain),

    {ok, Port} = application:get_env(websip, sip_udp_port),
    {ok, Port_rez} = application:get_env(websip, sip_udp_port_reserve),
    Sip_listen = "<" ++ Port ++ ">" ++ "," ++ "<" ++ Port_rez ++ ";transport=udp>", % TODO avoid string concatenation
    
    StartOptions = #{sip_from => Client1, 
                     plugins => [nksip_uac_auto_auth], 
                     sip_listen => Sip_listen},

    case nksip:start_link(client1, StartOptions) of 
        {ok, _} -> ok;
        {error, Term} -> erlang:error(Term)
    end,

    PBX_addr = string:concat("sip:", Domain),
    {ok, Sip_pass} = application:get_env(websip, sip_pass),
    RegOptions = [{sip_pass, Sip_pass}, contact, {meta, ["contact"]}],    

    case nksip_uac:register(client1, PBX_addr, RegOptions) of
        {ok, 200, _} -> ok;
        _ ->
            nksip:stop(client1),
            erlang:error(noregister)
    end,
    
    Client2 = "sip:" ++ Phone ++ "@" ++ Domain, % TODO avoid string concatenation
    InviteOptions = [{add, "x-nk-op", ok}, {add, "x-nk-prov", true},
                        {add, "x-nk-sleep", 8000},
                        auto_2xx_ack,
                        {sip_pass, Sip_pass}],

                        
    case nksip_uac:invite(client1, Client2, InviteOptions) of
        {ok, 200, {dialog, DigId}} ->
            erlang:sleep(5000),
            nksip_uac:bye(DigId, []);
        _ ->
            nksip:stop(client1),
            erlang:error(noinvite)
    end,

    nksip:stop(client1).
