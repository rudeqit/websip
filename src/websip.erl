-module(websip).

-export([get_page/0, get_error_page/1, post/2, parse_packet/1]).

-include_lib("../nksip/include/nksip.hrl").

-compile([{parse_transform, lager_transform}]).

%%% Return priv/www/index.html page to the HTTP GET request 
get_page() ->   
    {ok, Binary} = file:read_file("priv/www/index.html"),
    Size = erlang:byte_size(Binary),
    BinSize = erlang:integer_to_binary(Size), 
    HTTP = <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n">>,

    <<HTTP/binary, Binary/binary>>.


%%% Return page with the reason of the error to the Web
get_error_page({Class, Reason, _StackTrace}) ->
    BinClass = erlang:atom_to_binary(Class, utf8),
    BinReason = erlang:atom_to_binary(Reason, utf8),
    Size = erlang:byte_size(BinClass) + erlang:byte_size(BinReason) +  erlang:byte_size(<<": ">>),
    BinSize = erlang:integer_to_binary(Size),
    <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\n", BinClass/binary, ": ", BinReason/binary>>.

%%% Processes the HTTP POST request by the specified Phone&Text and forms the answer
post(Phone, Text) ->
    Size = erlang:byte_size(Phone) + erlang:byte_size(<<"Successfully!  heard your message">>),
    BinSize = erlang:integer_to_binary(Size),
    case sip_invite(unicode:characters_to_list(Phone), unicode:characters_to_list(Text)) of
        {ok, succes} ->
            <<"HTTP/1.1 200 OK\r\nContent-Length: ", BinSize/binary, "\r\n\r\nSuccessfully! ", Phone/binary, " heard your message">>;
        {C, R, S} ->
            get_error_page({C, R, S})
    end.

%%% Parsing msg from client
parse_packet(Packet) ->
    case string:split(Packet, "/") of
        [<<"POST ">> | _T] -> 
            case string:split(Packet, "\r\n\r\n") of 
                [_ | Post] when Post =/= [] ->
                    [PhonePart | TextPart] = string:split(Post, "&"),
                    [_ | Phone] = string:split(PhonePart, "phone="),
                    [_ | Text] = string:split(erlang:hd(TextPart), "text="), 
                    {ok, post, erlang:hd(Phone), erlang:hd(Text)};
                _ -> {error, badrequest, erlang:get_stacktrace()}
            end;
        [<<"GET ">> | _T] -> {ok, get, Packet};
        _ -> {error, badrequest, erlang:get_stacktrace()}
    end.

%%% Just trying make the call
sip_invite(Phone, Text) ->
    try make_call(Phone, Text) of
        ok -> 
            {ok, succes}
    catch
        Class:Reason ->
            {Class, Reason, erlang:get_stacktrace()}
    end.

%%% Launches SIP client, registers and does invite
make_call(Phone, Text) ->
    {ok, PBX_Domain} = application:get_env(websip, pbx_domain),
    {ok, PBX_Ip} = application:get_env(websip, pbx_ip),
    {ok, Client} = application:get_env(websip, client),
    {ok, Client_Pass} = application:get_env(websip, client_pass),
    {ok, UDP_Port} = application:get_env(websip, udp_port),
    {ok, UDP_Port_Reserve} = application:get_env(websip, udp_port_reserve),
    % {ok, Route} = application:get_env(websip, route),     % if u need route

    Client1 = string:concat(Client, PBX_Domain),
    Sip_listen = "<" ++ UDP_Port ++ ">" ++ "," ++ "<" ++ UDP_Port_Reserve ++ ";transport=udp>",
    StartOptions = #{sip_from => Client1, 
                     plugins => [nksip_uac_auto_auth], 
                     sip_listen => Sip_listen},

    case nksip:start_link(client1, StartOptions) of 
        {ok, _} -> ok;
        {error, Term} -> 
            erlang:error(Term)
    end,

    PBX_Addr = string:concat("sip:", PBX_Ip),
    RegOptions = [{sip_pass, Client_Pass}, contact, {meta, ["contact"]}],    

    nksip_uac:register(client1, PBX_Addr, RegOptions),

    Client2 = "sip:" ++ Phone ++ "@" ++ PBX_Domain,
    
    SDP = #sdp{address = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_Ip)},
               connect = {<<"IN">>, <<"IP4">>, erlang:list_to_binary(PBX_Ip)},
               time = [{0, 0, []}],
               medias = [#sdp_m{media = <<"audio">>,
                                port = 9990,
                                proto = <<"RTP/AVP">>,
                                fmt = [<<"0">>, <<"101">>],
                                attributes = [{<<"sendrecv">>, []}]
                                }
                        ]
                },
    
    InviteOptions = [{add, "x-nk-op", ok}, 
                     {add, "x-nk-prov", true},
                     {add, "x-nk-sleep", 8000},
                     auto_2xx_ack,
                     {sip_dialog_timeout, 40000},   % TODO: fix timeout
                     {sip_pass, Client_Pass},
                     {body, SDP}
                     % ,{route, Route},     % if u need route
                    ],

    invite(3, Client2, InviteOptions, Text),    % insofar as timeout didn't work trying to invite 3 times              
    nksip:stop(client1).

%%% Is trying make invite until Acc > 0
invite(0, _, _, _) ->
    erlang:error(noinvite);
invite(Acc, Client2, InviteOps, Text) when Acc > 0 ->
    case nksip_uac:invite(client1, Client2, InviteOps) of 
        {ok, 200, [{dialog, DlgId}]} ->
            {ok, SDPRemoteVoice} = nksip_dialog:get_meta(invite_remote_sdp, DlgId),
            [SDP_M | _] = SDPRemoteVoice#sdp.medias,
            Port = SDP_M#sdp_m.port,

            {ok, PBX_Ip} = application:get_env(websip, pbx_ip),
            GenVoice = "wget -O priv/voice/generate.wav \"https://tts.voicetech.yandex.net/generate?format=wav&lang=ru_RU&key=069b6659-984b-4c5f-880e-aaedcfd84102&text=" 
                        ++ Text ++ "\"",
            RmOld = "rm priv/voice/output.wav",
            ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 priv/voice/output.wav",
            StartVoice = "./voice_client priv/voice/output.wav " ++ PBX_Ip ++ " " ++ erlang:integer_to_list(Port),
            Cmd = GenVoice ++ " && " ++ RmOld ++ " && " ++ ConvertVoice ++ " && " ++ StartVoice,
            os:cmd(Cmd),
            nksip_uac:bye(DlgId, []),
            ok;
        _Error ->
            timer:sleep(4000), 
            invite(Acc - 1, Client2, InviteOps, Text)
    end.
