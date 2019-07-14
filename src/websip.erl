-module(websip).

-export([parse_packet/1]).

-include("websip.hrl").

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