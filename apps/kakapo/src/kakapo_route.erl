-module(kakapo_route).

-export([lookup_service/1
        ,connect_to_server/2
        ,forward/3
        ,forward/4]).

-define(CONNECT_TIMEOUT, 5000).

lookup_service(Domain) ->
    kakapo_core:lookup_service(Domain).

connect_to_server(Service, Port) ->
    Opts = [binary, {packet, http_bin}, {packet_size, 1024 * 1024}, {recbuf, 1024 * 1024}, {active, once}, {reuseaddr, true}],
    gen_tcp:connect(binary_to_list(Service), Port, Opts, ?CONNECT_TIMEOUT).

forward(BackendSocket, AdditionalHeaders, Req) ->
    forward(BackendSocket, AdditionalHeaders, Req, false).

forward(BackendSocket, AdditionalHeaders, Req, StripKakapoHdr) ->
    Request = make_request({cowboy_req:get(method, Req), cowboy_req:get(path, Req), cowboy_req:get(version, Req)}),
    Headers = cowboy_req:get(headers, Req),    
    gen_tcp:send(BackendSocket, Request),    
    Headers1 = lists:foldl(
                 fun(H, Acc) ->
                        case H of
                             {<<"x-kakapo-route">>, _} when StripKakapoHdr == true -> Acc;
                             _ -> make_header(H) ++ Acc
                        end
                 end, [<<"\r\n">>], Headers++AdditionalHeaders),
    gen_tcp:send(BackendSocket, Headers1),
    relay(cowboy_req:get(socket, Req), BackendSocket, Req),
    {ok, Req}.

relay(ClientSocket, BackendSocket, Req) ->
    inet:setopts(ClientSocket, [{packet, raw}, {active, once}]),
    inet:setopts(BackendSocket, [{active, once}]),
    receive
        {tcp, ClientSocket, Data} ->
            case gen_tcp:send(BackendSocket, Data) of
                ok ->
                    relay(ClientSocket, BackendSocket, Req);
                {error, _Reason} ->
                    error
            end;
        {tcp, BackendSocket, Data} ->
            case gen_tcp:send(ClientSocket, Data) of
                ok ->
                    relay(ClientSocket, BackendSocket, Req);
                {error, _Reason} ->
                    error
            end;
        {tcp_closed, BackendSocket} ->
            gen_tcp:close(ClientSocket),
            exit(normal);
        {tcp_closed, ClosedSock} ->
            _WhichSocket =
                case ClosedSock of
                    ClientSocket -> client;
                    BackendSocket -> backend
                end,
            error;
        {http, BackendSocket, {http_response, HttpVersion, HttpRespCode, RespStatus}} ->
            Packet = [make_version(HttpVersion), <<" ">>, make_io(HttpRespCode), <<" ">>, RespStatus, <<"\r\n">>],
            case gen_tcp:send(ClientSocket, Packet) of
                ok ->
                    relay(ClientSocket, BackendSocket, Req);
                {error, _Reason} ->
                    error
            end;
        {http, BackendSocket, {http_header, _, 'Connection', _, _Val}} ->
            relay(ClientSocket, BackendSocket, Req);
        {http, BackendSocket, {http_header, _, HttpField, _, Value}} ->
            case gen_tcp:send(ClientSocket, [make_header({HttpField, Value})]) of
                ok ->
                    relay(ClientSocket, BackendSocket, Req);
                {error, _Reason} ->
                    error
            end;
        {http, BackendSocket, http_eoh} ->
            case gen_tcp:send(ClientSocket, [make_header({'Connection', "close"}), <<"\r\n">>]) of
                ok ->
                    inet:setopts(BackendSocket, [{packet, raw}]),
                    relay(ClientSocket, BackendSocket, Req);
                {error, _Reason} ->
                    error
            end;
        {http, BackendSocket, {http_error, _HttpString}} ->
            gen_tcp:close(ClientSocket),
            gen_tcp:close(BackendSocket),
            exit(normal);
        {error, Reason} ->
            exit({error, Reason})
    end.

make_request({M, Path, Version}) ->
    [make_io(M), <<" ">>, Path, <<" ">>, make_version(Version), <<"\r\n">>].

make_header({K, V}) ->
    [make_io(K), <<": ">>, V, <<"\r\n">>].

make_version({1, 1}) ->
    <<"HTTP/1.1">>;
make_version(_) ->
    <<"HTTP/1.0">>.

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.
