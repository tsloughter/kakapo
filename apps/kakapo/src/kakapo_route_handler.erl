-module(kakapo_route_handler).

-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {}).

init({tcp, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    io:format("Got request~n"),
    {Domain, _} = cowboy_req:host(Req),
    lager:info("Got request with x-kakapo-route, fetching ~p~n", [Domain]),
    send_req_to_host(Req, State, [], <<"localhost">>, 5561),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%
%% Internal functions

forward_to_dyno(Domain, Req, State) ->
    {Host, Port} = kakapo_route:lookup_service(Domain),
    lager:info("Sending request to ~p ~p~n", [Host, Port]),
    send_req_to_host(Req, State, [], Host, Port, true).

send_req_to_host(Req, State, Headers, Host, Port) ->
    send_req_to_host(Req, State, Headers, Host, Port, false).

send_req_to_host(Req, State, Headers, Host, Port, StripKakapoHdr) ->
    {ok, Context} = erlzmq:context(),
	{ok, Req} = erlzmq:socket(Context, [req, {active, false}]),
	ok = erlzmq:connect(Req, "tcp://127.0.0.1:5561"),
    erlzmq:send(Req, <<"Test">>).
                                                % {ok, BackendSocket} = kakapo_route:connect_to_server(Host, Port),
%%     {ok, Req2} = kakapo_route:forward(BackendSocket, Headers, Req, StripKakapoHdr),
%%     {ok, Req2, State}.
