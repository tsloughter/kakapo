-module(kakapo_route_handler).

-export([init/3, 
         handle/2, 
         terminate/3]).

-record(state, {}).

init({tcp, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Domain, Req2} = cowboy_req:host(Req),
    case cowboy_req:header(<<"x-kakapo-route">>, Req2) of
        {undefined, Req3} ->
            Headers = [{<<"x-kakapo-route">>, <<"true">>}],
            {ok, Host, Port} = kakapo_core:lookup_router(Domain),
            send_req_to_host(Req3, State, Headers, Host, Port);
        {_, _} ->
            io:format("Got request with x-kakapo-route, fetching ~p~n", [Domain]),
            Headers = [],
            {ok, Host, Port} = kakapo_route:lookup_service(Domain),
            send_req_to_host(Req2, State, Headers, Host, Port)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%
%% Internal functions

send_req_to_host(Req, State, Headers, Host, Port) ->
    {ok, BackendSocket} = kakapo_route:connect_to_server(Host, Port),
    {ok, Req2} = kakapo_route:forward(BackendSocket, Headers, Req),
    {ok, Req2, State}.
