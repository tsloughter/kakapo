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
    case cowboy_req:header(<<"x-kakapo-route">>, Req) of
        {undefined, Req3} ->
            Headers = [{<<"x-kakapo-route">>, <<"true">>}],            
            case kakapo_core:lookup_router(Domain) of
                local_vnode ->
                    forward_to_dyno(Domain, Req, State);
                {ok, Host, Port} ->
                    send_req_to_host(Req3, State, Headers, Host, Port)
            end;
        {_, _} ->
            lager:info("Got request with x-kakapo-route, fetching ~p~n", [Domain]),            
            forward_to_dyno(Domain, Req, State)
    end.

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
    {ok, BackendSocket} = kakapo_route:connect_to_server(Host, Port),
    {ok, Req2} = kakapo_route:forward(BackendSocket, Headers, Req, StripKakapoHdr),
    {ok, Req2, State}.
