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
            {{ok, Host}, Port} = {kakapo_core:lookup_router(Domain), 8080};
        {_, Req3} ->
            Headers = [],
            {ok, Host, Port} = kakapo_route:lookup_service(Domain)
    end,

    {ok, BackendSocket} = kakapo_route:connect_to_server(Host, Port),
    {ok, Req4} = kakapo_route:forward(BackendSocket, Headers, Req3),
    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%
%% Internal functions
