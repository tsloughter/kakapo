-module(kakapo_dyno_route_handler).

-export([init/3, 
         handle/2, 
         terminate/3]).

-record(state, {}).

init({tcp, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Domain, Req2} = cowboy_req:host(Req),
    {ok, Host, Port} = kakapo_route:lookup_service(Domain),
    {ok, BackendSocket} = kakapo_route:connect_to_server(Host, Port),
    {ok, Req3} = kakapo_route:forward(BackendSocket, Req2),
    {ok, Req3, State}.    

terminate(_Reason, _Req, _State) ->
    ok.

%%
%% Internal functions
