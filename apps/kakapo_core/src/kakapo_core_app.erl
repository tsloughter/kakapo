-module(kakapo_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("kakapo_core.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case kakapo_core_sup:start_link() of
        {ok, Pid} ->
            ets:new(kakapo_domain, [set, public, named_table, {keypos, 2}]),
            ets:new(kakapo_domain_group, [set, public, named_table, {keypos, 2}]),
            ets:new(kakapo_route, [set, public, named_table, {keypos, 2}]),
            
            ok = riak_core:register(kakapo_core, [{vnode_module, kakapo_core_vnode}]),
            ok = riak_core:register(riak_kv, [{vnode_module, riak_kv_vnode}]),
            %ok = riak_core_ring_events:add_guarded_handler(kakapo_core_ring_event_handler, []),
            %ok = riak_core_node_watcher_events:add_guarded_handler(kakapo_core_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(kakapo_core, self()),
            ok = riak_core_node_watcher:service_up(riak_kv, self()),

            riak_core_bucket:set_bucket(<<"app">>, [{precommit, [{struct, [{<<"mod">>, <<"kakapo_core_utils">>},
                                                                           {<<"fun">>,<<"ets_update">>}]}]}]),
            
            %% Test Data
            load_test_data(),
            
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

load_test_data() ->                              
    {ok, C} = riak:local_client(),
    lists:foreach(fun({DomainName, DomainGroupName, RouteId, IP, Port}) ->
                          Domain = #kakapo_domain{name = DomainName,
                                                  domain_group_name = DomainGroupName},

                          DomainGroup = #kakapo_domain_group{name = DomainGroupName,
                                                             route_id = RouteId},

                          Service = #kakapo_route{id = RouteId,
                                                  ip = IP,
                                                  port = Port},

                          App = {Domain, DomainGroup, Service},
                          
                          AppObject = riak_object:new(<<"app">>, DomainGroupName, App),

                          C:put(AppObject)
                  end, [{<<"httpbin.org">>, <<"httpbin_group">>, <<"httpbin_route">>, <<"httpbin.org">>, 80}
                       ,{<<"localhost">>, <<"local_group">>, <<"local_route">>, <<"google.com">>, 80}]).

