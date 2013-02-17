-module(kakapo_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case kakapo_core_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register(kakapo_core, [{vnode_module, kakapo_core_vnode}]),
            ok = riak_core:register(riak_kv, [{vnode_module, riak_kv_vnode}]),
            %ok = riak_core_ring_events:add_guarded_handler(kakapo_core_ring_event_handler, []),
            %ok = riak_core_node_watcher_events:add_guarded_handler(kakapo_core_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(kakapo_core, self()),
            ok = riak_core_node_watcher:service_up(riak_kv, self()),

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
    lists:foreach(fun({Domain, Value}) ->
                          O = riak_object:new(<<"domains">>, Domain, Value),
                          C:put(O)
                  end, [{<<"zinn">>, {<<"localhost">>, 7999}}
                       ,{<<"localhost">>, {<<"google.com">>, 80}}]).

