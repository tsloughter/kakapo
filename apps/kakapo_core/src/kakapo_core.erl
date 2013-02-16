-module(kakapo_core).
-include("kakapo_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         lookup_router/1
        ]).

%% Public API

% @doc Pings a random vnode to make sure communication is functional
lookup_router(Domain) ->
    DocIdx = riak_core_util:chash_key({Domain, <<"domain">>}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, kakapo_core),
    [{IndexNode, _Type}] = PrefList,
    io:format("Index Node ~p~n", [IndexNode]),
    [_, Host] = string:tokens(atom_to_list(element(2, IndexNode)), "@"),    
    io:format("Host ~p~n", [Host]),
    {ok, list_to_binary(Host)}.
    %riak_core_vnode_master:sync_spawn_command(IndexNode, ping, kakapo_core_vnode_master).
