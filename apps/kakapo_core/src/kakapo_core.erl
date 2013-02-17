-module(kakapo_core).
-include("kakapo_core.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
        lookup_router/1
        ,lookup_service/1
        ]).

%% Public API

% @doc Pings a random vnode to make sure communication is functional
lookup_router(Domain) ->
    DocIdx = riak_core_util:chash_key({<<"domains">>, Domain}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, kakapo_core),
    Node = node(),
    case PrefList of
        [{{_P, Node}, _Type}] ->
            lager:info("Local vnode can serve request~n"),
            local_vnode;
        [{IndexNode, _Type}] ->
            lager:info("Index Node ~p~n", [IndexNode]),
            ["kakapo"++Port, Host] = string:tokens(atom_to_list(element(2, IndexNode)), "@"),
            lager:info("Host ~p Port ~p~n", [Host, Port]),
            {ok, list_to_binary(Host), list_to_integer(Port)}
    end.

    %riak_core_vnode_master:sync_spawn_command(IndexNode, ping, kakapo_core_vnode_master).

lookup_service(Domain) ->
    [{_, V}] = ets:lookup(domains_table, {<<"domains">>, Domain}),
    V.
