-module(kakapo_core_utils).

-export([ets_update/1]).

ets_update(Object) ->
    lager:info("In precommit hook update, adding values to ets tables"),
    case riak_object:get_value(Object) of
        {Domain, DomainGroup, Service} ->
            true = ets:insert(kakapo_domain, Domain),
            true = ets:insert(kakapo_domain_group, DomainGroup),
            true = ets:insert(kakapo_route, Service);
        Tuple ->
            true = ets:insert(element(1, Tuple), Tuple)
    end,

    Object.
