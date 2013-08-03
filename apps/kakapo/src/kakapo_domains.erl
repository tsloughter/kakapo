-module(kakapo_domains).

-export([lookup_domain_group/1]).

-include("kakapo_core.hrl").

lookup_domain_group(Domain) ->
    case ets:lookup(kakapo_domain, Domain) of
        [#kakapo_domain{name=Domain, domain_group_name=DomainGroupName}] ->
            case ets:lookup(kakapo_domain_group, DomainGroupName) of
                [DomainGroup] ->
                    {ok, Domain, DomainGroup};
                [] ->
                    undefined
            end;
        _ ->
            {error, domain_not_found}
    end.
