-module(kakapo_service).

-export([lookup_service/1]).

-include_lib("kakapo_core/include/kakapo_core.hrl").

lookup_service(RouteId) ->
    case ets:lookup(kakapo_route, RouteId) of
        [#kakapo_route{} = P] ->
            {ok, P};
        [] ->
            {error, route_not_found}
    end.

