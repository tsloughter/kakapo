-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-record(maestro_domain, {
        name :: binary(),
        domain_group_name :: binary(),
        txid :: non_neg_integer(),
        emitted_at :: non_neg_integer()}).
