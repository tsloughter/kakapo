-record(kakapo_domain, {name :: binary(),
                        domain_group_name :: binary(),
                        txid :: non_neg_integer(),
                        emitted_at :: non_neg_integer()}).

-record(kakapo_domain_group, {name :: binary(),
                              route_id,
                              heroku_log_token :: binary(),
                              maintenance_mode=false :: boolean(),
                              maintenance_page_url :: binary(),
                              error_page_url :: binary(),
                              feature_flags=[] :: [atom() | binary()],
                              num_backends = 0 :: non_neg_integer(),
                              txid :: integer(),
                              emitted_at :: integer()}).

-record(kakapo_route, {id,
                       upid,
                       txid :: integer(),
                       since :: integer(),
                       route_id,
                       state,
                       release_id :: integer() | undefined,
                       app_id :: integer(),
                       ps :: binary(),
                       ip = undefined :: 'undefined' | binary(),
                       port = undefined :: integer() | 'undefined',
                       created_at :: integer(),
                       emitted_at}).
