-module(election_SUITE).

-export_all().

all() ->
    [vote].

init_per_testcase(_, _) ->
    application:start(elections).

end_per_testcase(_, _) ->
    application:stop(elections).

vote(_) ->
    ok = vote_chain:vote(123, "kalle anka", riksdag),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag, undefined),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag, undefined),
    ok.
