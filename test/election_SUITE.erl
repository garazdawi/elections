-module(election_SUITE).

-compile(export_all).

all() ->
    [vote].

init_per_testcase(_, Config) ->
    application:start(elections),
    Config.

end_per_testcase(_, _) ->
    vote_chain:clear(),
    application:stop(elections).

vote(_) ->
    ok = vote_chain:vote(123, "kalle anka", riksdag),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag),
    error = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag),
    ok = vote_chain:vote(123, "kalle anka", lansting, "vg"),
    ok = vote_chain:vote(124, "kalle anka", lansting, "vg"),
    ok.
