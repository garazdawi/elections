-module(election_SUITE).

-compile(export_all).

all() ->
    [vote].

init_per_testcase(_, Config) ->
    application:start(elections),
    vote_chain:clear_tables(),
    Config.

end_per_testcase(_, _) ->
    application:stop(elections).

vote(_) ->
    ok = vote_chain:vote(123, "kalle anka", riksdag),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag),
    error = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag),
    ok = vote_chain:vote(123, "kalle anka", kommun, "ankeborg"),
    ok = vote_chain:vote(124, "kalle anka", kommun, "ankeborg"),
    ok = vote_chain:vote(125, "knatte", riksdag),
    ok = vote_chain:vote(126, "tjatte", riksdag),
    ok = vote_chain:vote(125, "kalle anka", kommun, "ankeborg"),
    ok = vote_chain:vote(126, "tjatte", kommun, "ankeborg"),
    ok = vote_chain:vote(127, "joakim", kommun, "ankeborg"),
    ok = vote_chain:vote(128, "sigge", kommun, "ankeborg"),
    error = vote_chain:vote(129, "sigge", kommun, "gåseborg"),
    ok = vote_chain:vote(130, "sigge", kommun, "ankeborg"),
    error = vote_chain:vote(128, "sigge", kommun, "ankeborg"),
    #{"kalle anka" := 2, "knatte" := 1,
      "tjatte" := 1} = vote_chain:count_votes(riksdag),
    #{"kalle anka" := 3, "tjatte" := 1,
      "joakim" := 1, "sigge" := 2} = vote_chain:count_votes(kommun, "ankeborg"),
    #{"sigge" := 1} = vote_chain:count_votes(kommun, "gåseborg"),
    ok.
