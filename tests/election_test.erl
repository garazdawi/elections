-module(election_test).

-export([test/0]).

test() ->
    [run_test(F) || F <- tests()].

run_test(F) ->
    application:start(elections),
    F(),
    application:stop(elections).

tests() ->
    [fun vote/0].

vote() ->
    ok = vote_chain:vote(123, "kalle anka", riksdag),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag, undefined),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    #{"kalle anka" := 2} = vote_chain:count_votes(riksdag, undefined),
    ok.
