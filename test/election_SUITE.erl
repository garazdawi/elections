-module(election_SUITE).

-compile(export_all).

-include_lib("elections/include/elections.hrl").

all() ->
    [vote, contract].

init_per_testcase(_, Config) ->
    vote_chain:clear_tables(),
    application:start(elections),
    Config.

end_per_testcase(_, _) ->
    application:stop(elections).

vote(_) ->
    vote_chain:add_contract(has_voted, fun unique/2, []),
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
    error = vote_chain:vote(127, "sigge", kommun, "gåseborg"),
    ok = vote_chain:vote(130, "sigge", kommun, "ankeborg"),
    error = vote_chain:vote(128, "sigge", kommun, "ankeborg"),
    #{"kalle anka" := 2, "knatte" := 1,
      "tjatte" := 1} = vote_chain:count_votes(riksdag),
    #{"kalle anka" := 3, "tjatte" := 1,
      "joakim" := 1, "sigge" := 2} = vote_chain:count_votes(kommun, "ankeborg"),
    #{} = vote_chain:count_votes(kommun, "gåseborg"),
    ok.

contract(_) ->
    vote_chain:add_contract(has_voted, fun unique/2, []),
    ok = vote_chain:vote(123, "kalle anka", riksdag),
    ok = vote_chain:vote(124, "kalle anka", riksdag),
    vote_chain:add_contract(
      riksdag_max,
      fun(#vote{ vote_type = riksdag }, Acc) ->
              if Acc < 5 ->
                      Acc + 1
              end;
         (_, Acc) ->
              Acc
      end, 0),
    ok = vote_chain:vote(125, "kalle anka", riksdag),
    ok = vote_chain:vote(126, "kalle anka", riksdag),
    error = vote_chain:vote(127, "kalle anka", riksdag).

unique(#vote{ personnummer = PN } = V, Acc) ->
    ct:pal("Vote: ~p", [V]),
    ct:pal("Acc: ~p", [Acc]),
    case lists:member(PN, Acc) of
        false ->
            [PN | Acc]
    end.
