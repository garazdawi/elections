
-record(person, {personnummer :: pos_integer(),
		 kommun :: string()}).

-record(vote, {personnummer :: pos_integer(),
	       partiet :: string(),
	       vote_type :: riksdag | lansting | kommun,
	       locality :: string()}).

-record(contract, {id :: term(),
    		   function :: fun((#vote{}, term()) -> term()),
		   state :: term()}).

-record(block, {previous_blockhash :: binary(),
		nonce :: pos_integer(),
		signature :: binary(),
		public_key :: binary(),
		votes = [] :: list(),
    		contracts = [#contract{}] :: list()}).

