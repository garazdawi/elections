%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2018 10:06
%%%-------------------------------------------------------------------

-record(person, {personnummer :: pos_integer(),
		 kommun :: string()}).

-record(vote, {personnummer :: pos_integer(),
	       partiet :: string(),
	       vote_type :: riksdag | lansting | kommun,
	       locality :: string()}).

-record(block, {nonce :: pos_integer(),
		votes=[] :: list()}).

