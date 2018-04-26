%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2018 10:13
%%%-------------------------------------------------------------------
-module(vote_chain).

-behaviour(gen_server).
 -include_lib("elections/include/elections.hrl").

%% API
-export([start_link/0, vote/3, vote/4, count_votes/1, count_votes/2, clear_tables/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {blockchain_handle, blockheader_handle}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

vote(Personnummer, Partiet, riksdag) ->
    vote(Personnummer, Partiet, riksdag, "sverige").

vote(Personummer, Partiet, VoteType, Locality) ->
    Vote = #vote{personnummer = Personummer, partiet = Partiet,
		 vote_type = VoteType, locality = Locality},
    gen_server:call(?SERVER, Vote).

count_votes(riksdag) ->
    count_votes(riksdag, "sverige").

count_votes(VoteType, Locality) ->
    gen_server:call(?SERVER, {count_votes, VoteType, Locality}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, BlockchainHandle} = dets:open_file(blocks, [{file, "/tmp/val_2018_blocks.dets"}]),
    {ok, BlockHeadersHandle} = dets:open_file(blockheaders, [{file, "/tmp/val_2018_block_headers.dets"}]),
    {ok, #state{blockchain_handle = BlockchainHandle,
		blockheader_handle = BlockHeadersHandle}}.

handle_call(#vote{} = Vote, _From, State = #state{blockchain_handle = BCHandle,
						 blockheader_handle = BHHandle}) ->
    case had_already_voted(Vote, BHHandle, BCHandle) of
	false ->
	    LastSlot = get_last_slot(BCHandle),
	    <<Nonce:64, _Tail/binary>> = crypto:strong_rand_bytes(40),
	    Block = #block{nonce = Nonce, votes = [Vote]},
	    BlockHash = crypto:hash(md4, erlang:term_to_binary(Block)),
	    Slot = LastSlot + 1,
	    ok = dets:insert(BHHandle, {BlockHash, Block}),
	    ok = dets:insert(BCHandle, {Slot, BlockHash}),
	    {reply, ok, State};
	true ->
	    {reply, error, State}
    end;

handle_call({count_votes, VoteType, Locality}, _From,
	    	State = #state{blockchain_handle = BCHandle,
			       blockheader_handle = BHHandle}) ->
    TotalVotes = count_votes(VoteType, Locality, BHHandle, BCHandle),
    {reply, TotalVotes, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_last_slot(BCHandle) ->
    dets:info(BCHandle, size).

count_votes(RequiredVoteType, RequiredLocality, BHHandle, BCHHandle) ->
    ct:pal("rq_vote_type: ~p", [RequiredVoteType]),
    ct:pal("req_locality: ~p", [RequiredLocality]),
    dets:foldl(
	fun({_Slot, BlockHash}, Acc)->
	    [{BlockHash, #block{votes = [Vote]}}] = dets:lookup(BHHandle, BlockHash),
	    #vote{vote_type = VoteType, partiet = Partiet, locality = Locality} = Vote,
	    ct:pal("vote: ~p", [Vote]),
	    case {VoteType, Locality} of
		{RequiredVoteType, RequiredLocality} ->
		    CurentNumOfVotes = maps:get(Partiet, Acc, 0),
		    maps:put(Partiet, CurentNumOfVotes + 1, Acc);
		_ -> Acc
	    end
	end,
	maps:new(), BCHHandle).


had_already_voted(#vote{personnummer = Personnummer, vote_type = VoteType,
			locality = Locality}, BHHandle, BCHHandle) ->
    dets:foldl(
	fun({_Slot, BlockHash}, Acc)->
	    [{BlockHash, #block{votes = [Vote]}}] = dets:lookup(BHHandle, BlockHash),
	    case Vote of
		#vote{personnummer = Personnummer,
		      vote_type = VoteType} when Acc == false -> true;
		_ -> Acc
	    end
	end,
	false, BCHHandle).


clear_tables() ->
    ok = dets:delete_all_objects(blocks),
    ok = dets:delete_all_objects(blockheaders).
