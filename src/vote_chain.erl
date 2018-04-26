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
-export([start_link/0, vote/3, vote/4, count_votes/1, count_votes/2, clear_tables/0,
         contract/3]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {blockchain_handle, blockheader_handle, public_key, private_key}).

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

-spec contract(term(), fun(#vote{}, term()) -> term(), term()) -> ok.
contract(Id, Fun, Init) ->
    gen_server:call(?SERVER, Id, Fun, Init).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

create_block(PreviousBlockHash, Votes, PrivateKey, PublicKey) ->
    <<Nonce:64, _Tail/binary>> = crypto:strong_rand_bytes(40),
    Block = #block{previous_blockhash = PreviousBlockHash,
		   nonce = Nonce,
		   votes = Votes,
		   public_key = PublicKey},
    BlockHash = crypto:hash(md4, erlang:term_to_binary(Block)),
    Signature = crypto:sign(ecdsa, sha256, BlockHash, [PrivateKey, sect571r1]),
    NewBlock = Block#block{signature = Signature},
    {BlockHash, NewBlock}.

write_block(Slot, BCHandle, BHHandle, Block, BlockHash) ->
    ok = dets:insert(BHHandle, {BlockHash, Block}),
    ok = dets:insert(BCHandle, {Slot, BlockHash}).


init([]) ->
    {ok, BlockchainHandle} = dets:open_file(blocks, [{file, "/tmp/val_2018_blocks.dets"}]),
    {ok, BlockHeadersHandle} = dets:open_file(blockheaders, [{file, "/tmp/val_2018_block_headers.dets"}]),
    {PublicKey, PrivateKey} = crypto:generate_key(ecdh, sect571r1),
    {BlockHash, Block} = create_block(<<>>, [], PrivateKey, PublicKey),
    write_block(1, BlockchainHandle, BlockHeadersHandle, Block, BlockHash),

    {ok, #state{blockchain_handle = BlockchainHandle,
		blockheader_handle = BlockHeadersHandle,
		public_key = PublicKey,
		private_key = PrivateKey}}.

handle_call(#vote{} = Vote, _From, State = #state{blockchain_handle = BCHandle,
						  blockheader_handle = BHHandle,
						  private_key = PrivateKey,
						  public_key = PublicKey}) ->
    case had_already_voted(Vote, BHHandle, BCHandle) of
	false ->
	    {LastSlot, LastBlockHash} = get_last_slot_and_blockhash(BCHandle),
	    {BlockHash, Block} = create_block(LastBlockHash, [Vote], PrivateKey, PublicKey),
	    write_block(LastSlot+1, BCHandle, BHHandle, Block, BlockHash),
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

get_last_slot_and_blockhash(BCHandle) ->
    LastSlot = dets:info(BCHandle, size),
    ct:pal("last slot: ~p", [LastSlot]),
    ct:pal("R: ~p", [dets:lookup(BCHandle, LastSlot)]),
    [{LastSlot, LastBlockHash}] = dets:lookup(BCHandle, LastSlot),
    {LastSlot, LastBlockHash}.

count_votes(RequiredVoteType, RequiredLocality, BHHandle, BCHHandle) ->
    ct:pal("rq_vote_type: ~p", [RequiredVoteType]),
    ct:pal("req_locality: ~p", [RequiredLocality]),
    dets:foldl(
	fun({_Slot, BlockHash}, Acc) ->
	    [{BlockHash, #block{votes = Votes}}] = dets:lookup(BHHandle, BlockHash),
	    lists:foldl(fun(#vote{vote_type = VoteType,
				  partiet   = Partiet,
				  locality  = Locality}, Acc1) ->
		case {VoteType, Locality} of
		    {RequiredVoteType, RequiredLocality} ->
			CurentNumOfVotes = maps:get(Partiet, Acc1, 0),
			maps:put(Partiet, CurentNumOfVotes + 1, Acc1);
		    _ -> Acc1
		end
			end, Acc, Votes)
	end,
	maps:new(), BCHHandle).


had_already_voted(#vote{personnummer = Personnummer, vote_type = VoteType}, BHHandle, BCHHandle) ->
    dets:foldl(
	fun({_Slot, BlockHash}, Acc)->
	    [{BlockHash, #block{votes = Votes}}] = dets:lookup(BHHandle, BlockHash),
	    Acc orelse
	    lists:any(fun(Vote)->
		case Vote of
		    #vote{personnummer = Personnummer,
			  vote_type = VoteType} -> true;
		    _ -> false
		end
		      end, Votes)
	end,
	false, BCHHandle).


clear_tables() ->
    {ok, _BlockchainHandle} = dets:open_file(blocks, [{file, "/tmp/val_2018_blocks.dets"}]),
    {ok, _BlockHeadersHandle} = dets:open_file(blockheaders, [{file, "/tmp/val_2018_block_headers.dets"}]),
    ok = dets:delete_all_objects(blocks),
    ok = dets:delete_all_objects(blockheaders),
    dets:close(blocks),
    dets:close(blockheaders).
