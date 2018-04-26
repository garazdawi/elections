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
-export([start_link/0]).

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

vote(Personummer, Partiet, VoteType) ->
    Vote = #vote{personnummer = Personummer, partiet = Partiet,
		 vote_type = VoteType},
    gen_server:call(?SERVER, Vote).

count_votes() ->
    gen_server:call(?SERVER, count_votes, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, BlockchainHandle} = dets:open_file("/tmp/val_2018_blocks.dets"),
    {ok, BlockHeadersHandle} = dets:open_file("/tmp/val_2018_block_headers.dets"),
    {ok, #state{blockchain_handle = BlockchainHandle,
		blockheader_handle = BlockHeadersHandle}}.

handle_call(#vote{} = Vote, From, State = #state{blockchain_handle = BCHandle,
						 blockheader_handle = BHHandle}) ->
    LastSlot = get_last_slot(),
    <<Nonce:64, _Tail/binary>> = crypto:strong_rand_bytes(40),
    BlockHeader = #block_header{nonce = Nonce, votes = [Vote]},
    BlockHash = crypto:hash(md4, BlockHeader),
    Slot = LastSlot + 1,
    Block = #block{slot = Slot, block_hash = BlockHash},
    ok = dets:insert(BHHandle, {BlockHash, Block}),
    ok = dets:insert(BCHandle, {Slot, BlockHash}),
    {reply, ok, State};

handle_call(count_votes, _From, State = #state{blockchain_handle = BCHandle,
					       blockheader_handle = BHHandle}) ->
    
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
