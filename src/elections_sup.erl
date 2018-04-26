%%%-------------------------------------------------------------------
%% @doc elections top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elections_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpec = #{id => "",
                start => {vote_chain, start_link, []}},
    {ok, { {one_for_one, 1, 5}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
