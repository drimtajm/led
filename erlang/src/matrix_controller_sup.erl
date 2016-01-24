%%%-------------------------------------------------------------------
%%% @author  drimtajm
%%% @copyright (C) 2016, 
%%% @doc     Supervisor for matrix controller application
%%%
%%% @end
%%% Created : 23 Jan 2016 by drimtajm
%%%-------------------------------------------------------------------
-module(matrix_controller_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 20000,
    Type = worker,

    CodeReceiver = {code_receiver, {code_receiver, start_link, []},
		    Restart, Shutdown, Type, [code_receiver]},
    MatrixDisplay = {matrix_display, {matrix_display, start_link, []},
		     Restart, Shutdown, Type, [matrix_display]},

    {ok, {SupFlags, [MatrixDisplay, CodeReceiver]}}.

stop() ->
    matrix_display:stop(),
    code_receiver:stop(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
