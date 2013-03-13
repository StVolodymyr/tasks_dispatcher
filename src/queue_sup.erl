-module(queue_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Queue, Listeners, CallbackUrl) ->
	supervisor:start_link(?MODULE, [Queue, Listeners, CallbackUrl]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Queue, Listeners, CallbackUrl]) ->
    Children = [ child_spec(Queue, CallbackUrl, Num) || Num <- lists:seq(1, Listeners) ],
    {ok, { {one_for_one, 5, 10}, Children } }.
    
child_spec(Queue, CallbackUrl, Num) ->
	{{listener, Num}, 
		{queue_listener, start_link, [Queue, CallbackUrl, Num]},
		permanent,
		5000,
		worker,
		[]
	}.

