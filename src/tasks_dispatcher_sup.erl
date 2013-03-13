-module(tasks_dispatcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, Queues} = application:get_env(queues),
	Children = [ child_spec(Queue, Listeners, CallbackUrl) || {Queue, Listeners, CallbackUrl} <- Queues ],
    {ok, { {one_for_one, 5, 10}, Children} }.
    
child_spec(Queue, Listeners, CallbackUrl) ->
	{{queue_sup, Queue}, 
		{queue_sup, start_link, [Queue, Listeners, CallbackUrl]},
		permanent,
		5000,
		supervisor,
		[]
	}.
