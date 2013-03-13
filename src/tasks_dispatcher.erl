-module(tasks_dispatcher).
%% API.
-export([start/0]).

%% API.

start() ->
    ok = application:start(log4erl),
    log4erl:conf("priv/l4e.conf"),
    ok = inets:start(),
	ok = application:start(tasks_dispatcher).
