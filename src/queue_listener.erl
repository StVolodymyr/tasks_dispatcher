-module(queue_listener).

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    connection,
    channel,
    callback_url
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the one call server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Queue, CallbackUrl, Num) ->
    gen_server:start_link(?MODULE, [Queue, CallbackUrl, Num], []).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Queue, CallbackUrl, Num])->
    put(queue, Queue),
    put(callback_url, CallbackUrl),
    put(item, Num),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    log4erl:debug(trace, "Connection ~p~n", [Connection]),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    log4erl:debug(trace, "Channel ~p~n", [Channel]),
    
    Declare = #'queue.declare'{queue = Queue, durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    
    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{} = amqp_channel:call(Channel, Sub),
    {ok, #state{connection = Connection, channel = Channel, callback_url = CallbackUrl}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(#'basic.consume_ok'{}, #state{channel = Channel} = State) ->
    log4erl:debug(trace, "Start consume ~p~n", [Channel]),
    {noreply, State};
    
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #state{channel = Channel, callback_url = CallbackUrl} = State) ->
    log4erl:debug(trace, "Receive message ~p. Channel ~p~n", [Payload, Channel]),
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} = 
        httpc:request(post, {CallbackUrl, [], "application/x-www-form-urlencoded", Payload}, [], []),
    log4erl:debug(trace, "Server responce ~p~n", [Body]),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
    
handle_info(Info, State) ->
    {stop, {unknown_message, Info}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
