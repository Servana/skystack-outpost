-module(outpost_pub_server).
-author('Tass Skoudros <tass@skystack.com>').
-behavior(gen_server).

-include("gen_bunny.hrl").

-export([start_link/4, start_link/5, stop/1]).
-export([publish/1,
		 publish/2,
         publish/3,
         async_publish/1,
         async_publish/2,
         async_publish/3,
         get/2,
         ack/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%
%% API
%%
publish(Message) ->
    publish(self(), Message, []).

publish(Name, Message) ->
    publish(Name, Message, []).

publish(Name, Message, Opts) ->
    gen_server:call(Name, {publish, Message, Opts}).

async_publish(Message) ->
    async_publish(self(), Message, []).
	
async_publish(Name, Message) ->
    async_publish(Name, Message, []).

async_publish(Name, Message, Opts) ->
    gen_server:cast(Name, {publish, Message, Opts}).

get(Name, NoAck) ->
    gen_server:call(Name, {get, NoAck}).

ack(Name, Tag) ->
    gen_server:cast(Name, {ack, Tag}).
		
start_link(Name, ConnectInfo, DeclareInfo, Args) ->
	DeclareInfo1 = get_info(DeclareInfo),
	start_link(Name, ConnectInfo, DeclareInfo1, Args, []).

start_link(Name, ConnectInfo, DeclareInfo, Args, []) ->
	Result = gen_server:start_link({local,Name}, ?MODULE,[ConnectInfo, DeclareInfo, Args], []),
	io:format("~p: started~n", [?MODULE]),
    Result.

stop(Name) ->
    gen_server:call(Name, stop).


%%
%% Callbacks
%%

%% @private
init([ConnectionInfo, DeclareInfo, Args]) ->
    ConnectFun = proplists:get_value(connect_fun, Args,
                                     fun gen_bunny_mon:connect/1),
    DeclareFun = proplists:get_value(declare_fun, Args,
                                     fun bunny_util:declare/2),

    {ok, {ConnectionPid, ChannelPid}} = ConnectFun(ConnectionInfo),

  	{ok, {Exchange,Queue}} = DeclareFun(ChannelPid, DeclareInfo),
	put(declare_info,DeclareInfo),
    {ok, #bunnyc_state{connection=ConnectionPid,
                channel=ChannelPid,
                exchange=Exchange}}.

%% @private
handle_call({publish, Key, Message, Opts}, _From,
            State = #bunnyc_state{channel=Channel, exchange=Exchange})
  when is_binary(Key), is_binary(Message) orelse ?is_message(Message),
       is_list(Opts) ->
    Resp = internal_publish(fun amqp_channel:call/3,
                            Channel, Exchange, Key, Message, Opts),
    {reply, Resp, State};

handle_call({get, NoAck}, _From,
            State = #bunnyc_state{channel=Channel, queue=Queue}) ->
    Resp = internal_get(Channel, Queue, NoAck),
    {reply, Resp, State};

handle_call(stop, _From,
            State = #bunnyc_state{channel=Channel, connection=Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {stop, normal, ok, State}.

%% @private
handle_cast({publish, Message, Opts},
            State = #bunnyc_state{channel=Channel, exchange=Exchange})
  when is_binary(Message) orelse ?is_message(Message),
       is_list(Opts) ->
	
	   Hostname = get_opts(hostname),
	   DefaultKey = "response.from."++Hostname,
    
	internal_publish(fun amqp_channel:cast/3,
                     Channel, Exchange, DefaultKey, Message, Opts),
    {noreply, State};

handle_cast({publish, {Key,Message}, Opts},
            State = #bunnyc_state{channel=Channel, exchange=Exchange})
  when is_binary(Message) orelse ?is_message(Message),
       is_list(Opts) ->

	   DefaultKey = Key,

	internal_publish(fun amqp_channel:cast/3,
                     Channel, Exchange, DefaultKey, Message, Opts),
    {noreply, State};
handle_cast({ack, Tag}, State = #bunnyc_state{channel=Channel}) ->
    internal_ack(Channel, Tag),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({reconnected, {ConnectionPid, ChannelPid}}, State) ->
    {noreply, State#bunnyc_state{connection=ConnectionPid, channel=ChannelPid}};

handle_info({From, {Name, Method, Message}}, State) ->
	gen_server:cast(Name, {publish, Message, []}),
	    {noreply, State};
	
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%
%% Internal
%%
internal_publish(Fun, Channel, Exchange, Key, Message, Opts)
  when ?is_message(Message) ->
    Mandatory = proplists:get_value(mandatory, Opts, false),

    BasicPublish = #'basic.publish'{
      exchange = bunny_util:get_name(Exchange),
      routing_key = Key,
      mandatory = Mandatory},

    Fun(Channel, BasicPublish, Message);
internal_publish(Fun, Channel, Exchange, Key, Message, Opts)
  when is_binary(Message) ->
    internal_publish(Fun, Channel, Exchange, Key,
                     bunny_util:new_message(Message), Opts).


internal_get(Channel, Queue, NoAck) ->
    amqp_channel:call(Channel, #'basic.get'{queue=bunny_util:get_name(Queue),
                                            no_ack=NoAck}).


internal_ack(Channel, DeliveryTag) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag=DeliveryTag}).

get_opts(Key) ->
	[{_,Value}] = outpost_userdata:read(Key),
	Value.
					
get_info(Info)->
	[{declare,{_,{_,Exchange},{_,Type}},{_,Queue},{_,RoutingKey}}] = Info,
	Declare = {{Exchange,Type}, Queue, RoutingKey},
	Declare.