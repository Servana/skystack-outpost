-module(outpost_sub_server).
-behavior(gen_bunny).

-export([start_link/3,
         stop/1]).

-export([init/1,
         handle_message/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).


-export([ack_messages/2, get_messages/1, get_calls/1, get_casts/1, get_infos/1,
		async_publish/2, async_publish/3]).

-include("gen_bunny.hrl").

-record(state, {messages=[], calls=[], infos=[], casts=[], channel=[]}).
	
%%gen_bunny / bunny_util connect({network, Host, Port, {User, Pass}, VHost}) line 214

start_link(ConnectInfo, Info, Opts) ->
	DeclareInfo = get_info(Info),
	start_link(ConnectInfo, DeclareInfo, Opts, []).
    
start_link(ConnectInfo, DeclareInfo, Opts, Args) ->
	Result = gen_bunny:start_link(?MODULE, ConnectInfo, DeclareInfo, Opts),
	io:format("~p: started~n", [?MODULE]),
    Result.

init(Args) ->
    {ok, #state{}}.

%%
%% API
%%
async_publish( To, Message) ->
    async_publish( To, Message, []).

async_publish( To, Message, Opts) ->
    handle_cast({publish, To, Message, Opts},[]).
	
ack_messages(Pid, Tag) ->
    gen_bunny:cast(Pid, {ack_stuff, Tag}).

get_messages(Pid) ->
    gen_bunny:call(Pid, get_messages).

get_channel(Pid) ->
    gen_bunny:call(Pid, get_channel).
	
get_calls(Pid) ->
    gen_bunny:call(Pid, get_calls).

get_casts(Pid) ->
    gen_bunny:call(Pid, get_casts).

get_infos(Pid) ->
    gen_bunny:call(Pid, get_infos).

stop(Pid) ->
    gen_bunny:call(Pid, stop).

handle_message(Message, State=#state{messages=Messages})
  when ?is_message(Message) orelse ?is_tagged_message(Message) ->
	
    	NewMessages = [ bunny_util:get_payload(Message)|Messages ],

		{ok,Result} = outpost_pubsub_handler:handle_command(NewMessages),
		
		async_publish(pub_agent, Result),
    {noreply, State#state{messages=NewMessages}}.

%%
%% handle call
%%
handle_call(get_messages, _From, State=#state{messages=Messages}) ->
    {reply, Messages, State};
handle_call(get_calls, _From, State=#state{calls=Calls}) ->
    {reply, Calls, State};
handle_call(get_casts, _From, State=#state{casts=Casts}) ->
    {reply, Casts, State};
handle_call(get_infos, _From, State=#state{infos=Infos}) ->
    {reply, Infos, State};
handle_call(crash, _From, _State=#state{}) ->
    ok = crashed;
handle_call(stop, _From, State=#state{}) ->
    {stop, normal, ok, State};
handle_call(Msg, _From, State=#state{calls=Calls}) ->
    {reply, ok, State#state{calls=[Msg|Calls]}}.

%%
%% handle casts
%%
	
handle_cast({publish, To, Message, Opts}, State)
  when is_binary(Message) orelse ?is_message(Message), is_list(Opts) ->
	gen_server:cast(To, {publish, Message, []}),
    {noreply, State};
handle_cast({ack, Tag}, State) ->
    gen_bunny:ack(Tag),
    {noreply, State};

handle_cast(Msg, State=#state{casts=Casts}) ->
    {noreply, State#state{casts=[Msg|Casts]}}.

handle_info(Info, State=#state{infos=Infos}) ->
    {noreply, State#state{infos=[Info|Infos]}}.

terminate(Reason, _State) ->
    io:format("~p terminating with reason ~p~n", [?MODULE, Reason]),
    ok.

get_opts(Key) ->
	[{_,Value}] = outpost_userdata:read(Key),
	Value.

get_info(Info)->
	[{declare,{_,{_,Exchange},{_,Type}},{_,Queue},{_,RoutingKey}}] = Info,
	Declare = {{Exchange,Type},Queue,RoutingKey},
	Declare.