-module(outpost_sup).

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
	outpost_userdata:read_and_save_to_ets(),
	outpost_util:register(register),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->

	%%%%%%
	%% Periodic Agents
	%%%%%%
	Heartbeat = {heartbeat_agent,
				 {outpost_periodic_server, start_link,[heartbeat_agent,[heartbeat,[]],60000]}, 
				 permanent, 2000, worker, dynamic},
				
	SnapAgent = {snap_agent,
			 	 {outpost_periodic_server, start_link,[snap_agent,[collect,[]],300000]}, 
			 	 permanent, 2000, worker, dynamic},
	
			Vhost = get_opts(vhost),
			Pass = get_opts(password),
			User = get_opts(username),

	ConnectInfo = {network,"my.skystack.com",5671,{User, Pass},list_to_binary(Vhost),
					[{cacertfile, get_opts(cacert)},
					 {certfile, get_opts(cert)},
					 {keyfile, get_opts(key)},
					 {verify, verify_peer},
					 {fail_if_no_peer_cert, true}]
				  },
				
	%%%%%%
	%% The Pub in PubSub (Publisher)
	%%%%%%	
	Hostname = get_opts(hostname),
	ReplyKey = "response.from."++Hostname,
	PubAgentInfo = [{declare,
						{exchange,
							{name,list_to_binary(get_opts(exchange))},{type,<<"topic">>}
						},
							{reply_queue, list_to_binary(get_opts(reply_q))},
							{reply_key, list_to_binary(ReplyKey)}
					}],

	PubAgentDeclareFun = fun(Ch, I)->
							{{Ex, Ty}, Qu, Rk} = I, 
							 {ok,{bunny_util:set_durable( bunny_util:new_exchange(Ex,Ty), true),
							 	  bunny_util:set_durable( bunny_util:new_queue(Qu), true)}
							}
                 		 end,

	PubAgentDeclareArgs = [{declare_fun, PubAgentDeclareFun},{no_ack, true}],
						
	PubAgent = {pub_agent,
				  {outpost_pub_server, start_link,[pub_agent, ConnectInfo, PubAgentInfo, PubAgentDeclareArgs ]}, permanent, 2000, worker, dynamic},
	
	%%%%%%
	%% The Sub in PubSub (Subscriber)
	%%%%%%
	SubAgentInfo = [{declare,
						{exchange,
							{name,list_to_binary(get_opts(exchange))},{type,<<"topic">>}
						},
							{queue, list_to_binary(get_opts(command_q))},
							{routing_key, list_to_binary("command")}
					}],
	
	SubAgentDeclareFun = fun(Ch, I)->
							{{Ex, Ty}, Qu, Rk} = I, 
							{ok,{bunny_util:set_durable( bunny_util:new_exchange(Ex,Ty), true),
								 bunny_util:set_durable( bunny_util:new_queue(Qu), true)}
							}
                 		 end,
					                                        
	SubAgentDeclareArgs = [{declare_fun, SubAgentDeclareFun},{no_ack, true}],
												
	SubAgent = {sub_agent,
				  {outpost_sub_server, start_link,[ConnectInfo, SubAgentInfo, SubAgentDeclareArgs]}, permanent, 2000, worker, dynamic},

	Children = [PubAgent, SubAgent, Heartbeat, SnapAgent],
	
		{ok, {{one_for_one, 5, 10}, Children}}.
		
get_opts(Key) ->
	[{_,Value}] = outpost_userdata:read(Key),
	Value.