%% @author Tass Skoudros <tass@skystack.com>
%% @copyright 23/10/2010 Skystack Limited.
%% Generic Gen_Server for implementing periodic agents.
-module(outpost_periodic_server).
-author('author <tass@skystack.com>').

-behaviour(gen_server).

%% Server API
-export([start_link/3, stop/1]).

%% Client API
-export([despatch/1,heartbeat/1,collect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER(Name), list_to_atom("outpost_" ++ atom_to_list(Name))).

-record(state, {name, mfa, interval, count}).

%%====================================================================
%% Server API
%%====================================================================

start_link(Name, ModFunArgs, Interval)
  when is_atom(Name), is_list(ModFunArgs), is_integer(Interval) ->
    Result = gen_server:start_link(
               {local, ?SERVER(Name)},
               ?MODULE, [Name, ModFunArgs, Interval], []),
    io:format("~p: started~n", [Name]),
    Result.

stop(Name) ->
    gen_server:cast(?SERVER(Name), shutdown).

%%====================================================================
%% Client API
%%====================================================================

despatch({Name,Mfa}) ->
	gen_server:cast(?SERVER(Name), {despatcher,{?MODULE,Mfa}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, ModFunArgs, Interval]) ->
    process_flag(trap_exit, true),
    timer:apply_interval(Interval, ?MODULE, despatch, [{Name,ModFunArgs}]),
    {ok, #state{name = Name,
                mfa = ModFunArgs,
                interval = Interval,
                count = 1}}.

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({despatcher, {Self, Mfa}}, State = #state{mfa = Mfa, count = Count}) ->
	[Fun, Args] = Mfa,
	%%erlang:apply(Self, Fun, Args),
	Self:Fun(Args),
    {noreply, State#state{count = Count + 1}}.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, #state{name = Name}) ->
    io:format("~p: shutdown cleanly (voluntary)~n", [Name]),
    ok;
terminate(shutdown, #state{name = Name}) ->
    io:format("~p: shutdown cleanly (forced)~n", [Name]),
    ok;
terminate(Reason, #state{name = Name}) ->
    io:format("~p: terminated because ~1024p~n", [Name, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%%====================================================================
%% Internal functions (Heartbeat)
%%====================================================================

async_publish(Message) ->
   	pub_agent ! {self(),{pub_agent, async_publish, Message} }.

heartbeat([])->
	Hostname = outpost_util:get_opt(hostname),
	Rk = list_to_binary("heartbeat.from." ++ Hostname),
	Payload = bert:encode(heartbeat_payload()),
	async_publish({Rk,Payload}).
	
heartbeat_payload() ->
	Hostname = outpost_util:get_opt(hostname),
	Alias = outpost_util:get_opt(alias),
	Avg1 = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.10.1.3.1 | awk \'{print $2}\''),
	TotalReal = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.5.0 | awk \'{print $2}\''),
	AvailReal = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.6.0 | awk \'{print $2}\''),
	Buffer = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.14.0 | awk \'{print $2}\''),
	Cached =  snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.15.0 | awk \'{print $2}\''),
	PercentReal = real_memory_as_percent(TotalReal,AvailReal,Buffer,Cached),
	Procs = capture(os:cmd('/bin/ps -ax | /usr/bin/wc -l')),
	{heartbeat,[{alias,Alias},
			  {hostname,Hostname},
			  {timestamp, outpost_util:get_unix_timestamp( now() )},
			  {processes_running,Procs},
			  {load_average,Avg1},
			  {memory,PercentReal}]}.
	
real_memory_as_percent(Tr,Ar,B,C) ->
	
	if 
		is_number(Tr), is_number(Ar), is_number(B), is_number(C) ->
			Ru = Tr - Ar,
			Rp = Ru - B - C / Tr *100;
		true ->
			"NaN"
	end.
	



%%====================================================================
%% Internal functions (SNMP)
%%====================================================================

collect([])->	
	User = outpost_util:get_opt(username),
	Alias = outpost_util:get_opt(alias),
	Hostname = outpost_util:get_opt(hostname),

	Data = {snapshot,[
				{username,User},
				{alias, Alias},
				{hostname, Hostname},
				{timestamp, outpost_util:get_unix_timestamp( now() )}, 
				{uptime, snap_uptime()},
				{load, snap_load()}, 
				{memory, snap_memory()}, 
				{cpu, snap_cpu()}
			]},
			Hostname = outpost_util:get_opt(hostname),
			Rk = list_to_binary("snapshot.from." ++ Hostname),
			Payload = bert:encode(Data),
			async_publish({Rk,Payload}).


snap_load () ->
	Avg1 = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.10.1.3.1 | awk \'{print $2}\''),
	Avg5 = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.10.1.3.2 | awk \'{print $2}\''),
	Avg15 = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.10.1.3.3 | awk \'{print $2}\''),

	{struct,[
			%% "UCD-SNMP-MIB::laLoad.1",".1.3.6.1.4.1.2021.10.1.3.1"
			{avg1, Avg1 },
			%% "UCD-SNMP-MIB::laLoad.2",".1.3.6.1.4.1.2021.10.1.3.2"
			{avg5, Avg5 },
			%% "UCD-SNMP-MIB::laLoad.3",".1.3.6.1.4.1.2021.10.1.3.2"
			{avg15, Avg15 }
	]}.

snap_uptime () ->
	snmp_query('snmpget -v 1 -c myskystack localhost .1.3.6.1.2.1.25.1.1.0').
		    %% "HOST-RESOURCES-MIB::hrSystemUptime.0",".1.3.6.1.2.1.25.1.1.0"

snap_memory() ->
	TotalSwap = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.3.0 | awk \'{print $2}\''),
	AvailSwap = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.4.0 | awk \'{print $2}\''),
	TotalReal = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.5.0 | awk \'{print $2}\''),
	AvailReal = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.6.0 | awk \'{print $2}\''),
	TotalFree = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.11.0 | awk \'{print $2}\''),
	MinimumSwap = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.12.0 | awk \'{print $2}\''),
	Shared = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.13.0 | awk \'{print $2}\''),
	Buffer = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.14.0 | awk \'{print $2}\''),
	Cached = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.4.15.0 | awk \'{print $2}\''),

	{struct,[	
		%% "UCD-SNMP-MIB::memTotalSwap.0",".1.3.6.1.4.1.2021.4.3.0"
		{memTotalSwap, TotalSwap },
		%% "UCD-SNMP-MIB::memAvailSwap.0",".1.3.6.1.4.1.2021.4.4.0"
		{memAvailSwap, AvailSwap },
		%% "UCD-SNMP-MIB::memTotalReal.0",".1.3.6.1.4.1.2021.4.5.0"
		{memTotalReal, TotalReal },
		%% "UCD-SNMP-MIB::memAvailReal.0",".1.3.6.1.4.1.2021.4.6.0"
		{memAvailReal, AvailReal },
		%% "UCD-SNMP-MIB::memTotalFree.0",".1.3.6.1.4.1.2021.4.11.0"
		{memTotalFree, TotalFree },
		%% "UCD-SNMP-MIB::memMinimumSwap.0",".1.3.6.1.4.1.2021.4.12.0"
		{memMinimumSwap, MinimumSwap },
		%% "UCD-SNMP-MIB::memShared.0",".1.3.6.1.4.1.2021.4.13.0"
		{memShared, Shared },
		%% "UCD-SNMP-MIB::memBuffer.0",".1.3.6.1.4.1.2021.4.14.0"
		{memBuffer, Buffer },
		%% "UCD-SNMP-MIB::memCached.0",".1.3.6.1.4.1.2021.4.15.0"
		{memCached, Cached }
	]}.

snap_cpu() ->
	CpuUser = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.9.0 | awk \'{print $2}\''),
	CpuSystem = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.10.0 | awk \'{print $2}\''),
	CpuIdle = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.11.0 | awk \'{print $2}\''),
	CpuRawUser = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.50.0 | awk \'{print $2}\''),
	CpuRawNice = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.51.0 | awk \'{print $2}\''),
	CpuRawSystem = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.52.0 | awk \'{print $2}\''),
	CpuRawIdle = snmp_query('snmpget -O sqU -v 1 -c myskystack localhost .1.3.6.1.4.1.2021.11.53.0 | awk \'{print $2}\''),

	{struct,[
		%% "UCD-SNMP-MIB::ssCpuUser.0",".1.3.6.1.4.1.2021.11.9.0"
		{ssCpuUser, CpuUser},
		%% "UCD-SNMP-MIB::ssCpuSystem.0",".1.3.6.1.4.1.2021.11.10.0"
		{ssCpuSystem, CpuSystem},			
		%% "UCD-SNMP-MIB::ssCpuIdle.0",".1.3.6.1.4.1.2021.11.11.0"
		{ssCpuIdle, CpuIdle},					
		%% "UCD-SNMP-MIB::ssCpuRawUser.0",".1.3.6.1.4.1.2021.11.50.0"
		{ssCpuRawUser, CpuRawUser},			
		%% "UCD-SNMP-MIB::ssCpuRawNice.0",".1.3.6.1.4.1.2021.11.51.0"
		{ssCpuRawNice, CpuRawNice},			
		%% "UCD-SNMP-MIB::ssCpuRawSystem.0",".1.3.6.1.4.1.2021.11.52.0"
		{ssCpuRawSystem, CpuRawSystem},			
		%% "UCD-SNMP-MIB::ssCpuRawIdle.0",".1.3.6.1.4.1.2021.11.53.0"
		{ssCpuRawIdle, CpuRawIdle}
	]}.
	
	
	
	
	
snmp_query(Cmd) ->
	Result = capture( os:cmd( Cmd ) ).

capture(Result) ->
		NoSuchName = string:str(Result, "noSuchName"),
		Error = string:str(Result, "Error in packet"),
		FailedObject = string:str(Result,"Failed object"),
	    if
	       NoSuchName > 1, Error >= 1, FailedObject > 1 ->
	           1;  
	       true ->			
	     		R = string:strip(Result, both, $\n),	
				string:strip(R, both)	
	    end.
			
				