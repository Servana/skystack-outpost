%% @author Tass Skoudros <tass@skystack.com>
%% @copyright 23/10/2010 Skystack Limited.

%% @doc Reads configuration file and writes it to a ETS table.

-module(outpost_userdata).
-author('author <tass@skystack.com>').

-export([read_and_save_to_ets/0,read/1,write/1]).

-define(ETS,sccstore).

read_and_save_to_ets() ->
	{ok,Terms} = file:consult("conf/outpost_userdata.conf"),

	[{_,Us},{_,To},{_,Pa},{_,Al},{_,Hn},{_,Ca},{_,Ce},{_,Ke},{_,Vh},{_,Ex},{_,Re},{_,Co}] = Terms,
	
   ets_new({registered,false}),
   ets_set({hostname,Hn}),
   ets_set({username,Us}),
   ets_set({password,Pa}),
   ets_set({alias,Al}),
   ets_set({token,To}),
   ets_set({cacert,Ca}),
   ets_set({cert,Ce}),
   ets_set({key,Ke}),
   ets_set({vhost,Vh}),
   ets_set({exchange,Ex}),
   ets_set({reply_q,Re}),
   ets_set({command_q,Co}).

		
	
read(K) -> ets_get(K).
write(D) -> ets_set(D).

ets_new(D) ->
	ets:new(?ETS,[set,public,named_table,{keypos,1}]),
	ets:insert(?ETS,D).

ets_set(D) -> ets:insert(?ETS,D).
ets_get(K) -> ets:lookup(?ETS,K).
%%ets_delete(T) -> ets:delete(T).
