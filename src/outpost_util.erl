-module(outpost_util).
-export([get_unix_timestamp/1, get_opt/1,register/1]).

get_unix_timestamp(TS) ->
	calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) - 
	calendar:datetime_to_gregorian_seconds( { {1970,1,1},{0,0,0} } ).
	
get_opt(Key) ->
	[{_,Value}] = outpost_userdata:read(Key),
	Value.	
	
	
%%====================================================================
%% Internal functions (Register)
%%====================================================================
register(Type) ->
	Alias = outpost_util:get_opt(alias),
	Hostname = get_opt(hostname),
	Username = get_opt(username),
	case Type of
		register -> DoRegister = true;			
		unregister -> DoRegister = false		
	end,

	Data = {register,[{alias,Alias},{hostname,Hostname},{registered,DoRegister}]},
	do_register(Type, Username, Alias).

do_register(Type, Username, Alias) ->
	
	Hn = lists:concat(["/",Alias,"/a/servers/",Username,".json","?action=",Type]),
	
	Response = [],
%%	Response = outpost_httpc:get(Hn),

		case Response  of
			{error,Reason} -> 
				outpost_metadata:write({registered,false}); %% do some kind of escalation here
			Result -> outpost_metadata:write({registered,true})
		end.