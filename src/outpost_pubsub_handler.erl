%% @author Tass Skoudros <tass@skystack.com>
%% @copyright 23/10/2010 Skystack Limited.
%% Generic Gen_Server for implementing agents.
-module(outpost_pubsub_handler).
-author('author <tass@skystack.com>').
-export([handle_command/1, get_params/1]).


process_command("snmp", Params, From, State) ->  
 	io:format("process_command: ~p", [string:concat("snmpget -O sqU -v 1 -c myskystack localhost ", Params)]),
	Result = prepare(os:cmd(string:concat("snmpget -O sqU -v 1 -c myskystack localhost ", Params))),
	{ok, Result};
process_command(_, _, From, State) -> 
    Result = "Invalid command or format.",
    {ok, prepare(Result)}.

handle_command(Msg) ->
	io:format("handle_command: ~p", [Msg]),
    case re:run(Msg, "^/([A-Za-z0-9]+) ?(.+){0,1}", [{capture, all_but_first, list}]) of
        {match, [Cmd]} ->process_command(Cmd, [], [], []);
        {match, [Cmd, ParamStr]} -> process_command(Cmd, ParamStr, [], []);
        _ -> {error, no_command}
    end.

get_params(X) when is_list(X) ->
    Opts = [{capture, [param,rest], list}],
    case {re:run(X, "^\\\"(?<param>[^\\\"\\\\]*(\\\\.[^\\\"\\\\]*)*)\\\"[ ]*(?<rest>.*)$", Opts), 
	      re:run(X, "^[ ]*(?<param>[A-Za-z0-9]+)[ ]*(?<rest>.*)$", Opts)} of
        {_, {match, [Param, Rest]}} -> [Param | get_params(Rest)];
        {{match, [Param, Rest]}, _} -> [Param | get_params(Rest)];
        _ -> []
    end;
get_params(_) -> [].
		
prepare(Term) ->

	NoSuchName = string:str(Term, "noSuchName"),
	Error = string:str(Term, "Error in packet"),
	FailedObject = string:str(Term,"Failed object"),
    if
       NoSuchName > 1, Error >= 1, FailedObject > 1 ->
           Result = "NaN";  
       true ->
     	   Result = string:strip(Term, both, $\n)	
    end,
	erlang:list_to_binary(Result).
	
