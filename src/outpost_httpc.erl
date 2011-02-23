%% @author Tass Skoudros <tass@skystack.com>
%% @copyright 23/10/2010 Skystack Limited.

%% @doc Web server for skystack_client.

-module(outpost_httpc).
-author('author <tass@skystack.com>').

-export([put/3,get/1]).

%%-define(SKYSTACK_HTTP,"my.skystack.com").
%%-define(SKYSTACK_HTTPS,"my.skystack.com").
-define(SKYSTACK_HTTP, "my-skystack").
-define(SKYSTACK_HTTPS, "my-skystack").


put(Endpoint,Body,ContentType) -> simple_put(Endpoint,Body,ContentType).

get(Endpoint) -> simple_get(Endpoint).

simple_put(Endpoint,Body,ContentType) ->
    URL = url(Endpoint),
	%%	inets:start(),
    {ok, Response} = httpc:request(put, {URL ,[{"user-agent", "Skystack Outpost/1.0"}] , [ContentType] , Body}, [], []),
   %% io:format("Response url: ~p~n", [URL]),
    {StatusCode, ReasonPhrase} = status(Response),
	case StatusCode of 
		204 -> ok;
		403 -> forbidden;
		_ -> error
	end.

simple_post(Endpoint,Body) ->
	    URL = url(Endpoint),
		%%	inets:start(),
	    {ok, Response} = httpc:request(post, {URL ,[{"user-agent", "Skystack Outpost/1.0"}] , [] , Body}, [], []),
	   %% io:format("Response url: ~p~n", [URL]),
	    {StatusCode, ReasonPhrase} = status(Response),
		case StatusCode of 
			204 -> ok;
			403 -> forbidden;
			_ -> error
		end.
	
simple_get(Endpoint) ->
	URL = url(Endpoint),
    {ok, Response} = httpc:request(get, {URL ,[{"user-agent", "Skystack Outpost/1.0"}]}, [], []),
	Body = body(Response),

    {StatusCode, ReasonPhrase} = status(Response),
	case StatusCode of 
		200 -> {ok,Body};
		204 -> {ok,accepted};
		403 -> {error,forbidden};
		_ -> {error,unknown}
	end.
	
	
status(Response) ->
 		{{_Version, StatusCode, ReasonPhrase}, _Headers, ResultBody} = Response,
	    {StatusCode, ReasonPhrase}.

body(Response) ->
	{{_Version, StatusCode, ReasonPhrase}, _Headers, ResultBody} = Response,
	    ResultBody.

ssl_url(Path) ->
	[{_,Username}] = outpost_metadata:read(username),
	[{_,Token}] = outpost_metadata:read(token),"https://"++Username++":"++Token++"@" ++ ?SKYSTACK_HTTPS ++ Path.
	
url(Path) ->
	[{_,Username}] = outpost_metadata:read(username),
	[{_,Token}] = outpost_metadata:read(token),"http://"++Username++":"++Token++"@" ++ ?SKYSTACK_HTTP ++ Path.
