%% Copyright 2009, Joe Williams <joe@joetify.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @author Joseph Williams <joe@joetify.com>
%% @copyright 2009 Joseph Williams
%% @version pre 0.1
%% @seealso http://www.last.fm/api
%% @doc Commonly used functions for fermal
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_util).

-export([get_body/1]).

%% @doc retreives info from last.fm
get_body(Url) ->
	{ ok, { _Status, _Headers, Body }} = http:request(Url),
	check_status(_Status),
	rfc4627:decode(Body).

%% @private
%% @doc checks http status code
check_status(_Status) ->
	case _Status of
		{_Http,200,"OK"} ->
			ok;
		_ ->
			io:format("Error! Bad Status Code. ~p ~n", [_Status]),
			exit(not_200_status_code)
	end.
