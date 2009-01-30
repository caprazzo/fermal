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
%% @doc Module for the tasteometer portion of the Last.fm API
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_tasteometer).

-export([get_tasteometer/1]).

%% @doc retreives and parses an tasteometer request
get_tasteometer(Url) ->
	JsonBody = fermal_util:get_body(Url),
	{ok,{obj,[{"comparison",
           {obj,[{"result",
                  {obj,[{"score", Score},
                        {"artists",
                         {obj,[{"artist",
                                [{obj,[{"name", SharedArtistName1},{"url", SharedArtistUrl1},_SharedArtistImages1]},
                                 {obj,[{"name", SharedArtistName2},{"url", SharedArtistUrl2},_SharedArtistImages2]},
                                 {obj,[{"name", SharedArtistName3},{"url", SharedArtistUrl3},_SharedArtistImages3]},
                                 {obj,[{"name", SharedArtistName4},{"url", SharedArtistUrl4},_SharedArtistImages4]},
                                 {obj,[{"name", SharedArtistName5},{"url", SharedArtistUrl5},_SharedArtistImages5]}]},
                               {"matches", MatchCount}]}}]}},
                 {"input",
                  {obj,[{"user",
                         [{obj,[{"name", User1},
                                {"url", UserUrl1},
                                _UserImages1]},
                          {obj,[{"name", User2},
                                {"url", UserUrl2},
                                _UserImages2]}]}]}}]}}]},
    []} = JsonBody,
    [ [ result, {score, binary_to_list(Score)} ],
    	[ user, {name, binary_to_list(User1)}, {url, binary_to_list(UserUrl1)},
    	{name, binary_to_list(User2)}, {url, binary_to_list(UserUrl2)} ],
    		[ artists, [{name, binary_to_list(SharedArtistName1)},{url, binary_to_list(SharedArtistUrl1)}],
    		[{name, binary_to_list(SharedArtistName2)},{url, binary_to_list(SharedArtistUrl2)}],
    		[{name, binary_to_list(SharedArtistName3)},{url, binary_to_list(SharedArtistUrl3)}],
    		[{name, binary_to_list(SharedArtistName4)},{url, binary_to_list(SharedArtistUrl4)}],
    		[{name, binary_to_list(SharedArtistName5)},{url, binary_to_list(SharedArtistUrl5)}],
    		{matches, binary_to_list(MatchCount)} ] ].
