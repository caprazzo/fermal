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
%% @doc Module for the artist portion of the Last.fm API
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_artist).

-export([get_artist_info/1]).

%% @doc retreives and parses an artist info request
get_artist_info(Url) ->
	JsonBody = fermal_util:get_body(Url),
	{ok,{obj,[{"artist",
           {obj,[{"name", ArtistName},
                 {"mbid", ArtistMbid},
                 {"url", ArtistUrl},
                 _ArtistImages,
                 {"streamable", Streamable},
                 {"stats",
                  {obj,[{"listeners", ArtistListeners},
                        {"playcount", ArtistPlaycount}]}},
                 {"similar",
                  {obj,[{"artist",
                         [{obj,[{"name", SimilarArtistName1},{"url", SimilarArtistUrl1},_SimilarArtistImages1]},
                          {obj,[{"name", SimilarArtistName2},{"url", SimilarArtistUrl2},_SimilarArtistImages2]},
                          {obj,[{"name", SimilarArtistName3},{"url", SimilarArtistUrl3},_SimilarArtistImages3]},
                          {obj,[{"name", SimilarArtistName4},{"url", SimilarArtistUrl4},_SimilarArtistImages4]},
                          {obj,[{"name", SimilarArtistName5},{"url", SimilarArtistUrl5},_SimilarArtistImages5]}]}]}},
                 {"bio",
                  {obj,[{"published", ArtistBioPubDate},
                        {"summary", ArtistBioSummary},
                        {"content", ArtistBioContent}]}}]}}]},
    []} = JsonBody,
    [ [ artist, {name, binary_to_list(ArtistName)}, {mbid, binary_to_list(ArtistMbid)}, {url, binary_to_list(ArtistUrl)},
    	{steamable, binary_to_list(Streamable)}, {listeners, binary_to_list(ArtistListeners)}, {playcount, binary_to_list(ArtistPlaycount)} ],
    		[ bio, {published, binary_to_list(ArtistBioPubDate)}, {summary, binary_to_list(ArtistBioSummary)}, {content, binary_to_list(ArtistBioContent)} ],
    			[ similar, [{name, binary_to_list(SimilarArtistName1)},{url, binary_to_list(SimilarArtistUrl1)}],
    			[{name, binary_to_list(SimilarArtistName2)},{url, binary_to_list(SimilarArtistUrl2)}],
    			[{name, binary_to_list(SimilarArtistName3)},{url, binary_to_list(SimilarArtistUrl3)}],
    			[{name, binary_to_list(SimilarArtistName4)},{url, binary_to_list(SimilarArtistUrl4)}],
    			[{name, binary_to_list(SimilarArtistName5)},{url, binary_to_list(SimilarArtistUrl5)}] ] ].
