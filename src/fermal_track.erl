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
%% @doc Module for the track portion of the Last.fm API
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_track).

-export([get_track_info/1]).

%% @doc retreives and parses an artist info request
get_track_info(Url) ->
	JsonBody = fermal_util:get_body(Url),
	{ok,{obj,[{"track",
           {obj,[{"id", TrackId},
                 {"name", TrackName},
                 {"mbid", TrackMbid},
                 {"url", TrackUrl},
                 {"duration",TrackDuration},
                 {"streamable",
                  {obj,[_Text,{"fulltrack",Streamable}]}},
                 {"listeners",Listeners},
                 {"playcount",Playcount},
                 {"artist",
                  {obj,[{"name",ArtistName},
                        {"mbid",ArtistMbid},
                        {"url",ArtistUrl}]}},
                 {"album",
                  {obj,[{"artist",AlbumArtistName},
                        {"title",AlbumName},
                        {"mbid",AlbumMbid},
                        {"url",AlbumUrl},
                        _Images,
                        {"position", AlbumPosition}]}},
                 {"toptags",
                 	{obj,[{"tag",
                         [{obj,[{"name",TagName1},
                         	{"url",TagUrl1}]},
                          {obj,[{"name",TagName2},
                          	{"url",TagUrl2}]},
                          {obj,[{"name",TagName3},
                          	{"url",TagUrl3}]},
                          {obj,[{"name",TagName4},
                          	{"url",TagUrl4}]},
                          {obj,[{"name",TagName5},
                          	{"url",TagUrl5}]}]}]}}]}}]},
    []} = JsonBody,
    [ track, {id, binary_to_list(TrackId)}, {name, binary_to_list(TrackName)}, {mbid, binary_to_list(TrackMbid)},
    {url, binary_to_list(TrackUrl)}, {duration, binary_to_list(TrackDuration)}, {streamable, binary_to_list(Streamable)},
    {listeners, binary_to_list(Listeners)}, {playcount, binary_to_list(Playcount)} ,
    [ artist, {name, binary_to_list(ArtistName)}, {mbid, binary_to_list(ArtistMbid)}, {url, binary_to_list(ArtistUrl)} ],
    [ album, {artist, binary_to_list(AlbumArtistName)}, {title, binary_to_list(AlbumName)}, {mbid, binary_to_list(AlbumMbid)},
    {url, binary_to_list(AlbumUrl)}, {position, binary_to_list(AlbumPosition)} ],
 	[ toptags, [{name, binary_to_list(TagName1)}, {url, binary_to_list(TagUrl1)}], [{name, binary_to_list(TagName2)}, {url, binary_to_list(TagUrl2)}],
 	[{name, binary_to_list(TagName3)}, {url, binary_to_list(TagUrl3)}], [{name, binary_to_list(TagName4)}, {url, binary_to_list(TagUrl4)}],
 	[{name, binary_to_list(TagName5)}, {url, binary_to_list(TagUrl5)}] ] ].
