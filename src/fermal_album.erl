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
%% @doc Module for the album portion of the Last.fm API
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_album).

-export([get_album_info/1]).

%% @doc retreives and parses an album info request
get_album_info(Url) ->
	JsonBody = fermal_util:get_body(Url),
	{ok,{obj,[{"album",
           {obj,[{"name", AlbumTitle},
                 {"artist", Artist},
                 {"id", Id},
                 {"mbid", Mbid},
                 {"url", ArtistUrl},
                 {"releasedate", ReleaseDate},
                 _ArtistImages,
                 {"listeners", Listeners},
                 {"playcount", Playcount},
                 {"toptags", TopTags}]}}]},
    []} = JsonBody,
    [ album,
    	{name, binary_to_list(AlbumTitle)},
    	{artist, binary_to_list(Artist)},
    	{id, binary_to_list(Id)},
    	{mbid, binary_to_list(Mbid)},
    	{url, binary_to_list(ArtistUrl)},
    	{releasedate, binary_to_list(ReleaseDate)},
    	{listeners, binary_to_list(Listeners)},
    	{playcount, binary_to_list(Playcount)},
    	{toptags, binary_to_list(TopTags)} ].
