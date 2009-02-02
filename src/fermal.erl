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
%% @doc A Last.fm API Library for Erlang.
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal).

-behaviour(gen_server).


-define(SERVER, ?MODULE).
-define(API_KEY, "63f8d5e4fa25774a097ac1d299dce5f4").
-define(API_URL, "http://ws.audioscrobbler.com/2.0/?method=").
-define(FORMAT, "&format=json").
-define(LIMIT, "&limit=3").

%% API
-export([start/0, artist_info/1, tasteometer/2, album_info/2,
		track_info/2, venue_search/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    inets:start().

init([]) ->
    {ok, #state{}}.


handle_call({artist_info, {Artist}}, _From, State) ->
    Reply = fermal_artist:get_artist_info(?API_URL ++ "artist.getinfo&artist=" ++ Artist ++ "&api_key=" ++ ?API_KEY ++ ?FORMAT),
    {reply, Reply, State};

handle_call({tasteometer, {User1, User2}}, _From, State) ->
    Reply = fermal_tasteometer:get_tasteometer(?API_URL ++ "tasteometer.compare&type1=user&type2=user&value1=" ++ User1
    	++ "&value2=" ++ User2 ++ "&api_key=" ++ ?API_KEY ++ ?FORMAT),
    {reply, Reply, State};

handle_call({album_info, {Artist, Album}}, _From, State) ->
    Reply = fermal_album:get_album_info(?API_URL ++ "album.getinfo&artist=" ++ Artist ++ "&album=" ++ Album ++"&api_key=" ++ ?API_KEY ++ ?FORMAT),
    {reply, Reply, State};

handle_call({track_info, {Artist, Track}}, _From, State) ->
    Reply = fermal_track:get_track_info(?API_URL ++ "track.getinfo&artist=" ++ Artist ++ "&track=" ++ Track ++"&api_key=" ++ ?API_KEY ++ ?FORMAT),
    {reply, Reply, State};

handle_call({venue_search, {Venue}}, _From, State) ->
    Reply = fermal_venue:get_venue_search(?API_URL ++ "venue.search&venue=" ++ Venue ++"&api_key=" ++ ?API_KEY ++ ?FORMAT ++ ?LIMIT),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc start gen_server
start() ->
	start_link().

%% @doc gets info about an artist
artist_info(Artist) ->
	gen_server:call(?SERVER, {artist_info, {Artist}}).

%% @doc compares two users for similar tastes
tasteometer(User1, User2) ->
	gen_server:call(?SERVER, {tasteometer, {User1, User2}}).

%% @doc gets info about an album
album_info(Artist, Album) ->
	gen_server:call(?SERVER, {album_info, {Artist, Album}}).

%% @doc search for a venue
venue_search(Venue) ->
	gen_server:call(?SERVER, {venue_search, {Venue}}).

%% @doc get track info
track_info(Artist, Track) ->
	gen_server:call(?SERVER, {track_info, {Artist, Track}}).
