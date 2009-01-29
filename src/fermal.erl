-module(fermal).

-behaviour(gen_server).


-define(SERVER, ?MODULE).
-define(API_KEY, "APIKEY").
-define(API_URL, "http://ws.audioscrobbler.com/2.0/?method=").

%% API
-export([start_link/0, artist_info/1]).

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
    Reply = get_artist_info(Artist),
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

artist_info(Artist) ->
	gen_server:call(?SERVER, {artist_info, {Artist}}).

get_artist_info(Artist) ->
	FullUrl = ?API_URL ++ "artist.getinfo&format=json&artist=" ++ Artist ++ "&api_key=" ++ ?API_KEY,
	{ ok, { _Status, _Headers, Body }} = http:request(FullUrl),
	JsonBody = rfc4627:decode(Body),
	{ok,
      {obj,
       [{"artist",
         {obj,
          [{"name",Name},
           {"mbid",Mbid},
           {"url",Url},
           Images,
           {"streamable",<<"1">>},
           {"stats",
            {obj,[{"listeners",StatsListeners},{"playcount",StatsPlayCounts}]}},
           SimilarArtists,
           {"bio",
            {obj,
             [{"published",BioPublished},
              {"summary",
               BioSummary},
              {"content",
               BioContent}]}}]}}]},
               []} = JsonBody,
    [binary_to_list(Name), binary_to_list(Url), binary_to_list(StatsListeners), binary_to_list(StatsPlayCounts)].
