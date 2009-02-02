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
%% @doc Module for the venue portion of the Last.fm API
%%
%% This code is available as Open Source Software under the MIT license.
%%
%% Updates at http://github.com/joewilliams/fermal/


-module(fermal_venue).

-export([get_venue_search/1]).

%% @doc retreives and parses a venue search request
get_venue_search(Url) ->
	JsonBody = fermal_util:get_body(Url),
	{ok,{obj,[{"results",
           {obj,[{"opensearch:Query",
                  {obj,[{"#text",_Text},
                        {"role",_Role},
                        {"searchTerms",SearchTerms},
                        {"startPage",_StartPage}]}},
                 {"opensearch:totalResults",TotalResults},
                 {"opensearch:startIndex",_StartIndex},
                 {"opensearch:itemsPerPage",_ItemsPerPage},
                 {"venuematches",
                  {obj,[{"venue",
                         [{obj,[{"id",VenueId1},
                                {"name",VenueName1},
                                {"location",{obj,[{"geo:point",
                     				{obj,[{"geo:lat",VenueGeoLat1},
                           			{"geo:long",VenueGeoLong1}]}},
                    				{"city",VenueCity1},
                    				{"country",VenueCountry1},
                    				{"street",VenueStreet1},
                    				{"postalcode",VenuePostalCode1}]}},
                                {"url",VenueUrl1}]},
                          {obj,[{"id",VenueId2},
                                {"name",VenueName2},
								{"location",{obj,[{"geo:point",
                     				{obj,[{"geo:lat",VenueGeoLat2},
                           			{"geo:long",VenueGeoLong2}]}},
                    				{"city",VenueCity2},
                    				{"country",VenueCountry2},
                    				{"street",VenueStreet2},
                    				{"postalcode",VenuePostalCode2}]}},
                                {"url",VenueUrl2}]},
                          {obj,[{"id",VenueId3},
                                {"name",VenueName3},
                                {"location",{obj,[{"geo:point",
                     				{obj,[{"geo:lat",VenueGeoLat3},
                           			{"geo:long",VenueGeoLong3}]}},
                    				{"city",VenueCity3},
                    				{"country",VenueCountry3},
                    				{"street",VenueStreet3},
                    				{"postalcode",VenuePostalCode3}]}},
                                {"url",VenueUrl3}]}]}]}},
                 {"for",_For}]}}]},
    []} = JsonBody,
    [ results,
    	{searchterms, binary_to_list(SearchTerms)},
    	{totalresults, binary_to_list(TotalResults)},
    	[ venuematches,
    		[ {id, binary_to_list(VenueId1)},
    		{name, binary_to_list(VenueName1)},
    		{url, binary_to_list(VenueUrl1)},
    			[ location,
     				{geopoint,
     					{geolat, binary_to_list(VenueGeoLat1)},
     					{geolong, binary_to_list(VenueGeoLong1)}},
     				{city, binary_to_list(VenueCity1)},
     				{country, binary_to_list(VenueCountry1)},
     				{street, binary_to_list(VenueStreet1)},
     				{postalcode, binary_to_list(VenuePostalCode1)} ] ],
     		[ {id, binary_to_list(VenueId2)},
     		{name, binary_to_list(VenueName2)},
     		{url, binary_to_list(VenueUrl2)},
     			[ location,
     				{geopoint,
     					{geolat, binary_to_list(VenueGeoLat2)},
     					{geolong, binary_to_list(VenueGeoLong2)}},
     				{city, binary_to_list(VenueCity2)},
     				{country, binary_to_list(VenueCountry2)},
     				{street, binary_to_list(VenueStreet2)},
     				{postalcode, binary_to_list(VenuePostalCode2)} ] ],
     		[ {id, binary_to_list(VenueId3)},
     		{name, binary_to_list(VenueName3)},
     		{url, binary_to_list(VenueUrl3)},
     			[ location,
     				{geopoint,
     					{geolat, binary_to_list(VenueGeoLat3)},
     					{geolong, binary_to_list(VenueGeoLong3)}},
     				{city, binary_to_list(VenueCity3)},
     				{country, binary_to_list(VenueCountry3)},
     				{street, binary_to_list(VenueStreet3)},
     				{postalcode, binary_to_list(VenuePostalCode3)} ] ] ] ].
