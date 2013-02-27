%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2013 by John Daily <jd@epep.us>

-module(emup).
-export([start/1]).

-export([event_search/1, event_search/2,
         event_topic_filter/3, event_upcoming_filter/1, event_to_record/1,
         group_details/1, group_details/2, member_list/1, timestamp/1, timestamp/2,
         event_rsvps/2, render_event/2, render_events/2, simple_event_renderer/1, simple_event_renderer/2,
         csv_event_renderer/1]).

-include("emup.hrl").

start(Key) ->
    inets:start(),
    ssl:start(),
    emup_sup:start_link(),
    emup_sup:start_api(emup_api:single_auth(Key)).


%% event_search:
%%    1  Searches for the specified topic
%%    2  Filters events by the optional second argument
%%    3  Converts the JSON data to event records
%%
%% The meetup.com API itself sorts the results of the event search in
%% ascending order
event_search(Topic) ->
    event_search(Topic, fun(_X) -> true end).

event_search(Topic, Filter) ->
    lists:map(fun event_to_record/1,
              lists:filter(Filter,
                           proplists:get_value(<<"results">>,
                                               emup_api:events(text, Topic)))).

render_events(Topic, Renderer) ->
    lists:map(fun(X) -> render_event(X, Renderer) end, event_search(Topic)).

render_event(Event, Renderer) ->
    io_lib:format("~ts", [Renderer(Event)]).

%% Event filter functions (all defined against raw JSON data, not the
%% records). All take the event as the first value for consistency.

%%
%% See find_regex/2 for the list of supported Options
event_topic_filter(Event, Topic, Options) when not is_binary(Topic) ->
    event_topic_filter(Event, unicode:characters_to_binary(Topic), Options);
event_topic_filter(Event, Topic, Options) ->
    {ok, Re} = re:compile(find_regex(Topic, Options), [caseless]),
    find_string(Re, kvc:path([<<"group">>, <<"name">>], Event)) orelse
        find_string(Re, kvc:path([<<"description">>], Event)) orelse
        find_string(Re, kvc:path([<<"event_url">>], Event)) orelse
        find_string(Re, kvc:path([<<"name">>], Event)).

event_upcoming_filter(Event) ->
    proplists:get_value(<<"status">>, Event) =:= <<"upcoming">>.

location_to_record(undefined) ->
    undefined;
location_to_record(Location) ->
    #em_location{
               city = proplists:get_value(<<"city">>, Location),
               state = proplists:get_value(<<"state">>, Location),
               country = proplists:get_value(<<"country">>, Location),
               lat = proplists:get_value(<<"lat">>, Location),
               lon = proplists:get_value(<<"lon">>, Location)
             }.


venue_to_record(undefined) ->
    undefined;
venue_to_record(Venue) ->
    #em_venue{
            id = proplists:get_value(<<"id">>, Venue),
            name = proplists:get_value(<<"name">>, Venue)
          }.

%% An event doesn't always have a venue, but it always has a group, so
%% we embed group name and id into the record, but stash the venue in
%% a nested record.
%%
%% We create a location record based on venue if it is defined, group if
%% venue isn't.
event_to_record(Event) ->
    Group = proplists:get_value(<<"group">>, Event),
    #em_event{
            id = proplists:get_value(<<"id">>, Event),
            status = proplists:get_value(<<"status">>, Event),
            name = proplists:get_value(<<"name">>, Event),
            description = proplists:get_value(<<"description">>, Event),
            start = calendar:gregorian_seconds_to_datetime(proplists:get_value(<<"time">>, Event, 0) div 1000
                                                           + proplists:get_value(<<"utc_offset">>, Event, 0) div 1000),
            duration = proplists:get_value(<<"duration">>, Event, 0) div 1000,
            utc_offset = proplists:get_value(<<"utc_offset">>, Event, 0) div 1000,
            headcount = proplists:get_value(<<"headcount">>, Event),
            rsvp = proplists:get_value(<<"yes_rsvp_count">>, Event),
            rsvp_limit = proplists:get_value(<<"rsvp_limit">>, Event),
            url = proplists:get_value(<<"event_url">>, Event),
            group_id = proplists:get_value(<<"id">>, Group),
            group_name = proplists:get_value(<<"name">>, Group),
            venue = venue_to_record(proplists:get_value(<<"venue">>, Event)),
            location = location_to_record(proplists:get_value(<<"venue">>, Event,
                                                              Group))
          }.

%% Embedded in some group data, an organizer has different JSON
%% attributes than a normal member record. It depends on whether the
%% group data was returned from /find/groups (id) or /2/groups (member_id).
%% Location data will only be available via /2/groups.
organizer_to_record(undefined) ->
    undefined;
organizer_to_record(Person) ->
    #em_person{
             id = proplists:get_value(<<"id">>, Person,
                                     proplists:get_value(<<"member_id">>, Person)),
             name = proplists:get_value(<<"name">>, Person),
             location = #em_location{
               city = proplists:get_value(<<"member_city">>, Person),
               state = proplists:get_value(<<"member_state">>, Person),
               country = proplists:get_value(<<"member_country">>, Person)
              }
           }.

%% Operating on the assumption that new services may be added at any
%% time, create a proplist instead of a record
services_to_proplist(undefined) ->
    [];
services_to_proplist([]) ->
    [];
services_to_proplist(Services) ->
    io:format("~p~n", [Services]),
    lists:map(fun(X) -> { X, proplists:get_value(<<"identifier">>,
                                                 proplists:get_value(X, Services)) } end,
              proplists:get_keys(Services)).

person_to_record(undefined) ->
    undefined;
person_to_record(Person) ->
    #em_person{
             id = proplists:get_value(<<"id">>, Person),
             name = proplists:get_value(<<"name">>, Person),
             location = location_to_record(Person),
             aliases = services_to_proplist(proplists:get_value(<<"other_services">>,
                                                                Person))
           }.

group_to_record(Group) ->
    #em_group{
            id = proplists:get_value(<<"id">>, Group),
            name = proplists:get_value(<<"name">>, Group),
            description = proplists:get_value(<<"description">>, Group),
            url = proplists:get_value(<<"link">>, Group),
            member_count = proplists:get_value(<<"members">>, Group),
            created = proplists:get_value(<<"created">>, Group, 0) div 1000,
            member_label = proplists:get_value(<<"who">>, Group),
            organizer = organizer_to_record(
                          proplists:get_value(<<"organizer">>, Group))
          }.

event_rsvps(event_id, EventId) ->
    lists:map(fun(X) -> [{ <<"name">>, kvc:path([<<"member">>, <<"name">>], X)},
                         { <<"member_id">>, kvc:path([<<"member">>, <<"member_id">>], X)}] end,
              lists:filter(fun(X) -> proplists:get_value(<<"response">>, X) =:= <<"yes">> end,
                           proplists:get_value(<<"results">>, emup_api:rsvps(EventId)))).


group_details(Search) ->
    lists:map(fun group_digger/1, emup_api:find_groups(search, Search)).

group_details(group_id, GroupId) ->
    pick_first_group(proplists:get_value(<<"results">>, emup_api:group_info(GroupId))).

pick_first_group([]) ->
    no_such_group;
pick_first_group(Results) ->
    hd(Results).

group_digger([]) ->
    no_such_group;
group_digger(GroupData) ->
    [ proplists:get_value(<<"name">>, GroupData),
     { <<"group">>, group_to_record(GroupData)},
     { <<"members">>, member_list(GroupData) }].


find_string(Re, Text) ->
    case re:run(Text, Re) of
        {match, _} ->
            true;
        _ ->
            false
    end.

%% The only option supported is `word_boundary`, which ensures the
%% regular expression doesn't match Topic inside another word, but can
%% lead to false negatives if the topic adjoins another word in a URL
%% or group name.
find_regex(Topic, Options) ->
    Fmt = case lists:member(word_boundary, Options) of
              true ->
                  "(^|\\W)~ts($|\\W)";
              _ ->
                  "~ts"
          end,
    io_lib:format(Fmt, [Topic]).


member_list(Group) ->
    io:format("~p~n", [Group]),
    lists:map(fun person_to_record/1,
              proplists:get_value(<<"results">>,
                                  emup_api:members(
                                    proplists:get_value(<<"id">>, Group)))).


timestamp(Seconds) when is_integer(Seconds) ->
    timestamp(calendar:gregorian_seconds_to_datetime(Seconds));
timestamp({{Year, Month, Day}, {Hour, Min, _Sec}}) ->
    io_lib:format("~4.10.0B/~2.10.0B/~2.10.0B ~2B:~2.10.0B",
                  [Year + 1970, Month, Day, Hour, Min]).

timestamp(day_only, {{_Year, Month, Day}, {_Hour, _Min, _Sec}}) ->
    io_lib:format("~2.10.0B/~2.10.0B",
                  [Month, Day]).

simple_event_renderer(with_url, #em_event{url=Url}=Event) ->
    [simple_event_renderer(Event), "\n            * ", Url].

simple_event_renderer(#em_event{name=Title, start=Date, rsvp=Count, location=#em_location{city=City}}) ->
    io_lib:format("~3B  ~s  ~-15ts  ~ts", [Count, timestamp(day_only, Date), City, Title]).

csv_event_renderer(#em_event{name=Title, start=Date, rsvp=Count,
                             location=#em_location{city=City},
                             group_name=Group, url=Url}) ->
    io_lib:format("\"~ts\",\"~ts\",\"~ts\",\"~s\",\"~ts\",\"~B\"~n",
                  [City, Group, Title, timestamp(Date),
                   Url, Count]).
