%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2013 by John Daily <jd@epep.us>

-module(emup).
-export([start/1]).

-export([event_search/1, event_search/2, member_events/2, member_events/3, remove_duplicates_by_id/1,
         event_topic_filter/3, event_upcoming_filter/1, event_to_record/1, local_events/1, local_groups/2,
         group_details/1, group_details/2, member_list/1, timestamp/1, timestamp/2, all_results/1,
         event_rsvps/2, render_event/2, render_events/2, simple_event_renderer/1, simple_event_renderer/2,
         filter_events_by_date/3, check_date/3,
         csv_event_renderer/1]).

-include("emup.hrl").

%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).


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

%% Leverage emup_api:next_page() to pull down all results
all_results({ok, Batch1}) ->
    page_results(proplists:get_value(next, Batch1), proplists:get_value(results, Batch1)).

page_results(Url, Accum) when length(Url) =:= 0 ->
    lists:reverse(Accum);
page_results(Url, Accum) ->
    {ok, NextBatch} = emup_api:next_page(Url),
    page_results(proplists:get_value(next, NextBatch), proplists:get_value(results, NextBatch) ++ Accum).

member_events(StartDate, EndDate, MemberID) ->
    filter_events_by_date(all_results(emup_api:events(member, MemberID)),
                          StartDate, EndDate).

member_events(StartDate, EndDate) ->
    filter_events_by_date(all_results(emup_api:events(member)),
                          StartDate, EndDate).

check_date(Groups, Events, Date) ->
    StartDate = {Date, {0, 0, 0}},
    EndDate = {Date, {23, 59, 59}},
    TodayEvents = filter_events_by_date(Events, StartDate, EndDate),
    lists:foreach(fun(X) ->
                          io:format("~s (~4B/~4B) ~40ts ~80ts~n",
                                    [emup:timestamp(X#em_event.start),
                                     X#em_event.rsvp,
                                     proplists:get_value(<<"members">>,
                                                         hd(lists:filter(fun(Y) -> proplists:get_value(<<"id">>, Y) =:=  X#em_event.group_id end, Groups))),
                                     X#em_event.group_name,
                                     X#em_event.name
                                    ]) end, TodayEvents).

filter_events_by_date(Events, StartDate, EndDate) ->
    lists:sort(fun(A, B) -> A#em_event.start =< B#em_event.start end,
               lists:filter(fun(X) -> X#em_event.start >= StartDate andalso X#em_event.start =< EndDate end,
                            lists:map(fun event_to_record/1, Events))).

remove_duplicates_by_id(List) ->
    SortedList = lists:sort(
                   fun(A, B) ->
                           proplists:get_value(<<"id">>, A) =< proplists:get_value(<<"id">>, B)
                   end,
                   List),
    remove_id_dupes_aux(SortedList, -999999, []).

remove_id_dupes_aux([], _LastId, Accum) ->
    lists:reverse(Accum);
remove_id_dupes_aux([H|T], LastId, Accum) ->
    case proplists:get_value(<<"id">>, H) of
        LastId ->
            remove_id_dupes_aux(T, LastId, Accum);
        OtherId ->
            remove_id_dupes_aux(T, OtherId, [H] ++ Accum)
    end.

%% Category is an integer from Meetup's API; emup_api:categories() will provide the list.
%% 34 is tech. Dates should be local to the city.
%%
%% This is a very intensive request, so throttle ourselves to keep from annoying Meetup.com.
%%
%% Also filter out duplicate groups, since as of this writing there's
%% a bug with the Meetup API paging logic that results in duplication
local_groups(Category, City) ->
    remove_duplicates_by_id(all_results(emup_api:find_groups([{category, Category}, {location, City}]))).

local_events(Groups) ->
    collect_events_from_groups(Groups, []).

collect_events_from_groups([], Accum) ->
    Accum;
collect_events_from_groups([H|T], Accum) ->
    timer:sleep(1000), %% Self-throttle
    collect_events_from_groups(T, all_results(
                                    emup_api:events(group,
                                                    proplists:get_value(<<"id">>, H)))
                               ++ Accum).


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
                                                           + proplists:get_value(<<"utc_offset">>, Event, 0) div 1000
                                                           + ?SEC_TO_EPOCH),
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
    timestamp(calendar:gregorian_seconds_to_datetime(Seconds + ?SEC_TO_EPOCH));
timestamp({{Year, Month, Day}, {Hour, Min, _Sec}}) when Year > 1970 ->
    io_lib:format("~4.10.0B/~2.10.0B/~2.10.0B ~2B:~2.10.0B",
                  [Year, Month, Day, Hour, Min]);
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
