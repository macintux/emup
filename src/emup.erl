%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2013 by John Daily <jd@epep.us>

-module(emup).
-export([start/1]).

-export([event_search/1, member_events/2, member_events/3,
         event_topic_filter/3, event_upcoming_filter/1, local_events/1, local_groups/2,
         render_group_details/3, group_details/1, group_details/2, member_list/1, timestamp/1, timestamp/2, all_results/1,
         event_rsvps/2, render_event/2, render_events/2, simple_event_renderer/1, simple_event_renderer/2,
         filter_events_by_date/3, check_date/3,
         csv_event_renderer/1, group_record_to_string/1, group_detail_to_csv/2,
         location_to_record/1, venue_to_record/1,
         event_to_record/1, organizer_to_record/1,
         person_to_record/1, group_to_record/1,
         fix_location/1
        ]).

-include("emup.hrl").



%% 719528 days from Jan 1, 0 to Jan 1, 1970
%%  *86400 seconds/day
-define(SEC_TO_EPOCH, 62167219200).

%% @doc Initiates the application with a Meetup.com API key
-spec start(string()) -> term().
start(Key) ->
    inets:start(),
    ssl:start(),
    emup_sup:start_link(),
    emup_sup:start_api(emup_api:single_auth(Key)).

%% @doc Replaces undefined event locations (typically because one
%% hasn't been announced yet) with location information pulled from
%% the group itself (e.g., the Berlin Ruby meetup will typically take
%% place in Berlin).
-spec fix_location(em_raw_object()) -> em_raw_object().
fix_location(Event) ->
    check_group_location(proplists:get_value(venue, Event),
                         kvc:path([group, id], Event),
                         Event).

%% @doc Helper function for fix_location(). If the location for an event
%% is anything other than undefined, leave it alone, else grab the group's
%% location.
-spec check_group_location(undefined|em_raw_object(), term(), em_raw_object()) -> em_raw_object().
check_group_location(undefined, GroupId, Event) ->
    NewEvent = proplists:delete(city,
                                proplists:delete(state,
                                                 proplists:delete(country, Event))),
    {ok, Results} = emup_api:group_info(GroupId),
    Group = hd(proplists:get_value(results, Results)),
    [{ venue,
      [
       { city, proplists:get_value(city, Group)},
       { state, proplists:get_value(state, Group)},
       { country, proplists:get_value(country, Group)}
      ]
     }]
        ++ NewEvent;
check_group_location(_, _GroupId, Event) ->
    Event.

%% @doc Return a list of events from a global search for Topic
-spec event_search(string()) -> list(em_raw_object()).
event_search(Topic) ->
    {ok, Results} = emup_api:events(text, Topic),
    proplists:get_value(results, Results).

%% @doc Leverage emup_api:next_page() to pull down all results
-spec all_results({ok, api_result()}) -> list(em_raw_object()).
all_results({ok, Batch1}) ->
    page_results(proplists:get_value(next, Batch1), proplists:get_value(results, Batch1)).

%% @doc Helper function for all_results()
-spec page_results(string(), list(em_raw_object())) -> list(em_raw_object()).
page_results(Url, Accum) when length(Url) =:= 0 ->
    lists:reverse(Accum);
page_results(Url, Accum) ->
    {ok, NextBatch} = emup_api:next_page(Url),
    page_results(proplists:get_value(next, NextBatch), proplists:get_value(results, NextBatch) ++ Accum).

%% @doc Return upcoming events for groups to which the given member ID
%% has joined, between the specified dates.
-spec member_events(ldate(), ldate(), string()) -> list(em_raw_object()).
member_events(StartDate, EndDate, MemberID) ->
    filter_events_by_date(all_results(emup_api:events(member, MemberID)),
                          StartDate, EndDate).

%% @doc Return upcoming events for the authenticating member between
%% the specified dates.
-spec member_events(ldate(), ldate()) -> list(em_raw_object()).
member_events(StartDate, EndDate) ->
    filter_events_by_date(all_results(emup_api:events(member)),
                          StartDate, EndDate).

%% @doc Given a list of groups, a list of events, and a date, print any
%% events that land on that date.
-spec check_date(list(em_group()), list(em_event()), sdate()) -> 'ok'.
check_date(Groups, Events, Date) ->
    StartDate = {Date, {0, 0, 0}},
    EndDate = {Date, {23, 59, 59}},
    TodayEvents = filter_events_by_date(Events, StartDate, EndDate),
    lists:foreach(fun(X) ->
                          io:format("~s (~4B/~4B) ~40ts ~80ts~n",
                                    [emup:timestamp(X#em_event.start),
                                     X#em_event.rsvp,
                                     proplists:get_value(members,
                                                         hd(lists:filter(fun(Y) -> proplists:get_value(id, Y) =:=  X#em_event.group_id end, Groups))),
                                     X#em_event.group_name,
                                     X#em_event.name
                                    ]) end, TodayEvents).

%% @doc Given a list of events and two dates, return all events
%% (converted to records) that fall between those dates (inclusive).
-spec filter_events_by_date(list(em_raw_object()), ldate(), ldate()) -> list(em_event()).
filter_events_by_date(Events, StartDate, EndDate) ->
    lists:sort(fun(A, B) -> A#em_event.start =< B#em_event.start end,
               lists:filter(fun(X) -> X#em_event.start >= StartDate andalso X#em_event.start =< EndDate end,
                            lists:map(fun event_to_record/1, Events))).

%% @doc Return a list of groups for any given city.
%%
%% Category is an integer from Meetup's API; emup_api:categories() will provide the list.
%% 34 is tech. Dates should be local to the city.
%%
%% This is very, very slow on multiple pages to avoid getting our API credentials blocked.
-spec local_groups(integer(), string()) -> list(em_raw_object()).
local_groups(Category, City) ->
    all_results(emup_api:find_groups([{category, Category}, {location, City}])).


%% @doc Return a list of events pulled from a list of groups.
%%
%% This is very, very slow for any significant list of groups.
-spec local_events(list(em_raw_object())) -> list(em_raw_object()).
local_events(Groups) ->
    collect_events_from_groups(Groups, []).

%% @doc Helper function for local_events()
-spec collect_events_from_groups(list(em_raw_object()),
                                 list(em_raw_object())) ->
                                        list(em_raw_object()).
collect_events_from_groups([], Accum) ->
    Accum;
collect_events_from_groups([H|T], Accum) ->
    timer:sleep(3000), %% Self-throttle
    collect_events_from_groups(T, all_results(
                                    emup_api:events(group,
                                                    proplists:get_value(id, H)))
                               ++ Accum).

%% @doc Return a list of strings representing upcoming events with the
%% specified topic.
-spec render_events(string(), fun((em_raw_object()) -> string())) -> list(string()).
render_events(Topic, Renderer) ->
    lists:map(fun(X) -> render_event(X, Renderer) end, event_search(Topic)).

%% @doc Return a string representing an individual event.
-spec render_event(em_raw_object(), fun((em_raw_object()) -> string())) -> string().
render_event(Event, Renderer) ->
    io_lib:format("~ts", [Renderer(Event)]).

%% Event filter functions (all defined against raw JSON data, not the
%% records). All take the event as the first value for consistency.

%% @doc Search an event for strings in the group name, description,
%% URL, or event name. Meetup's event search will return events when
%% the group's keyword includes the topic, which is generally not what
%% we want.
%%
%% See find_regex/2 for the list of supported Options.
-spec event_topic_filter(em_raw_object(), string()|binary(), list()) -> boolean().
event_topic_filter(Event, Topic, Options) when not is_binary(Topic) ->
    event_topic_filter(Event, unicode:characters_to_binary(Topic), Options);
event_topic_filter(Event, Topic, Options) ->
    {ok, Re} = re:compile(find_regex(Topic, Options), [caseless]),
    find_string(Re, kvc:path([group, name], Event)) orelse
        find_string(Re, kvc:path([description], Event)) orelse
        find_string(Re, kvc:path([event_url], Event)) orelse
        find_string(Re, kvc:path([name], Event)).

%% @doc Determine whether an event's status is "upcoming". To the best
%% of my knowledge, Meetup's API always only returns upcoming events.
-spec event_upcoming_filter(em_raw_object()) -> boolean().
event_upcoming_filter(Event) ->
    proplists:get_value(status, Event) =:= <<"upcoming">>.

%% @doc Convert a JSON location object to a record.
-spec location_to_record(undefined|em_raw_object()) -> undefined|em_location().
location_to_record(undefined) ->
    undefined;
location_to_record(Location) ->
    #em_location{
               city = proplists:get_value(city, Location),
               state = proplists:get_value(state, Location),
               country = proplists:get_value(country, Location),
               lat = proplists:get_value(lat, Location),
               lon = proplists:get_value(lon, Location)
             }.


%% @doc Convert a JSON venue object to a record.
-spec venue_to_record(undefined|em_raw_object()) -> undefined|em_venue().
venue_to_record(undefined) ->
    undefined;
venue_to_record(Venue) ->
    #em_venue{
            id = proplists:get_value(id, Venue),
            name = proplists:get_value(name, Venue)
          }.

%% @doc Convert a JSON event object to a record.
%%
%% An event doesn't always have a venue, but it always has a group, so
%% we embed group name and id into the record, but stash the venue in
%% a nested record.
%%
%% We create a location record based on venue if it is defined, group if
%% venue isn't.
-spec event_to_record(undefined|em_raw_object()) -> undefined|em_event().
event_to_record(Event) ->
    Group = proplists:get_value(group, Event),
    #em_event{
            id = proplists:get_value(id, Event),
            status = proplists:get_value(status, Event),
            name = proplists:get_value(name, Event),
            description = proplists:get_value(description, Event),
            start = calendar:gregorian_seconds_to_datetime(proplists:get_value(time, Event, 0) div 1000
                                                           + proplists:get_value(utc_offset, Event, 0) div 1000
                                                           + ?SEC_TO_EPOCH),
            duration = proplists:get_value(duration, Event, 0) div 1000,
            utc_offset = proplists:get_value(utc_offset, Event, 0) div 1000,
            headcount = proplists:get_value(headcount, Event),
            rsvp = proplists:get_value(yes_rsvp_count, Event),
            rsvp_limit = proplists:get_value(rsvp_limit, Event),
            url = proplists:get_value(event_url, Event),
            group_id = proplists:get_value(id, Group),
            group_name = proplists:get_value(name, Group),
            venue = venue_to_record(proplists:get_value(venue, Event)),
            location = location_to_record(proplists:get_value(venue, Event,
                                                              Group))
          }.

%% @doc Convert a JSON organizer object to a record.
%%
-spec organizer_to_record(undefined|em_raw_object()) -> undefined|em_person().
%% Embedded in some group data, an organizer has different JSON
%% attributes than a normal member record. It depends on whether the
%% group data was returned from /find/groups (id) or /2/groups (member_id).
%% Location data will only be available via /2/groups.
organizer_to_record(undefined) ->
    undefined;
organizer_to_record(Person) ->
    #em_person{
             id = proplists:get_value(id, Person,
                                     proplists:get_value(member_id, Person)),
             name = proplists:get_value(name, Person),
             location = #em_location{
               city = proplists:get_value(member_city, Person),
               state = proplists:get_value(member_state, Person),
               country = proplists:get_value(member_country, Person)
              }
           }.

%% @doc Turn third-party services (e.g., LinkedIn) associated with a
%% member account into a proplist (not a record because new services
%% could be added without warning).
-spec services_to_proplist(undefined|em_raw_object()) -> list(em_raw_object()).
services_to_proplist(undefined) ->
    [];
services_to_proplist([]) ->
    [];
services_to_proplist(Services) ->
    lists:map(fun(X) -> { X, service_value_to_binary(
                               proplists:get_value(identifier,
                                                   proplists:get_value(X, Services))) } end,
              proplists:get_keys(Services)).

%% Facebook identifiers are integers; much easier to treat them as
%% binary strings, same as other identifiers.
-spec service_value_to_binary(binary()|integer()) -> binary().
service_value_to_binary(Value) when is_integer(Value) ->
    list_to_binary(integer_to_list(Value));
service_value_to_binary(Value) ->
    Value.


%% @doc Convert a JSON person object to a record.
%%
%% @end
%% Example:
%% #em_person{id = 62024982,name = <<"John Daily">>,
%%            location = #em_location{city = <<"Indianapolis">>,
%%                                    state = <<"IN">>,country = <<"us">>,lat = 39.84000015258789,
%%                                    lon = -86.05000305175781},
%%            aliases = [{twitter,<<"@macintux">>}]}
-spec person_to_record(undefined|em_raw_object()) -> undefined|em_person().
person_to_record(undefined) ->
    undefined;
person_to_record(Person) ->
    #em_person{
             id = proplists:get_value(id, Person),
             name = proplists:get_value(name, Person),
             location = location_to_record(Person),
             aliases = services_to_proplist(proplists:get_value(other_services,
                                                                Person))
           }.

%% @doc Convert a JSON group object to a record.
-spec group_to_record(em_raw_object()) -> em_group().
group_to_record(Group) ->
    #em_group{
            id = proplists:get_value(id, Group),
            name = proplists:get_value(name, Group),
            description = proplists:get_value(description, Group),
            url = proplists:get_value(link, Group),
            member_count = proplists:get_value(members, Group),
            created = proplists:get_value(created, Group, 0) div 1000,
            member_label = proplists:get_value(who, Group),
            organizer = organizer_to_record(
                          proplists:get_value(organizer, Group)),
            location = location_to_record(Group)
          }.

%% @doc Return a list of "yes" RSVPs for an event.
-spec event_rsvps(event_id, string()) -> list(tuple()).
event_rsvps(event_id, EventId) ->
    lists:map(fun(X) -> [{ name, kvc:path([member, name], X)},
                         { member_id, kvc:path([member, member_id], X)}] end,
              lists:filter(fun(X) -> proplists:get_value(response, X) =:= <<"yes">> end,
                           proplists:get_value(results, emup_api:rsvps(EventId)))).

%% @doc Invoke renderer on group + member detail when provided a list
-spec render_group_details(list(list(tuple())),
                           fun((em_group()) -> ok),
                           fun((em_person()) -> ok)
                          ) -> ok.
render_group_details(Groups, RenderGroup, RenderPerson) ->
    lists:foreach(fun(G) -> RenderGroup(proplists:get_value(group, G)),
                            lists:foreach(fun(M) -> RenderPerson(M) end,
                                          lists:sort(fun(A, B) ->
                                                             string:to_lower(
                                                               binary_to_list(A#em_person.name)) <
                                                                 string:to_lower(
                                                                   binary_to_list(B#em_person.name))
                                                     end,
                                                     proplists:get_value(members, G)))
                            end,
                  Groups).

%% @doc Convert group record to string
-spec group_record_to_string(em_group()) -> string().
group_record_to_string(Group) ->
    io_lib:format("=== ~-15ts ~ts", [(Group#em_group.location)#em_location.city,
                  Group#em_group.name]).

%% @doc Convert person record to string
-spec person_record_to_string(em_person()) -> string().
person_record_to_string(Person) ->
    io_lib:format("~-60ts~n~ts", [Person#em_person.name,
                                 aliases_to_string(Person#em_person.aliases)]).

%% @doc Convert list of aliases (Twitter, LinkedIn, etc) to string
-spec aliases_to_string(list(tuple())) -> iolist().
aliases_to_string(Aliases) ->
    aliases_to_string_aux(Aliases, []).

-spec aliases_to_string_aux(list(tuple()), iolist()) -> iolist().
aliases_to_string_aux([], Buffer) ->
    Buffer;
aliases_to_string_aux([{Service, Alias}|T], Buffer) when is_integer(Alias)->
    aliases_to_string_aux(T, [Buffer,
                              io_lib:format("    +~-15ts ~B~n",
                                            [atom_to_list(Service),
                                             Alias])]);
aliases_to_string_aux([{Service, Alias}|T], Buffer) ->
    aliases_to_string_aux(T, [Buffer,
                              io_lib:format("    +~-15ts ~ts~n",
                                            [atom_to_list(Service),
                                             Alias])]).



%% @doc Return a list of groups + members for a given search string.
-spec group_details(string()) -> list(group_container()).
group_details(Search) ->
    lists:map(fun group_digger/1,
              all_results(emup_api:find_groups(search, Search))).

-spec group_details('group_id', string()) -> group_container().
group_details(group_id, GroupId) ->
    group_digger(pick_first_group(all_results(emup_api:group_info(GroupId)))).

pick_first_group([]) ->
    no_such_group;
pick_first_group(Results) ->
    hd(Results).

group_digger([]) ->
    no_such_group;
group_digger(GroupData) ->
    [ { name, proplists:get_value(name, GroupData)},
      { group, group_to_record(GroupData)},
      { members, member_list(GroupData) }].


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
    lists:map(fun person_to_record/1,
              all_results(emup_api:members(
                            proplists:get_value(id, Group)))).


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

group_detail_to_csv(GroupId, Filename) ->
    {ok, CSV} = file:open(Filename, [write, {encoding, utf8}]),
    GroupRender = fun(X) ->
                          io:fwrite(CSV,
                                    "\"~ts\",\"~ts\",\"~ts\"~n",
                                    [(X#em_group.location)#em_location.city,
                                     X#em_group.name, X#em_group.url]) end,
    PersonRender = fun(X) ->
                           io:fwrite(CSV,
                                     "\"~ts\",\"~ts\",\"~ts\",\"~ts\",\"~ts\",\"~ts\"~n",
                                     [html_to_unicode(X#em_person.name),
                                      proplists:get_value(twitter, X#em_person.aliases, ""),
                                      proplists:get_value(linkedin, X#em_person.aliases, ""),
                                      proplists:get_value(facebook, X#em_person.aliases, ""),
                                      proplists:get_value(tumblr, X#em_person.aliases, ""),
                                      proplists:get_value(flickr, X#em_person.aliases, "")])
                   end,
    render_group_details([group_details(group_id, GroupId)],
                         GroupRender, PersonRender),
    file:close(CSV).

%% Meetup gives us "&#272;or&#273;e Torbica" instead of "Đorđe Torbica"
%% 272 dec => 0110 hex => 0420 oct
%% 3> io:format("~ts~n", [unicode:characters_to_list(<<196,144>>, utf8)]).
%% Đ
%% 4> io:format("~ts~n", [unicode:characters_to_list(<<196,145>>, utf8)]).
%% đ
%% (196,144 values stolen from http://www.utf8-chartable.de/unicode-utf8-table.pl?start=256&utf8=dec)
%% io:format("~ts~n", [unicode:characters_to_list(<<272/utf8>>)]).
%% Đ
%% 25>     {ok, Re3} = re:compile("&#([0-9]+);", [unicode]).
%% {ok,{re_pattern,1,1,
%%                 <<69,82,67,80,103,0,0,0,0,8,0,0,7,0,0,0,1,0,0,0,38,0,59,
%%                   ...>>}}
%% 26> {match, List2} = re:run(<<"&#272;or&#273;e Torbica">>, Re3, [global, {capture, all, binary}]).
%% {match,[[<<"&#272;">>,<<"272">>],[<<"&#273;">>,<<"273">>]]}
%% 38> A = hd(tl(hd(List2))).
%% <<"272">>
%% 48> A2 = list_to_integer(binary_to_list(A)).
%% 272
%% 49> A3 = <<A2/utf8>>.
%% <<196,144>>
%% 50> io:format("~ts~n", [A3]).
%% Đ
-spec html_to_unicode(string()) -> string().
html_to_unicode(String) ->
    io:format("Matching against ~ts~n", [String]),
    {ok, Re} = re:compile("&#([0-9]+);", []),
    find_matches(re:run(String, Re, [global, {capture, all, binary}]),
                 String).

-spec find_matches(nomatch|{match, list()}, string()) -> string().
find_matches(nomatch, String) ->
    String;
find_matches({match, []}, String) ->
    String;
find_matches({match, [H|T]}, String) ->
    io:format("~p / ~p / ~ts~n", [H, T, String]),
    [HTML, Bin] = H,
    find_matches({match, T},
                 replace_match(HTML, unicode_conversion(Bin), String)).

-spec replace_match(string(), binary(), string()) -> string().
replace_match(Old, UTF8, String) ->
    re:replace(String, Old, UTF8, [global]).

-spec unicode_conversion(binary()) -> binary().
unicode_conversion(Bin) ->
    Integer = list_to_integer(binary_to_list(Bin)),
    <<Integer/utf8>>.
