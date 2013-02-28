Erlang Meetup.com API library.

## Status

Very early days, works but needs help with OTP/Rebar, and the current
API hooks are not comprehensive.

Expect significant changes with no regard for backwards compatibility.

## Dependencies

* OAuth: [erlang-oauth](https://github.com/tim/erlang-oauth)
* JSON: [jsx](https://github.com/talentdeficit/jsx)
* KVC: [kvc](https://github.com/etrepum/kvc.git)

## Building

1. `$ rebar get-deps`
2. `$ rebar compile`

## Authenticating

Currently supports the Meetup.com API key as a parameter. OAuth 1.0a
support may arrive someday.

## Examples

    1> emup:start("<API key>").

    2> lists:foreach(fun(X) -> io:format("~ts~n", [X]) end, emup:render_events("Riak", fun(X) -> emup:simple_event_renderer(with_url, X) end)).
     65  02/25  Cambridge        Discuss "cloud war stories"
            * http://www.meetup.com/Cloudy-Mondays/events/92914182/
     16  02/25  Providence       February Prov Ops
            * http://www.meetup.com/provops/events/101785102/
     28  02/26  San Francisco    Deploying NoSQL Riak on Windows Azure
            * http://www.bayazure.org/events/102178482/
     32  02/26  London           Riak in action: how Temetra use Riak to track utility meter readings
            * http://www.meetup.com/riak-london/events/103646822/
     19  02/27  Radnor           Datomic by Joe Snikeris
            * http://www.meetup.com/Clojadelphia/events/99741942/
     ...

    9> MyGroupsJSON = emup_api:groups(member_of).
    10> emup:group_details(group_id, proplists:get_value(<<"id">>, lists:nth(3, proplists:get_value(<<"results">>, MyGroupsJSON)))).
    [{<<"lon">>,-87.63999938964844},
     {<<"visibility">>,<<"public">>},
     {<<"organizer">>,
      [{<<"name">>,<<"Ray Hightower">>},
       {<<"member_id">>,3576901}]},
     {<<"link">>,<<"http://www.meetup.com/ChicagoRuby/">>},
     {<<"state">>,<<"IL">>},
     {<<"join_mode">>,<<"open">>},
     {<<"who">>,<<"Rubyists">>},
     {<<"country">>,<<"US">>},
     {<<"city">>,<<"Chicago">>},

    11> emup_api:find_groups([{category, 34}, {zip, 60601}]).
    [[{<<"id">>,2949512},
      {<<"name">>,<<"Data Science Chicago">>},
      {<<"link">>,
       <<"http://www.meetup.com/Data-Science-Chicago/">>},
      {<<"urlname">>,<<"Data-Science-Chicago">>},
      {<<"description">>,
       <<"<p>Data Science Chicago brings together people interested in data science:</p>\n<ul>\n"...>>},
      {<<"created">>,1323882866000},
      {<<"city">>,<<"Chicago">>},
      {<<"country">>,<<"US">>},
      {<<"state">>,<<"IL">>},
      {<<"join_mode">>,<<"open">>},
      {<<"visibility">>,<<"public">>},
      {<<"lat">>,41.880001068115234},
      {<<"lon">>,-87.62999725341797},
      {<<"members">>,824},
      {<<"organizer">>,
       [{<<"id">>,14492981},{<<"name">>,<<"Mike Stringer">>}]},
      ...

     20> emup:local_events(34, "Indianapolis", {{2013, 03, 01}, {0, 0, 0}}, {{2013, 03, 10}, {0, 0, 0}}).
     [#em_event{id = <<"101825002">>,status = <<"upcoming">>,
           name = <<"Indiana Podcaster's Meetup">>,
           description = <<"<p>Join fellow podcasters from across Indiana! We're looking to connect podcasters to help g"...>>,
           start = {{2013,3,1},{19,0,0}},
           duration = 0,utc_offset = -18000,headcount = 0,rsvp = 7,
           rsvp_limit = undefined,
           url = <<"http://www.meetup.com/Indy-Podcasting-Network/events/101825002/">>,
           group_id = 5271952,
           group_name = <<"Indy Podcasting Network">>,
           venue = #em_venue{id = 1358330,name = <<"Tilt Studio">>},
           location = #em_location{city = <<"Indianapolis">>,
                                   state = <<"IN">>,country = <<"us">>,lat = 39.766304,
                                   lon = -86.159322}},
     #em_event{id = <<"qsghpcyrfbdb">>,status = <<"upcoming">>,
           name = <<"Open Project Workshop">>,
           description = <<"<p>Come in and meet other makers, and show off projects. Right now the space is under co"...>>,
           start = {{2013,3,2},{13,0,0}},
           duration = 0,utc_offset = -18000,headcount = 0,rsvp = 2,
           rsvp_limit = undefined,
           url = <<"http://www.meetup.com/Club-Cyberia/events/106176402/">>,
           group_id = 3329272,group_name = <<"Club Cyberia">>,
           venue = #em_venue{id = 5780072,
                             name = <<"Club Cyberia (In the Indy Self Stora"...>>},
           location = #em_location{city = <<"Indianapolis">>,
                                   state = <<"IN">>,country = <<"us">>,lat = 39.811115,
                                   lon = -86.050484}},
       ...

## Notes on development

Paging is surprisingly tricky. There are two different ways in which
the Meetup API indicates that the query should be re-run to collect
more information. I think it depends on the API generation.

`/find/groups` will return `Link` values as HTTP headers:

    Link: <https://api.meetup.com/find/groups?zip=60601&page=50&offset=3>; rel="next"
    Link: <https://api.meetup.com/find/groups?zip=60601&page=50&offset=1>; rel="prev"

`/2/events` will return `next` values in metadata (but no `prev` URL):

    "meta": {
         "link": "https://api.meetup.com/2/events",
         "method": "Events",
         "next": "https://api.meetup.com/2/events?group_id=1709331&status=upcoming&order=time&limited_events=False&desc=false&offset=1&format=json&page=3&fields=",
         "title": "Meetup Events v2",
         "total_count": 13,
         "updated": 1360773015000,
         "url": "https://api.meetup.com/2/events?group_id=1709331&status=upcoming&order=time&limited_events=False&desc=false&offset=0&format=json&page=3&fields="
    },

This generation gap also impacts the interpretation of JSON in
general.  The corresponding hash key to `meta` in the v2 API is
`results`, whose value is a list of entries, while the v3 API returns
just a list of entries with no `meta` or `results` key wrappers.

The new API results don't maintain any concept of a cursor, so if you
have 201 results, the default (and maximum) page size (200) will give
you all but 1, and the next URL that the API returns to grab the last
one will also return 200 results; it's up to the API client to filter
out the duplicates.

**This is a bug and has been filed.**

I have implemented a simple `next_page` function to take the URL and
return more results, but I don't have a good solution for removing
duplicates at the moment.
