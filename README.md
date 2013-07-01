Erlang Meetup.com API library.

## Status

Very early days, works but needs help with OTP/Rebar, and the current
API hooks are not comprehensive.

Expect significant changes with no regard for backwards compatibility.

Haven't run it from any other context than the `erl` shell.

## Dependencies

* OAuth: [erlang-oauth](https://github.com/tim/erlang-oauth)
* JSON: [jsx](https://github.com/talentdeficit/jsx)
* KVC: [kvc](https://github.com/etrepum/kvc.git)

## Building

1. `$ rebar get-deps`
2. `$ rebar compile`

## Docs
Edoc is slowly making its way through the library; running it is
highly recommended. Since some functions take JSON proplists as
arguments, and others (but fairly few) take records, the `-spec`
typing hints are generally useful.

## Authenticating

Currently supports the Meetup.com API key as a parameter. OAuth 1.0a
support may arrive someday.

## Examples

    1> emup:start("<API key>").

    2> lists:foreach(fun(X) -> io:format("~ts~n", [X]) end, emup:render_events("Riak", fun(X) -> emup:simple_event_renderer(with_url, emup:group_to_record(X)) end)).
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

    55> ChicagoGroups = emup:local_groups(34, "Chicago").
    56> ChicagoEvents = emup:local_events(ChicagoGroups).
    57> emup:check_date(ChicagoGroups, ChicagoEvents, {2013, 03, 18}).
    2013/03/18 17:30 (  62/1109)    Chicago area Hadoop User Group (CHUG)                                      SQL Server PDW w. Polybase:  What, Why, How
    2013/03/18 18:00 (  44/ 786)                             momo-chicago                                                Mobile Game Apps and Sencha Touch


This rather ugly shell-foo will generate a CSV file with all groups
and members of each group for the search string passed to
`emup:group_details/1`.

    28> RiakGroups = emup:group_details("Riak").
    29> {ok, CSV} = file:open("riak.csv", [write, {encoding, utf8}]).
    30> GR = fun(X) -> io:fwrite(CSV, "\"~ts\",\"~ts\",\"~ts\"~n", [(X#em_group.location)#em_location.city, X#em_group.name, X#em_group.url]) end.
    31> PR = fun(X) -> io:fwrite(CSV, "\"~ts\",\"~ts\",\"~ts\",\"~ts\",\"~ts\",\"~ts\"~n", [X#em_person.name, proplists:get_value(twitter, X#em_person.aliases, ""), proplists:get_value(linkedin, X#em_person.aliases, ""), proplists:get_value(facebook, X#em_person.aliases, ""), proplists:get_value(tumblr, X#em_person.aliases, ""), proplists:get_value(flickr, X#em_person.aliases, "")]) end.
    32> emup:render_group_details(lists:sort(fun(A, B) -> proplists:get_value(name, A) < proplists:get_value(name, B) end, RiakGroups), GR5, PR5).
    33> file:close(CSV).


## Pagination

Paging is surprisingly tricky. There are two different ways in which
the Meetup API indicates that the query should be re-run to collect
more information.

### First generation API
`/find/groups` will return `Link` values as HTTP headers:

    Link: <https://api.meetup.com/find/groups?zip=60601&page=50&offset=3>; rel="next"
    Link: <https://api.meetup.com/find/groups?zip=60601&page=50&offset=1>; rel="prev"

### Second generation API
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

### Abstracted

The `emup:all_results/1` function handles pagination for you; nearly
every call to `emup_api` is sent to `all_results` for processing. Some
that aren't currently should be.

## JSON output across generations

### First generation API
The JSON is just a list of results:

    [
        {
            "category": {
                "id": 34,
                "name": "Tech",
                "shortname": "Tech"
            },
            "city": "Austin",
            "country": "US",
            "created": 1364616219000,
            "description": "Interested in distributed systems? Riak is a bullet-proof key/value data store inspired by Amazon's Dynamo paper. We will look at distributed systems in general and Riak specifically, exploring best-practices, pitfalls, and strategies. Our goal is to bring in speakers from the community as well as from Basho itself, to help share experiences and solutions to help members get the most out of Riak and learn from each others' approaches and mistakes.",
            "id": 7841512,
            "join_mode": "open",
            "lat": 30.270000457763672,
            "link": "http://www.meetup.com/Austin-Riak-Meetup/",
            "lon": -97.73999786376953,
            "members": 29,
            "name": "Austin Riak Meetup",
            "organizer": {
                "id": 62024982,
                "name": "John Daily"
            },
            "state": "TX",
            "urlname": "Austin-Riak-Meetup",
            "visibility": "public",
            "who": "Riaktors"
        },
        ...


### Second generation API
Since the 2nd generation API has metadata embedded in the API output, the list of results that comprised the entirety of the v1 JSON is now nested under a `results` label, with metadata nested under `meta`:

    {
        "meta": {
            "count": 200,
            "description": "Access Meetup events using a group, member, or event id. Events in private groups are available only to authenticated members of those groups. To search events by topic or location, see [Open Events](/meetup_api/docs/2/open_events).",
            "id": "",
            "lat": "",
            "link": "https://api.meetup.com/2/events",
            "lon": "",
            "method": "Events",
            "next": "https://api.meetup.com/2/events?key=730162a9547b3a82b4816d60636f&status=upcoming&order=time&limited_events=False&desc=false&member_id=62024982&offset=1&format=json&page=200&fields=",
            "title": "Meetup Events v2",
            "total_count": 363,
            "updated": 1372569328000,
            "url": "https://api.meetup.com/2/events?key=730162a9547b3a82b4816d60636f&status=upcoming&order=time&limited_events=False&desc=false&member_id=62024982&offset=0&format=json&page=200&fields="
        },
        "results": [
            {
                "created": 1357509140000,
                "description": "<p>Hot Jazz with no cover every Sunday night with a local group. Jared Thomson leads on Sax, with Steven Jones on keyboards, and various other locals filling in drums, guitar, trumpet, etc.</p>\n<p><a href=\"http://www.youtube.com/watch?v=PSlSUlEjCK4&amp;feature=share&amp;list=UUdtFA8oLFn9Pl1HWJokFGvQ\">Premium Blend on Youtube</a></p>\n<p>\u00a0</p>",
                "event_url": "http://www.meetup.com/Indy-In-Tune-Local-Music-Supporters/events/126081112/",
                "group": {
                    "group_lat": 39.77000045776367,
                    "group_lon": -86.16000366210938,
                    "id": 6105732,
                    "join_mode": "open",
                    "name": "Indy In-Tune:  Local Music Supporters",
                    "urlname": "Indy-In-Tune-Local-Music-Supporters",
                    "who": "Local Music Fans"
                },
                "headcount": 0,
                "id": "qkmvcfyrjbnc",
                "maybe_rsvp_count": 0,
                "name": "Premium Blend (Jazz)",
                "status": "upcoming",
            ...
