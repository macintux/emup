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
