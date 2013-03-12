%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Store Meetup data in Riak
%%% @end
%%% Created : 8 Mar 2013 by John Daily <jd@epep.us>

-module(emup_riak).

-include("emup.hrl").

-compile(export_all).

store_event(Event, R) ->
    Bucket = <<"meetup_events">>,
    Name = proplists:get_value(<<"name">>, Event),
    put_object(create_object(Bucket, Name, Event), R).


store_group(Group, R) ->
    Bucket = <<"meetup_groups">>,
    Name = proplists:get_value(<<"name">>, Group),
    put_object(create_object(Bucket, Name, Group), R).

put_object(Object, R) ->
    riakc_pb_socket:put(R, Object).

retrieve_or_create_object(Bucket, Key, R) ->
    create_object_unless(riakc_pb_socket:get(R, Bucket, Key),
                         Bucket, Key, R).

create_object_unless({ok, Obj}, _Bucket, _Key, _R) ->
    Obj;
create_object_unless({error, _}, Bucket, Key, R) ->
    create_object(Bucket, Key).

create_object(Bucket, Key) ->
    riakc_obj:new(Bucket, Key).

create_object(Bucket, Key, Value) ->
    riakc_obj:new(Bucket, Key, Value).
