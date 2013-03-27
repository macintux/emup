%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Store Meetup data in Riak
%%% @end
%%% Created : 8 Mar 2013 by John Daily <jd@epep.us>

-module(emup_riak).
-export([store_object/5]).

store_object(R, Bucket, Key, Value, TwoI) ->
    put_object(
      riakc_obj:set_secondary_index(create_object(Bucket, Key, Value),
                                    TwoI, R),
      R).

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
