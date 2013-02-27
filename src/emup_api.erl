%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%    Meetup API wrapper.
%%% @end

-module(emup_api).
-behavior(gen_server).

%% Our API
-export([single_auth/1, start_link/1, stop/0, next_page/1,
         members/1, member_info/1, event_info/1, group_info/1,
         groups/1, groups/2, events/1, events/2, find_groups/1, find_groups/2, rsvps/1
        ]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(auth, {
          type=apikey,  %% alternative: type=oauth1
          apikey=""
          }).

-type auth() :: #auth{}.

-record(state, {
          auth,
          user,
          urls=[]
          }).

-type state() :: #state{}.

-define(BASE_URL(X), "https://api.meetup.com/" ++ X).
-define(OAUTH_URL(X), "https://api.meetup.com/oauth/" ++ X).

-define(SERVER, ?MODULE).

%% API

%% @doc Construct an authorization object with single-user API key
-spec single_auth(string()) -> auth().
single_auth(ApiKey) ->
    #auth{apikey=ApiKey}.

%% @doc Starts the API service
%%
%% @spec start_link(Auth::auth()) -> {ok, Pid::pid()}

-spec start_link(auth()) -> {ok, pid()}.
start_link(Auth) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Auth], []).

stop() ->
    gen_server:cast(?SERVER, stop).

members(GroupId) ->
    gen_server:call(?SERVER, {members, GroupId}, 10000).

member_info(MemberId) ->
    gen_server:call(?SERVER, {member_info, MemberId}, infinity).

groups(member_of) ->
    gen_server:call(?SERVER, {groups, {member_id, authorized}}).

groups(member_of, MemberId) ->
    gen_server:call(?SERVER, {groups, {member_id, MemberId}}).

events(group, GroupId) ->
    gen_server:call(?SERVER, {events, {group_id, GroupId}});
events(member, MemberId) ->
    gen_server:call(?SERVER, {events, {member_id, MemberId}});
%% See note below
events(text, Topic) ->
    gen_server:call(?SERVER, {events, {text, Topic}}).

event_info(EventId) when is_number(EventId) ->
    gen_server:call(?SERVER, {event_info, integer_to_list(EventId)});
event_info(EventId) ->
    gen_server:call(?SERVER, {event_info, EventId}).

group_info(GroupId) ->
    gen_server:call(?SERVER, {group_info, GroupId}).

rsvps(EventId) ->
    gen_server:call(?SERVER, {rsvps, EventId}).


%% We hand the next page URL back to the client, and the client gives
%% it back to us if more results are desired
next_page(Url) ->
    gen_server:call(?SERVER, {next_page, Url}).

%% Note on event topic searches: they will match on group metadata as
%% well as event description/title/URL, so there will be false
%% positives here. May wish to do further text searches to verify the
%% desired topic is present somewhere meaningful.
%%
%% I tried 'topic' as search key instead of 'text', but that missed a
%% notable meeting that included the search value in the description.

events(member) ->
    gen_server:call(?SERVER, {events, {member_id, authorized}}).

find_groups(Params) ->
    gen_server:call(?SERVER, {groups, {params, Params}}, 10000).

find_groups(search, Text) ->
    gen_server:call(?SERVER, {groups, {search_text, Text}}, 10000).


%% behavior implementation
init([Auth]) ->
    Urls = meetup_urls(),
    init_auth(Auth, Urls, user_info(meetup_call(#state{auth=Auth, urls=Urls}, verify_creds, []))).

user_info({ok, MemberResponse}) ->
    %% Verify we just got details for one user back, and return those details
    1 = proplists:get_value(count, MemberResponse),
    hd(proplists:get_value(results, MemberResponse));
user_info({Error, What}) ->
    {Error, What}.

init_auth(_Auth, _Urls, {_Error, Message}) ->
    {stop, Message};
init_auth(Auth, Urls, UserData) ->
    {ok, #state{auth=Auth, urls=Urls, user=UserData}}.

handle_call({next_page, Url}, _From, State) ->
    {reply, request_url(get, Url), State};
handle_call({member_info, MemberId}, _From, State) ->
    {reply, meetup_call(State, members, [{member_id, MemberId}]), State};
handle_call({rsvps, EventId}, _From, State) ->
    {reply, meetup_call(State, rsvps, [{event_id,EventId}]), State};
handle_call({event_info, EventId}, _From, State) ->
    {reply, meetup_call(State, event_info, [], [EventId]), State};
handle_call({groups, {search_text, Search}}, _From, State) ->
    {reply, meetup_call(State, find_groups, [{text, Search}, {radius, "global"}]), State};
handle_call({groups, {params, Params}}, _From, State) ->
    {reply, meetup_call(State, find_groups, Params), State};
handle_call({members, GroupId}, _From, State) ->
    {reply, meetup_call(State, members, [{group_id, GroupId}]), State};
handle_call({group_info, GroupId}, _From, State) ->
    {reply, meetup_call(State, groups, [{group_id, GroupId}]), State};
handle_call({events, {group_id, GroupId}}, _From, State) ->
    {reply, meetup_call(State, events, [{group_id, GroupId}]), State};
handle_call({events, {text, Topic}}, _From, State) ->
    {reply, meetup_call(State, open_events, [{text, Topic}]), State};
handle_call({events, {member_id, authorized}}, _From, State) ->
    MemberId = proplists:get_value(<<"id">>, State#state.user),
    {reply, meetup_call(State, events, [{member_id, MemberId}]), State};
handle_call({events, {member_id, MemberId}}, _From, State) ->
    {reply, meetup_call(State, events, [{member_id, MemberId}]), State};
handle_call({groups, {member_id, authorized}}, _From, State) ->
    MemberId = proplists:get_value(<<"id">>, State#state.user),
    {reply, meetup_call(State, groups, [{member_id, MemberId}]), State};
handle_call({groups, {member_id, MemberId}}, _From, State) ->
    {reply, meetup_call(State, groups, [{member_id, MemberId}]), State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_X, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-record(url, {
          url,
          method=get,
          args=[]
          }).

-type url() :: #url{}.


request_url(Method, Url) ->
    io:format("Requesting: ~s~n", [Url]),
    check_http_results(
      httpc:request(Method, {Url, [{"Accept-Charset", "utf-8"}]},
                    [{autoredirect, false}], [{body_format, binary}]),
      fun(X) -> parse_response(X) end).


%% Use meetup_call/3 if the API call involves query parameters with no
%% URL modifications
-spec meetup_call(state(), atom(), list()) -> any().
meetup_call(State, What, UrlArgs) ->
    UrlDetails = proplists:get_value(What, State#state.urls),
    make_request(UrlDetails#url.method,
                 {url, UrlDetails#url.url, UrlArgs ++ UrlDetails#url.args},
                 State#state.auth
                ).

%% Use meetup_call/4 if there are strings to change in the body of the URL
-spec meetup_call(state(), atom(), list(), list()) -> any().
meetup_call(State, What, UrlArgs, Append) ->
    UrlDetails = proplists:get_value(What, State#state.urls),
    Url = io_lib:format(UrlDetails#url.url, Append),
    make_request(UrlDetails#url.method,
                 {url, Url, UrlArgs ++ UrlDetails#url.args},
                 State#state.auth
                ).

-spec meetup_urls() -> list(url()).
meetup_urls() ->
    [ { members, #url{url=?BASE_URL("2/members")} },
      { verify_creds, #url{url=?BASE_URL("2/members"),
                           args=[{"member_id", "self"}] } },
      { groups, #url{url=?BASE_URL("2/groups")} },
      { events, #url{url=?BASE_URL("2/events")} },
      { open_events, #url{url=?BASE_URL("2/open_events")} },
      { find_groups, #url{url=?BASE_URL("find/groups")} },
      { event_info, #url{url=?BASE_URL("2/event/~s")} },
      { rsvps, #url{url=?BASE_URL("2/rsvps")} }
    ].

make_request(HttpMethod, {url, Url, UrlArgs}, #auth{type=apikey, apikey=ApiKey}) ->
    FullUrl = oauth:uri(Url, [{key, ApiKey}|UrlArgs]),
    request_url(HttpMethod, FullUrl).

%%
%% There are two possibilities depending on the API call:
%%   * Old API: there's no dedicated metadata block in the body, so
%%              the body is just a list of entries
%%   * v2 API : dedicated metadata, dedicated results list
%%
%% If the 2nd argument to this is undefined, this is the old API.
%%
%% To retrieve the next link (for paging) from the headers we have to
%% be careful, because both the prev and next links are given the same
%% header name, and thus Headers is not useful as an Erlang proplist
%% for the links.
%%
%% Will return a list with 3 keys: next, count, and results
%% next will be an empty list (or empty binary) if there is no next link
extract_meta(Headers, undefined, Results) ->
    [ { next, unicode:characters_to_list(find_next_link(Headers)) },
      { count, length(Results) },
      { results, Results } ];
extract_meta(_Headers, MetaData, Results) ->
    [ { next, proplists:get_value(<<"next">>, MetaData) },
      { count, proplists:get_value(<<"count">>, MetaData) },
      { results, proplists:get_value(<<"results">>, Results) } ].

-spec check_http_results(tuple(), fun()) -> any().
check_http_results({ok, {{_HttpVersion, 200, _StatusMsg}, Headers, Body}}, Fun) ->
    ParsedJson = Fun(Body),
    {ok, extract_meta(Headers, proplists:get_value(<<"meta">>, ParsedJson), ParsedJson) };
check_http_results({ok, {{_HttpVersion, _Status, StatusMsg}, _Headers, Body}}, _Fun) ->
    {error, extract_error_message(StatusMsg, Body) };
check_http_results(Other, _Fun) ->
    {unknown, Other}.

%%
%% Scan headers list looking for something like:
%% {"link",
%%  "<https://api.meetup.com/find/groups?page=3&offset=2&category=34>; rel=\"next\""},
find_next_link([]) ->
    "";
find_next_link([{"link", Header}|T]) ->
    case re:run(Header, "<(http.*)>;\\s*rel=.next") of
        {match, Matches} ->
            {Start, Len} = lists:nth(2, Matches),
            string:sub_string(Header, Start + 1, Start + Len);
        _ ->
            find_next_link(T)
    end;
find_next_link([_H|T]) ->
    find_next_link(T).


%% Meetup does not appear to reliably provide error details in the
%% body of a failed request, so just return the status message
-spec extract_error_message(string(), string()) -> string().
extract_error_message(HttpStatusMsg, _Body) ->
    HttpStatusMsg.


-spec parse_response(binary() | string()) -> any().
parse_response(JSON) when is_binary(JSON) ->
    jsx:decode(JSON);
parse_response(JSON) ->
    jsx:decode(unicode:characters_to_binary(JSON)).
