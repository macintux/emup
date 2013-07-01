-record(em_person, {
          id,
          name,
          location,
          aliases=[] %% LinkedIn, Twitter, Flickr, Tumblr
         }).

-type em_person() :: #em_person{}.

-record(em_location, {
          city,
          state,   %% Will be undefined for most countries
          country,
          lat,
          lon
         }).

-type em_location() :: #em_location{}.

-record(em_venue, {
          id,
          name
         }).

-type em_venue() :: #em_venue{}.

-record(em_event, {
          id,           %% binary string
          status,       %% binary string
          name,
          description,
          start,        %% Erlang datetime tuple representing local start time
          duration,     %% integer, seconds
          utc_offset,   %% integer, time zone adjustment
          headcount,    %% integer, only useful during/after
          rsvp,         %% integer, "Yes" tally
          rsvp_limit,   %% integer
          url,          %% 'event_url'
          group_id,
          group_name,
          venue,
          location      %% Taken from group information if venue is undefined
         }).

-type em_event() :: #em_event{}.

-record(em_group, {
          id,
          name,
          description,
          url,  %% 'link' field
          created, %% epoch
          member_count,
          organizer,
          location,
          member_label,
          topics=[]
         }).

-type em_group() :: #em_group{}.

%% Events and groups come back from the Meetup API + JSON parser
%% as a list of results as proplists (API v1) or (v2) a list of two
%% tuples, with 'meta' and 'results' as labels.

%% @doc Any individual object (person, event, group) is a proplist
-type em_raw_object() :: [tuple()].

%% @doc Oh, Erlang date handling, why must thou be such a PITA? These
%% are the short version of dates, just {YYYY, MM, DD}.
-type sdate() :: {integer(), integer(), integer()}.

%% @doc Long dates: {{YYYY, MM, DD}, {HH, MM, SS}}
-type ldate() :: {sdate(), {integer(), integer(), integer()}}.

%% @doc API result from emup_api:
%%   * next is a URL for the next page of results
%%   * count is the number of results for this query
%%   * results is the list of JSON proplists
-type api_result() :: list(tuple()).
