-record(em_person, {
          id,
          name,
          location,
          aliases=[] %% LinkedIn, Twitter, Flickr, Tumblr
         }).

-record(em_location, {
          city,
          state,   %% Will be undefined for most countries
          country,
          lat,
          lon
         }).

-record(em_venue, {
          id,
          name
         }).

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
