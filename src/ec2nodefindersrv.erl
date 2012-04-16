%% @hidden
%% @doc ec2-describe-instances based node discovery service.
%% @end

-module (ec2nodefindersrv).
-behaviour (gen_server).
-export ([ start_link/4, discover/0, get_group/0 ]).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).

-record (state, { group,
                  ping_timeout,
                  access,
                  secret
                  }).
-define(APIVERSION, "2012-03-01").
-define(ENDPOINT, "ec2.amazonaws.com").

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Group, PingTimeout, Access, Secret)
  when is_list (Group),
       is_integer (PingTimeout),
       is_list (Access),
       is_list (Secret) ->
  gen_server:start_link 
    ({ local, ?MODULE }, 
     ?MODULE, 
     [ Group, PingTimeout, Access, Secret ], 
     []).

discover () ->
    gen_server:call (?MODULE, discover, 120000).

get_group() ->
    gen_server:call (?MODULE, get_group, 5000).


%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

init ([ Group, PingTimeout, Access, Secret  ]) ->
  pong = net_adm:ping (node ()), % don't startup unless distributed

  process_flag (trap_exit, true),
  State = #state{ group = Group,
                  ping_timeout = PingTimeout,
                  access = Access,
                  secret = Secret },
  discover (State),
  { ok, State }.

handle_call (discover, _From, State) -> 
    { reply, { ok, discover (State) }, State };
handle_call (get_group, _From, State) ->
    { reply, { ok, State#state.group }, State };
handle_call (_Request, _From, State) -> 
    { noreply, State }.


handle_cast (_Request, State) -> { noreply, State }.

handle_info (_Msg, State) -> { noreply, State }.

terminate (_Reason, _State) -> ok.

code_change (_OldVsn, State, _Extra) -> { ok, State }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

async (Fun, Timeout) ->
  Me = self (),
  Ref = make_ref (),
  spawn (fun () ->
           { ok, _ } = timer:kill_after (Timeout),
           Me ! { Ref, Fun () }
         end),

  Ref.

collect (Key, Timeout) ->
  receive
    { Key, Status } -> Status
  after Timeout ->
    timeout
  end.

discover (State) ->    
  Group = State#state.group,
  Timeout = State#state.ping_timeout,
  Access = State#state.access,
  Secret = State#state.secret,
  GroupHosts = awssign:describe_instances(Group, ?ENDPOINT, ?APIVERSION, Access, Secret),
  error_logger:info_report([{ec2nodefinder,discover},{group,Group},{hosts,GroupHosts}]),
  [ { Node, collect (Key2, Timeout) } ||
    { Node, Key2 } <- 
      [ { Node, start_ping (Node, Timeout) } ||
        { Host, { ok, NamesAndPorts } } <- 
          [ { Host, collect (Key, Timeout) } ||
            { Host, Key } <- [ { Host, start_names (Host, Timeout) } 
                            || Host <- GroupHosts ] ],
        { Name, _ } <- NamesAndPorts,
      Node <- [ a(Name ++ "@" ++ Host) ] ] ].



start_names (Host, Timeout) ->
  async (fun () -> net_adm:names (a(Host)) end, Timeout).

start_ping (Node, Timeout) ->
  async (fun () -> net_adm:ping (a(Node)) end, Timeout).

a(Name) when is_atom(Name) -> Name;
a(Name) when is_list(Name) -> list_to_atom(Name).
