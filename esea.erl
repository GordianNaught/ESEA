-module(esea).
-behavior(gen_server).
-export([
  init/1,
  handle_call/3
  % handle_cast/2,
  % terminate/1
]).
-export([
  start_link/1,
  start/1,
  get_account_info/1,
  get_balance/1,
  list_images/1,
  list_droplets/1,
  make_droplet/3,
  make_droplets/3,
  delete_droplet/2,
  power_cycle_droplet/2,
  reboot_droplet/2,
  get_droplet_info/2
]).

-export([
  minimal_droplet_options/0,
  make_minimal_droplet/2,
  make_minimal_droplets/2
]).

% behavior implementation
init(#{token := Token}) ->
  {ok, #{token => Token, url => "https://api.digitalocean.com/v2"}}.
request_options() -> [{ssl, [{verify, verify_none}]}].
get_response_body({Status,
                   {{Version, StatusCode, ReasonPhrase},
                    Headers,
                    Body}}) ->
  #{status => Status,
    version => Version,
    status_code => StatusCode,
    reason_phrase => ReasonPhrase,
    headers => Headers,
    body => jjson:deserialize(Body)}.
standard_headers(#{token := Token}) ->
  [
    {"Content-Type", "application/json"},
    {"Authorization", lists:append(["Bearer ", Token])}
  ].

handle_call(list_images, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/images"]),
  Headers = standard_headers(State),
  Response = httpc:request(get, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call(get_account_info, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/account"]),
  Headers = standard_headers(State),
  Response = httpc:request(get, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call(get_balance, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/customers/my/balance"]),
  Headers = standard_headers(State),
  Response = httpc:request(get, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call(list_droplets, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/droplets"]),
  Headers = standard_headers(State),
  Response = httpc:request(get, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({make_droplet, Name, DropletInfo}, _From, State) ->
  #{url := URL} = State,
  #{
    region := Region,
    size := Size,
    image := Image
  } = DropletInfo,
  Headers = standard_headers(State),
    %[{"Authorization", lists:append(["Bearer ", Token])}],
  Body = jjson:serialize(#{
           name => Name,
           region => Region,
           size => Size,
           image => Image
         }),
  FullURL = lists:append([URL, "/droplets"]),
  Request = { FullURL, Headers, "application/json", Body},
  Response = httpc:request(post, Request, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({make_droplets, Names, DropletInfo}, _From, State) ->
  #{url := URL} = State,
  #{
    region := Region,
    size := Size,
    image := Image
  } = DropletInfo,
  Headers = standard_headers(State),
  Body = jjson:serialize(#{
           names => Names,
           region => Region,
           size => Size,
           image => Image
         }),
  FullURL = lists:append([URL, "/droplets"]),
  Request = { FullURL, Headers, "application/json", Body},
  Response = httpc:request(post, Request, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({delete_droplet, DropletID}, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append(
              [URL, "/droplets/", integer_to_list(DropletID)]),
  Headers = standard_headers(State),
  Response = httpc:request(delete, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({power_cycle_droplet, DropletID}, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/droplets/", DropletID, "/actions"]),
  Headers = lists:append(standard_headers(State),
                         [{"type", "power_cycle"}]),
  Response = httpc:request(post, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({reboot_droplet, DropletID}, _From, State) ->
  #{url := URL, token := Token} = State,
  FullURL = lists:append([URL, "/droplets/", DropletID, "/actions"]),
  Response = httpc:request(post, {FullURL, [{"Authorization", Token},
                                            {"type", "reboot"}
                                           ]}, request_options(), []),
  {reply, get_response_body(Response), State};
handle_call({get_droplet_info, DropletID}, _From, State) ->
  #{url := URL} = State,
  FullURL = lists:append([URL, "/droplets/", DropletID]),
  Headers = lists:append(standard_headers(State),
                         [{"type", "reboot"}]),
  Response = httpc:request(get, {FullURL, Headers}, request_options(), []),
  {reply, get_response_body(Response), State}.

% client functions
start_link(Token) -> gen_server:start_link(esea, #{token => Token}, []).
start(Token) -> gen_server:start(esea, #{token => Token}, []).
get_account_info(Client) -> gen_server:call(Client, get_account_info).
get_balance(Client) -> gen_server:call(Client, get_balance).
list_images(Client) -> gen_server:call(Client, list_images).
list_droplets(Client) -> gen_server:call(Client, list_droplets).
make_droplet(Client, Name, Options) -> gen_server:call(Client, {make_droplet, Name, Options}).
make_droplets(Client, Names, Options) -> gen_server:call(Client, {make_droplets, Names, Options}).
delete_droplet(Client, DropletID) -> gen_server:call(Client, {delete_droplet, DropletID}).
power_cycle_droplet(Client, DropletID) -> gen_server:call(Client, {power_cycle_droplet, DropletID}).
reboot_droplet(Client, DropletID) -> gen_server:call(Client, {reboot_droplet, DropletID}).
get_droplet_info(Client, DropletID) -> gen_server:call(Client, {get_droplet_info, DropletID}).

minimal_droplet_options() -> #{region => "nyc3", size => "s-1vcpu-1gb", image => "Fedora-33-x64"}.
make_minimal_droplet(Client, Name) -> make_droplet(Client, Name, minimal_droplet_options()).
make_minimal_droplets(Client, Names) -> make_droplets(Client, Names, minimal_droplet_options()).
