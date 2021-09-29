-module(main).
-export([
  init/0,
  read_token/0,
  %fix/0,
  main/0,
  start/1
]).
init() ->
  ok = inets:start(),
  ok = ssl:start().
start(Name) ->
  {ok, Session} = esea:start_link(read_token()),
  register(Name, Session).
read_token() ->
  {ok, TokenFileContents} = file:read_file("token.txt"),
  erlang:binary_to_list(string:trim(TokenFileContents)).

% If I recall correctly, this is a hack to make things work on
% older versions of Erlang.
%
%fix() -> ssl:filter_cipher_suites(
           %ssl:cipher_suites(default, TLSVersion),  
           %[{cipher, fun (chacha20_poly1305) -> false;
                     %(_) -> true
                     %end}]).
main() ->
  init(),
  start(s),
  #{status := ok, body := #{account := AccountInfo}} = esea:get_account_info(s),
  % #{status := ok, body := DropletResponse} = esea:delete_droplet(s, 266118713),
  %#{status := ok, body := MakeResp} = esea:make_minimal_droplet(s, thisname),
  %#{status := ok, body := Droplets} = esea:list_droplets(s),
  #{status := ok, body := #{droplets := Droplets, links := Links, meta := Meta}} = esea:list_droplets(s),
  #{status := ok, body := #{images := Images}} = esea:list_images(s),
  #{status := ok, body := Balance} = esea:get_balance(s),
  Results = #{
    account => AccountInfo,
    droplets => #{ droplets => lists:map(fun (I) -> maps:get(name, I) end, Droplets),
                   links => Links,
                   meta => Meta },
    images => lists:map(fun (I) -> maps:get(description, I) end, Images),
    balance => Balance
   },
  io:format("~p", [Results]).
    %droplet_response => DropletResponse,
 
