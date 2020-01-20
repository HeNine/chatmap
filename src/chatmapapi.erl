%%%-------------------------------------------------------------------
%%% @author henine
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2016 12:01
%%%-------------------------------------------------------------------
-module(chatmapapi).
-author("henine").

-include("../include/yaws_api.hrl").
-include("../include/yaws.hrl").

-include("../include/chatizen.hrl").

%% API
-export([out/1, start/1]).

start(Sconf) ->
  io:format("~w~n", [Sconf#sconf.opaque]),
  {"database_file", DatabaseFile} = lists:keyfind("database_file", 1, Sconf#sconf.opaque),
  spawn(
    fun() ->
      {ok, _Pid} = chatmapdb_sup:start_link(DatabaseFile),
      receive after infinity -> ok end
    end).

out(Args) ->
  Cookies = lists:merge([yaws_api:parse_cookie(Cookie) || Cookie <- Args#arg.headers#headers.cookie]),
%%  io:format("~w~n", [Cookies]),
  Password = case lists:keyfind("password", #cookie.key, Cookies) of
               false -> false;
               #cookie{value = Pass} -> Pass
             end,
  if
    Password =:= "<write_password>" ->
      handle_call(Args, Args#arg.pathinfo, Args#arg.req#http_request.method, allow_rw);
    Password =:= "sadmcasldkfjsdclasdkcmascdmklasdm" ->
      handle_call(Args, Args#arg.pathinfo, Args#arg.req#http_request.method, allow_r);
    true -> {status, 403}
  end.

handle_call(_Args, "/chatizen/" ++ Name, 'GET', Permissions) when Permissions == allow_rw; Permissions == allow_r ->
  case validate_name(Name) of
    invalid_name -> {status, 400};
    Name ->
      case chatmapdb:get(Name) of
        notfound -> {status, 404};
        Chatizen ->
          {content,
            "application/json",
            json2:encode(
              {struct, [
                {"name", Chatizen#chatizen.name},
                {"lat", Chatizen#chatizen.lat},
                {"lon", Chatizen#chatizen.lon}
              ]})}
      end
  end;

handle_call(_Arge, "/chatizen", 'GET', Permissions) when Permissions == allow_rw; Permissions == allow_r ->
  All = chatmapdb:all(),
  AllJSON = json2:encode(
    [{struct, [
      {"name", Chatizen#chatizen.name},
      {"lat", Chatizen#chatizen.lat},
      {"lon", Chatizen#chatizen.lon}
    ]} || Chatizen <- All]
  ),
  {content, "application/json", AllJSON};


handle_call(Args, "/chatizen/" ++ Name, 'POST', Permissions) when Permissions == allow_rw ->
  case validate_name(Name) of
    invalid_name -> {status, 400};
    Name ->
      case chatmapdb:add(parse_chatizen(binary:bin_to_list(Args#arg.clidata), Name)) of
        ok -> {status, 204}
      end
  end;

handle_call(_Args, "/chatizen/" ++ Name, 'DELETE', Permissions) when Permissions == allow_rw ->
  case validate_name(Name) of
    invalid_name -> {status, 400};
    Name ->
      case chatmapdb:remove(Name) of
        ok -> {status, 204}
      end
  end;

handle_call(_Args, _, _, _) ->
  {status, 404}.

validate_name(Name) ->
  case re:run(Name, "^[a-z_\\-\\[\\]\\\\^{}|`][a-z0-9_\\-\\[\\]\\\\^{}|`]*$", [{capture, none}]) of
    match -> Name;
    nomatch -> invalid_name
  end.

parse_chatizen(ChatizenJSON, Name) ->
  {ok, {struct, [{"lat", Latitude}, {"lon", Longitude}]}} = json2:decode_string(ChatizenJSON),
  #chatizen{name = Name, lat = Latitude, lon = Longitude}.
