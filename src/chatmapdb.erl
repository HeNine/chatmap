%%%-------------------------------------------------------------------
%%% @author henine
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2016 12:35
%%%-------------------------------------------------------------------
-module(chatmapdb).
-author("henine").

-behaviour(gen_server).

-include("../include/chatizen.hrl").

%% API
-export([get/1, add/1, remove/1, start_link/1, all/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(string) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(DatabaseFile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [DatabaseFile], []).

add(Chatizen) ->
  gen_server:call(?SERVER, {add, Chatizen}).

all() ->
  gen_server:call(?SERVER, all).

remove(ChatizenName) ->
  gen_server:call(?SERVER, {remove, ChatizenName}).

get(ChatizenName) ->
  case gen_server:call(?SERVER, {get, ChatizenName}) of
    [Chatizen] -> Chatizen;
    [] -> notfound;
    Error -> Error
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(DatabaseFile) ->
  {ok, chatmapdb_handle} = dets:open_file(chatmapdb_handle, [{file, DatabaseFile}, {keypos, #chatizen.name}]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({add, Chatizen}, _From, State) ->
  {reply, dets:insert(chatmapdb_handle, Chatizen), State};

handle_call({remove, ChatizenName}, _From, State) ->
  {reply, dets:match_delete(chatmapdb_handle, #chatizen{name = ChatizenName, lat = '_', lon = '_'}), State};

handle_call({get, ChatizenName}, _From, State) ->
  {reply, dets:match_object(chatmapdb_handle, #chatizen{name = ChatizenName, lat = '_', lon = '_'}), State};

handle_call(all, _From, State) ->
  {reply, dets:match_object(chatmapdb_handle, #chatizen{name = '_', lat = '_', lon = '_'}), State};

handle_call(_Request, _From, State) ->
  {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok = dets:sync(chatmapdb_handle),
  ok = dets:close(chatmapdb_handle),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
