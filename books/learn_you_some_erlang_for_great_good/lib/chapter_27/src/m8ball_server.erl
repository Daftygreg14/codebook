-module(m8ball_server).
-behavior(gen_server).
-author("piotr").

%% API
-export([start_link/0, stop/0, ask/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call({global, ?MODULE}, stop).

ask(_Question) ->
  gen_server:call({global, ?MODULE}, question).

%% Callbacks

init([]) ->
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  rand:seed(A,B,C),
  {ok, []}.

handle_call(question, _From, State) ->
  {ok, Answers} = application:get_env(m8ball, answers),
  Answer = element(rand:uniform(tuple_size(Answers)), Answers),
  {reply, Answer, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
  {noreply, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.



