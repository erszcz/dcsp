-module(dcsp).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% User API
-export([start/0,
         poke/0,
         get_pokes/0]).

%% -------------------------------------------------------------------
%% Application callbacks
%% -------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    dcsp_sup:start_link().

stop(_State) ->
    ok.

%% -------------------------------------------------------------------
%% User API
%% -------------------------------------------------------------------

start() ->
    application:start(dcsp).

poke() ->
    dcsp_srv:poke(dcsp_srv).

get_pokes() ->
    dcsp_srv:get_pokes(dcsp_srv).
