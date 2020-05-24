-module(sni_elli_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> sni_elli_sup:start_link().

stop(_State) -> ok.
