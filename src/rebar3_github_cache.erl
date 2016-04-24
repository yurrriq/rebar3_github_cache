-module(rebar3_github_cache).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Resource = {github, rebar_github_resource},
  {ok, rebar_state:add_resource(State, Resource)}.
