%%% @doc Main entry point for the rebar3 rebar3_dynamic_plugin plugin.
-module(rebar3_dynamic_plugin).

-export([init/1]).

-ignore_xref([init/1]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_dynamic_plugin_prv:init(State),
    {ok, State1}.
