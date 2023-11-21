%%% @doc Plugin provider for rebar3 rebar3_dynamic_plugin.
-module(rebar3_dynamic_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, dynamic_plugin).
-define(DEPS, []).
-define(OPTS, [{task, undefined, "undefined", string, "Erlang script file"}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        % The 'user friendly' name of the task
        providers:create([
            {name, ?PROVIDER},
            % The module implementation of the task
            {module, ?MODULE},
            % The task can be run by the user, always true
            {bare, true},
            % The list of dependencies
            {deps, ?DEPS},
            % How to use the plugin
            {example, "rebar3 dynamic_plugin SCRIPT"},
            % list of options understood by the plugin
            {opts, ?OPTS},
            {short_desc, "rebar3 dynamic plugin"},
            {desc,
                "A plugin which directly passthrough the rebar3 provider call to a specific script"}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case parse_opts(State) of
        #{task := Script} ->
            do_script(State, Script);
        _ ->
            {ok, State}
    end.

do_script(State, Script) ->
    case compile:file(Script) of
        {ok, Mod} ->
            do_call(State, Mod);
        error ->
            {error, format_error("compile failed")};
        {error, Errors, Warnings} ->
            {error, io_lib:format("compile failed, errors:~p~n, warning.~p~n", [Errors, Warnings])}
    end.

do_call(State, Mod) ->
    try Mod:main(State) of
        ok ->
            {ok, State};
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, format_error(Reason)}
    catch
        _:Error ->
            {error, Error}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec parse_opts(rebar_state:t()) -> maps:map().
parse_opts(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case Args of
        [] ->
            find_script(State);
        _ ->
            maps:from_list(Args)
    end.

find_script(State) ->
    case rebar_state:get(State, dynamic_plugin_script, undefined) of
        undefined ->
            #{};
        Script ->
            find_script_path(State, Script)
    end.

find_script_path(State, Script) ->
    App = rebar_state:current_app(State),
    Dir = rebar_app_info:dir(App),
    Path = filename:join(Dir, Script),
    #{script => Path}.
