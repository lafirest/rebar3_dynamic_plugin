# rebar3 dynamic plugin

A plugin which directly passthrough the rebar3 provider call to a specific script,
you can extend rebar3 plugin with any Erlang script file.

## Build

```sh
$ rebar3 compile
```

## Use

For example,, Add the plugin to your rebar config:

```erlang
{project_plugins, [rebar3_dynamic_plugin]}.

%% A script to call
{dynamic_plugin_script, "scripts/sdk_checking.erl"}.

{provider_hooks, [
    {pre, [
        {compile, project_plugins} %% call the above script before compile
    ]}
]}.
```
