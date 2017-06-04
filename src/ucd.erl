-module(ucd).
-export([parse_transform/2, format_error/1]).


parse_transform(Forms, _Opts) ->
    Forms.


format_error({Fmt, Args}) ->
    lists:flatten(io_lib:format(Fmt, Args)).
