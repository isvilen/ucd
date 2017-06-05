-module(ucd_ast).
-export([context/0, add/3, forms/1]).


context() ->
    [].


add(Fun, _Args, _Ctx) ->
    {error, io_lib:format("invalid UCD function: ~s", [Fun])}.


forms(_Ctx) ->
    [].
