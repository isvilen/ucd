-module(ucd).
-export([parse_transform/2, format_error/1]).

-include_lib("syntax_tools/include/merl.hrl").


parse_transform(Forms, _Opts) ->
    {Forms1, Ctx} = collect_ucd_funs(Forms),
    erl_syntax:revert_forms(Forms1 ++ ucd_ast:forms(Ctx)).


format_error(Error) ->
    lists:flatten(Error).


collect_ucd_funs(Forms) ->
    File  = get_file(Forms),
    Fun = fun (Form, Ctx) -> collect_ucd_funs(File, Form, Ctx) end,
    lists:mapfoldr(Fun, ucd_ast:context(), Forms).


collect_ucd_funs(File, Form, Ctx0) ->
    Fun = fun (AST, Ctx) ->
              case AST of
                  ?Q("ucd:'@Fun'(_@@Args)") ->
                      collect_ucd_fun(File, Form, Fun, Args, Ctx);
                  _ ->
                      {AST, Ctx}
              end
          end,
    erl_syntax_lib:mapfold(Fun, Ctx0, Form).


collect_ucd_fun(File, Form, Fun, Args, Ctx) ->
    case ucd_ast:add(erl_syntax:concrete(Fun), Args, Ctx) of
        {ok, AST1, Ctx1} ->
            {AST1, Ctx1};
        {error, Error} ->
            Ln = erl_syntax:get_pos(Form),
            throw({error, [{File, [{Ln, ?MODULE, Error}]}], []})
    end.


get_file([]) ->
    "undefined";

get_file([Form | Forms]) ->
    case Form of
        ?Q("-file(\"'@File\",9090).") -> erl_syntax:string_value(File);
        _                             -> get_file(Forms)
    end.
