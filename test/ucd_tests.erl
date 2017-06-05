-module(ucd_tests).
-include_lib("eunit/include/eunit.hrl").


invalid_fun_test() ->
  ?assertMatch({error, [{_, "invalid UCD function: x"}]},
               compile(["-module(x)."
                       ,"f() -> ucd:x()."
                       ])).



compile(Code) ->
    Forms = [erl_syntax:revert(F) || F <- merl:quote(1, Code)],
    Options = [ {parse_transform, ucd}
              , return
              , binary
              , export_all
              , nowarn_export_all
              , warnings_as_errors
              ],
    case compile:noenv_forms(Forms, Options) of
        {error, [{_, Errors}], []} ->
            {error, [{Loc, Mod:format_error(E)} || {Loc, Mod, E} <- Errors]};
        Other ->
            Other
    end.
