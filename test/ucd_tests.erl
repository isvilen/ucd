-module(ucd_tests).
-include_lib("eunit/include/eunit.hrl").


invalid_fun_test() ->
  ?assertMatch({error, [{_, "invalid UCD function: x"}]},
               compile(["-module(x)."
                       ,"f() -> ucd:x()."
                       ])).



codepoint_data_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_data)."
                  ,"category(CP) -> ucd:category(CP)."
                  ,"combining_class(CP) -> ucd:combining_class(CP)."
                  ,"bidi_class(CP) -> ucd:bidi_class(CP)."
                  ,"decomposition(CP) -> ucd:decomposition(CP)."
                  ,"numeric(CP) -> ucd:numeric(CP)."
                  ,"bidi_mirrored(CP) -> ucd:bidi_mirrored(CP)."
                  ,"bidi_mirroring(CP) -> ucd:bidi_mirroring(CP)."
                  ,"uppercase(CP) -> ucd:uppercase(CP)."
                  ,"lowercase(CP) -> ucd:lowercase(CP)."
                  ,"titlecase(CP) -> ucd:titlecase(CP)."
                  ,"special_uppercase(CP) -> ucd:special_uppercase(CP)."
                  ,"special_lowercase(CP) -> ucd:special_lowercase(CP)."
                  ,"special_titlecase(CP) -> ucd:special_titlecase(CP)."
                  ,"common_case_folding(CP) -> ucd:common_case_folding(CP)."
                  ,"simple_case_folding(CP) -> ucd:simple_case_folding(CP)."
                  ,"full_case_folding(CP) -> ucd:full_case_folding(CP)."
                  ,"turkic_case_folding(CP) -> ucd:turkic_case_folding(CP)."
                  ,"east_asian_width(CP) -> ucd:east_asian_width(CP)."
                  ,"composition_exclusion(CP) -> ucd:composition_exclusion(CP)."
                  ,"full_composition_exclusion(CP) -> ucd:full_composition_exclusion(CP)."
                  ,"changes_when_nfkc_casefolded(CP) -> ucd:changes_when_nfkc_casefolded(CP)."
                  ,"nfkc_casefold(CP) -> ucd:nfkc_casefold(CP)."
                  ,"nfd_quick_check(CP) -> ucd:nfd_quick_check(CP)."
                  ,"nfc_quick_check(CP) -> ucd:nfc_quick_check(CP)."
                  ,"nfkd_quick_check(CP) -> ucd:nfkd_quick_check(CP)."
                  ,"nfkc_quick_check(CP) -> ucd:nfkc_quick_check(CP)."
                  ,"line_break(CP) -> ucd:line_break(CP)."
                  ,"grapheme_break_property(CP) -> ucd:grapheme_break_property(CP)."
                  ,"sentence_break_property(CP) -> ucd:sentence_break_property(CP)."
                  ,"word_break_property(CP) -> ucd:word_break_property(CP)."
                  ,"hangul_syllable_type(CP) -> ucd:hangul_syllable_type(CP)."
                  ,"block(CP) -> ucd:block(CP)."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertMatch('So', ucd_data:category(16#33FF))
  , ?_assertMatch('Lo', ucd_data:category(16#3400))
  , ?_assertMatch('So', ucd_data:category(16#4DC0))
  , ?_assertMatch('Cn', ucd_data:category(16#E0080))

  , ?_assertMatch(230, ucd_data:combining_class(16#599))
  , ?_assertMatch(222, ucd_data:combining_class(16#59A))
  , ?_assertMatch(220, ucd_data:combining_class(16#59B))
  , ?_assertMatch(0, ucd_data:combining_class(16#E0080))

  , ?_assertMatch('NSM', ucd_data:bidi_class(16#074A))
  , ?_assertMatch('AL', ucd_data:bidi_class(16#074B))
  , ?_assertMatch('L', ucd_data:bidi_class(16#E0080))

  , ?_assertMatch({compat, [16#0E4D ,16#0E32]}, ucd_data:decomposition(16#0E33))
  , ?_assertMatch([16#4359], ucd_data:decomposition(16#2F974))
  , ?_assertMatch(undefined, ucd_data:decomposition(16#E0080))

  , ?_assertMatch({decimal,1}, ucd_data:numeric(16#0E51))
  , ?_assertEqual({decimal, 9}, ucd_data:numeric(16#0F29))
  , ?_assertEqual({numeric, {1,2}}, ucd_data:numeric(16#0F2a))
  , ?_assertEqual({numeric, {17,2}}, ucd_data:numeric(16#0F32))
  , ?_assertEqual({numeric, {-1,2}}, ucd_data:numeric(16#0F33))
  , ?_assertEqual({decimal, 0}, ucd_data:numeric(16#1040))
  , ?_assertEqual({digit, 0}, ucd_data:numeric(16#2070))
  , ?_assertEqual({k_other_numeric, 5}, ucd_data:numeric(16#3405))
  , ?_assertEqual({k_primary_numeric, 10000}, ucd_data:numeric(16#4E07))
  , ?_assertEqual({k_accounting_numeric, 1000}, ucd_data:numeric(16#4EDF))
  , ?_assertEqual({k_primary_numeric, 1000000000000}, ucd_data:numeric(16#5146))
  , ?_assertEqual({k_primary_numeric, 100}, ucd_data:numeric(16#767E))
  , ?_assertEqual({k_accounting_numeric, 10000}, ucd_data:numeric(16#842C))
  , ?_assertEqual({k_other_numeric, 30}, ucd_data:numeric(16#20983))
  , ?_assertEqual({k_other_numeric, 40}, ucd_data:numeric(16#2098C))
  , ?_assertEqual({k_other_numeric, 4}, ucd_data:numeric(16#2626D))
  , ?_assertEqual(undefined, ucd_data:numeric($a))
  , ?_assertMatch(undefined, ucd_data:numeric(16#E0080))

  , ?_assertMatch(true, ucd_data:bidi_mirrored($[))
  , ?_assertMatch(false, ucd_data:bidi_mirrored(16#E0080))

  , ?_assertMatch($], ucd_data:bidi_mirroring($[))

  , ?_assertEqual($i, ucd_data:lowercase($I))
  , ?_assertEqual($I, ucd_data:uppercase($i))
  , ?_assertEqual([16#1F08, 16#0399], ucd_data:uppercase(16#1F80))
  , ?_assertEqual([16#03A1, 16#0313], ucd_data:titlecase(16#1FE4))

  , ?_assertEqual([{[<<"az">>], 16#0069}, {[<<"tr">>], 16#0069}],
                  ucd_data:special_lowercase(16#0130))
  , ?_assertEqual([{[<<"lt">>,more_above], [16#012F, 16#0307]}],
                  ucd_data:special_lowercase(16#012E))

  , ?_assertEqual(16#0069, ucd_data:common_case_folding(16#0049))
  , ?_assertEqual(16#0131, ucd_data:turkic_case_folding(16#0049))

  , ?_assertEqual(16#03C9, ucd_data:common_case_folding(16#2126))
  , ?_assertEqual(16#1FF3, ucd_data:simple_case_folding(16#1FFC))
  , ?_assertEqual([16#0073, 16#0073], ucd_data:full_case_folding(16#00DF))

  , ?_assertMatch(narrow, ucd_data:east_asian_width(16#0020))
  , ?_assertMatch(narrow, ucd_data:east_asian_width(16#00A2))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#0000))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#007F))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#2011))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#25E6))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#a4D0))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#FFF9))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#1F890))
  , ?_assertMatch(neutral, ucd_data:east_asian_width(16#E0001))
  , ?_assertMatch(ambiguous, ucd_data:east_asian_width(16#00A1))
  , ?_assertMatch(ambiguous, ucd_data:east_asian_width(16#2013))
  , ?_assertMatch(ambiguous, ucd_data:east_asian_width(16#25E2))
  , ?_assertMatch(ambiguous, ucd_data:east_asian_width(16#E0100))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#2E80))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#a4C6))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#aC00))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#D7A3))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#1F9C0))
  , ?_assertMatch(wide, ucd_data:east_asian_width(16#20000))
  , ?_assertMatch(half_width, ucd_data:east_asian_width(16#20A9))
  , ?_assertMatch(half_width, ucd_data:east_asian_width(16#FF61))
  , ?_assertMatch(full_width, ucd_data:east_asian_width(16#FF01))

  , ?_assert(ucd_data:composition_exclusion(16#0F43))

  , ?_assert(not ucd_data:full_composition_exclusion(16#0339))
  , ?_assert(ucd_data:full_composition_exclusion(16#0340))

  , ?_assert(not ucd_data:changes_when_nfkc_casefolded(16#0040))
  , ?_assert(ucd_data:changes_when_nfkc_casefolded(16#005A))
  , ?_assert(ucd_data:changes_when_nfkc_casefolded(16#00A8))

  , ?_assertEqual(16#0020, ucd_data:nfkc_casefold(16#00A0))
  , ?_assertEqual([16#0020, 16#0308], ucd_data:nfkc_casefold(16#00A8))

  , ?_assertEqual(yes, ucd_data:nfd_quick_check(16#00BF))
  , ?_assertEqual(no, ucd_data:nfd_quick_check(16#00C0))

  , ?_assertEqual(yes, ucd_data:nfc_quick_check(16#033F))
  , ?_assertEqual(no, ucd_data:nfc_quick_check(16#0340))
  , ?_assertEqual(maybe, ucd_data:nfc_quick_check(16#0345))

  , ?_assertEqual(no, ucd_data:nfkd_quick_check(16#00BA))

  , ?_assertEqual(no, ucd_data:nfkc_quick_check(16#00BA))
  , ?_assertEqual(maybe, ucd_data:nfkc_quick_check(16#115AF))

  , ?_assertEqual(ex, ucd_data:line_break(16#003F))
  , ?_assertEqual(al, ucd_data:line_break(16#007E))
  , ?_assertEqual(xx, ucd_data:line_break(16#100000))

  , ?_assertEqual(extend, ucd_data:grapheme_break_property(16#0300))
  , ?_assertEqual(spacing_mark, ucd_data:grapheme_break_property(16#1133F))
  , ?_assertEqual(other, ucd_data:grapheme_break_property(16#100000))

  , ?_assertEqual(newline, ucd_data:word_break_property(16#000B))
  , ?_assertEqual(newline, ucd_data:word_break_property(16#0085))
  , ?_assertEqual(extend, ucd_data:word_break_property(16#0300))
  , ?_assertEqual(other, ucd_data:word_break_property(16#100000))

  , ?_assertEqual(sep, ucd_data:sentence_break_property(16#0085))
  , ?_assertEqual(extend, ucd_data:sentence_break_property(16#0300))
  , ?_assertEqual(other, ucd_data:sentence_break_property(16#100000))

  , ?_assertEqual(l, ucd_data:hangul_syllable_type(16#A97C))
  , ?_assertEqual(lv, ucd_data:hangul_syllable_type(16#B54C))
  , ?_assertEqual(lvt, ucd_data:hangul_syllable_type(16#B099))

  , ?_assertEqual(<<"Cyrillic">>, ucd_data:block(16#400))
  , ?_assertEqual(<<"No_Block">>, ucd_data:block(16#E0080))
  , ?_assertEqual(<<"Supplementary Private Use Area-B">>, ucd_data:block(16#100000))
  ]}.


static_data_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_static)."
                  ,"blocks() -> ucd:blocks()."
                  ,"named_sequences() -> ucd:named_sequences()."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertMatch({{0,127}, <<"Basic Latin">>}, lists:nth(1,ucd_static:blocks()))
  , ?_assertMatch({<<"LATIN CAPITAL LETTER A WITH MACRON AND GRAVE">>, [16#0100, 16#0300]},
                  lists:nth(1,ucd_static:named_sequences()))
  ]}.


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


load(Code) ->
    case compile(Code) of
        {ok, Module, Binary, []} ->
            {module, Module} = code:load_binary(Module, "", Binary),
            Module;
        {error, Errors} -> error(Errors)
    end.
