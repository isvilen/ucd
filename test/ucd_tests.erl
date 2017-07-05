-module(ucd_tests).
-include_lib("eunit/include/eunit.hrl").


invalid_fun_test_() ->
  [ ?_assertMatch({error, [{_, "invalid UCD function: x"}]},
                  compile(["-module(x)."
                          ,"f() -> ucd:x()."
                          ]))

  , ?_assertMatch({error, [{_, "invalid function argument: xx"}]},
                  compile(["-module(x)."
                          ,"f(CP) -> ucd:prop_list(CP, [xx])."
                          ]))

  , ?_assertMatch({error, [{_, "invalid function argument: xx"}]},
                  compile(["-module(x)."
                          ,"f(CP) -> ucd:category(CP, xx)."
                          ]))

  , ?_assertMatch({error, [{_, "invalid function argument: 1000"}]},
                  compile(["-module(x)."
                          ,"f(CP) -> ucd:combining_class(CP, 1000)."
                          ]))
  ].


codepoint_data_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_cp)."
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
                  ,"prop_list(CP) -> ucd:prop_list(CP)."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertMatch('So', ucd_cp:category(16#33FF))
  , ?_assertMatch('Lo', ucd_cp:category(16#3400))
  , ?_assertMatch('So', ucd_cp:category(16#4DC0))
  , ?_assertMatch('Cn', ucd_cp:category(16#E0080))

  , ?_assertMatch(230, ucd_cp:combining_class(16#599))
  , ?_assertMatch(222, ucd_cp:combining_class(16#59A))
  , ?_assertMatch(220, ucd_cp:combining_class(16#59B))
  , ?_assertMatch(0, ucd_cp:combining_class(16#E0080))

  , ?_assertMatch('NSM', ucd_cp:bidi_class(16#074A))
  , ?_assertMatch('AL', ucd_cp:bidi_class(16#074B))
  , ?_assertMatch('L', ucd_cp:bidi_class(16#E0080))

  , ?_assertMatch({compat, [16#0E4D ,16#0E32]}, ucd_cp:decomposition(16#0E33))
  , ?_assertMatch([16#4359], ucd_cp:decomposition(16#2F974))
  , ?_assertMatch(undefined, ucd_cp:decomposition(16#E0080))

  , ?_assertMatch({decimal,1}, ucd_cp:numeric(16#0E51))
  , ?_assertEqual({decimal, 9}, ucd_cp:numeric(16#0F29))
  , ?_assertEqual({numeric, {1,2}}, ucd_cp:numeric(16#0F2a))
  , ?_assertEqual({numeric, {17,2}}, ucd_cp:numeric(16#0F32))
  , ?_assertEqual({numeric, {-1,2}}, ucd_cp:numeric(16#0F33))
  , ?_assertEqual({decimal, 0}, ucd_cp:numeric(16#1040))
  , ?_assertEqual({digit, 0}, ucd_cp:numeric(16#2070))
  , ?_assertEqual({k_other_numeric, 5}, ucd_cp:numeric(16#3405))
  , ?_assertEqual({k_primary_numeric, 10000}, ucd_cp:numeric(16#4E07))
  , ?_assertEqual({k_accounting_numeric, 1000}, ucd_cp:numeric(16#4EDF))
  , ?_assertEqual({k_primary_numeric, 1000000000000}, ucd_cp:numeric(16#5146))
  , ?_assertEqual({k_primary_numeric, 100}, ucd_cp:numeric(16#767E))
  , ?_assertEqual({k_accounting_numeric, 10000}, ucd_cp:numeric(16#842C))
  , ?_assertEqual({k_other_numeric, 30}, ucd_cp:numeric(16#20983))
  , ?_assertEqual({k_other_numeric, 40}, ucd_cp:numeric(16#2098C))
  , ?_assertEqual({k_other_numeric, 4}, ucd_cp:numeric(16#2626D))
  , ?_assertEqual(undefined, ucd_cp:numeric($a))
  , ?_assertMatch(undefined, ucd_cp:numeric(16#E0080))

  , ?_assertMatch(true, ucd_cp:bidi_mirrored($[))
  , ?_assertMatch(false, ucd_cp:bidi_mirrored(16#E0080))

  , ?_assertMatch($], ucd_cp:bidi_mirroring($[))

  , ?_assertEqual($i, ucd_cp:lowercase($I))
  , ?_assertEqual($I, ucd_cp:uppercase($i))
  , ?_assertEqual([16#1F08, 16#0399], ucd_cp:uppercase(16#1F80))
  , ?_assertEqual([16#03A1, 16#0313], ucd_cp:titlecase(16#1FE4))

  , ?_assertEqual([{[<<"az">>], 16#0069}, {[<<"tr">>], 16#0069}],
                  ucd_cp:special_lowercase(16#0130))
  , ?_assertEqual([{[<<"lt">>,more_above], [16#012F, 16#0307]}],
                  ucd_cp:special_lowercase(16#012E))

  , ?_assertEqual(16#0069, ucd_cp:common_case_folding(16#0049))
  , ?_assertEqual(16#0131, ucd_cp:turkic_case_folding(16#0049))

  , ?_assertEqual(16#03C9, ucd_cp:common_case_folding(16#2126))
  , ?_assertEqual(16#1FF3, ucd_cp:simple_case_folding(16#1FFC))
  , ?_assertEqual([16#0073, 16#0073], ucd_cp:full_case_folding(16#00DF))

  , ?_assertMatch(narrow, ucd_cp:east_asian_width(16#0020))
  , ?_assertMatch(narrow, ucd_cp:east_asian_width(16#00A2))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#0000))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#007F))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#2011))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#25E6))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#a4D0))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#FFF9))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#1F890))
  , ?_assertMatch(neutral, ucd_cp:east_asian_width(16#E0001))
  , ?_assertMatch(ambiguous, ucd_cp:east_asian_width(16#00A1))
  , ?_assertMatch(ambiguous, ucd_cp:east_asian_width(16#2013))
  , ?_assertMatch(ambiguous, ucd_cp:east_asian_width(16#25E2))
  , ?_assertMatch(ambiguous, ucd_cp:east_asian_width(16#E0100))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#2E80))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#a4C6))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#aC00))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#D7A3))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#1F9C0))
  , ?_assertMatch(wide, ucd_cp:east_asian_width(16#20000))
  , ?_assertMatch(half_width, ucd_cp:east_asian_width(16#20A9))
  , ?_assertMatch(half_width, ucd_cp:east_asian_width(16#FF61))
  , ?_assertMatch(full_width, ucd_cp:east_asian_width(16#FF01))

  , ?_assert(ucd_cp:composition_exclusion(16#0F43))

  , ?_assert(not ucd_cp:full_composition_exclusion(16#0339))
  , ?_assert(ucd_cp:full_composition_exclusion(16#0340))

  , ?_assert(not ucd_cp:changes_when_nfkc_casefolded(16#0040))
  , ?_assert(ucd_cp:changes_when_nfkc_casefolded(16#005A))
  , ?_assert(ucd_cp:changes_when_nfkc_casefolded(16#00A8))

  , ?_assertEqual(16#0020, ucd_cp:nfkc_casefold(16#00A0))
  , ?_assertEqual([16#0020, 16#0308], ucd_cp:nfkc_casefold(16#00A8))

  , ?_assertEqual(yes, ucd_cp:nfd_quick_check(16#00BF))
  , ?_assertEqual(no, ucd_cp:nfd_quick_check(16#00C0))

  , ?_assertEqual(yes, ucd_cp:nfc_quick_check(16#033F))
  , ?_assertEqual(no, ucd_cp:nfc_quick_check(16#0340))
  , ?_assertEqual(maybe, ucd_cp:nfc_quick_check(16#0345))

  , ?_assertEqual(no, ucd_cp:nfkd_quick_check(16#00BA))

  , ?_assertEqual(no, ucd_cp:nfkc_quick_check(16#00BA))
  , ?_assertEqual(maybe, ucd_cp:nfkc_quick_check(16#115AF))

  , ?_assertEqual(ex, ucd_cp:line_break(16#003F))
  , ?_assertEqual(al, ucd_cp:line_break(16#007E))
  , ?_assertEqual(xx, ucd_cp:line_break(16#100000))

  , ?_assertEqual(extend, ucd_cp:grapheme_break_property(16#0300))
  , ?_assertEqual(spacing_mark, ucd_cp:grapheme_break_property(16#1133F))
  , ?_assertEqual(other, ucd_cp:grapheme_break_property(16#100000))

  , ?_assertEqual(newline, ucd_cp:word_break_property(16#000B))
  , ?_assertEqual(newline, ucd_cp:word_break_property(16#0085))
  , ?_assertEqual(extend, ucd_cp:word_break_property(16#0300))
  , ?_assertEqual(other, ucd_cp:word_break_property(16#100000))

  , ?_assertEqual(sep, ucd_cp:sentence_break_property(16#0085))
  , ?_assertEqual(extend, ucd_cp:sentence_break_property(16#0300))
  , ?_assertEqual(other, ucd_cp:sentence_break_property(16#100000))

  , ?_assertEqual(l, ucd_cp:hangul_syllable_type(16#A97C))
  , ?_assertEqual(lv, ucd_cp:hangul_syllable_type(16#B54C))
  , ?_assertEqual(lvt, ucd_cp:hangul_syllable_type(16#B099))

  , ?_assertEqual(<<"Cyrillic">>, ucd_cp:block(16#400))
  , ?_assertEqual(<<"No_Block">>, ucd_cp:block(16#E0080))
  , ?_assertEqual(<<"Supplementary Private Use Area-B">>, ucd_cp:block(16#100000))

  , ?_assertEqual([pattern_white_space, white_space], ucd_cp:prop_list($\s))
  , ?_assertEqual([], ucd_cp:prop_list(16#100000))
  ]}.


static_data_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_static)."
                  ,"blocks() -> ucd:blocks()."
                  ,"named_sequences() -> ucd:named_sequences()."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertEqual({{0,127}, <<"Basic Latin">>}, lists:nth(1,ucd_static:blocks()))
  , ?_assertEqual({<<"KEYCAP NUMBER SIGN">>, [16#0023, 16#FE0F, 16#20E3]},
                  lists:nth(1,ucd_static:named_sequences()))
  , ?_assertEqual({<<"LATIN CAPITAL LETTER A WITH MACRON AND GRAVE">>, [16#0100, 16#0300]},
                  lists:nth(13,ucd_static:named_sequences()))
  ]}.


name_data_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_name)."
                  ,"name(CP) -> ucd:name(CP)."
                  ,"range(CP) -> ucd:range(CP)."
                  ,"ranges() -> ucd:ranges()."
                  ,"lookup_name(Name) -> ucd:lookup_name(Name)."
                  ,"lookup_cc_alias(Name) -> ucd:lookup_aliases(Name, control)."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertEqual(<<"LATIN SMALL LETTER A">>, ucd_name:name($a))
  , ?_assertEqual(undefined, ucd_name:name($\n))
  , ?_assertEqual(undefined, ucd_name:name(16#3400))

  , ?_assertEqual({{cjk_ideograph,{extension,"A"}}, {16#3400, 16#4DB5}},
                  lists:nth(1,ucd_name:ranges()))

  , ?_assertEqual({cjk_ideograph,{extension,"A"}}, ucd_name:range(16#3400))
  , ?_assertEqual(hangul_syllable, ucd_name:range(16#AC00))
  , ?_assertEqual({high_surrogate, non_private_use}, ucd_name:range(16#D800))

  , ?_assertEqual($A,
                  ucd_name:lookup_name(<<"LATIN CAPITAL LETTER A">>))
  , ?_assertEqual(16#A7AA,
                  ucd_name:lookup_name(<<"LATIN CAPITAL LETTER H WITH HOOK">>))
  , ?_assertEqual(16#32D9,
                  ucd_name:lookup_name(<<"CIRCLED KATAKANA KO">>))

  , ?_assertEqual(16#0008, ucd_name:lookup_cc_alias(<<"BACKSPACE">>))
  , ?_assertEqual(undefined, ucd_name:lookup_cc_alias(<<"???">>))
  ]}.


specialized_funs_test_() ->
  {setup, fun () ->
             load(["-module(ucd_special)."
                  ,"hyphen_dash(CP) -> ucd:prop_list(CP, [hyphen, dash])."
                  ,"other_lowercase(CP) -> ucd:prop_list(CP, other_lowercase)."
                  ,"is_control(CP) -> ucd:category(CP, 'Cc')."
                  ,"is_left_to_right(CP) -> ucd:bidi_class(CP, 'L')."
                  ,"lbk(CP) -> ucd:line_break(CP, bk)."
                  ,"gbk(CP) -> ucd:grapheme_break_property(CP, spacing_mark)."
                  ,"mwb(CP) -> ucd:word_break_property(CP,[mid_letter, mid_num_let, mid_num])."
                  ,"scb(CP) -> ucd:sentence_break_property(CP, close)."
                  ,"cc(CP) -> ucd:combining_class(CP, 240)."
                  ,"ccn(CP)-> ucd:combining_class(CP, not [0, 230])."
                  ,"name_correction(CP)-> ucd:name_aliases(CP, correction)."
                  ,"composition(CP1,CP2) -> ucd:composition(CP1,CP2)."
                  ,"bracket(CP) -> ucd:bidi_bracket(CP)."
                  ,"normalize_name(Name) -> ucd:normalize_name(Name)."
                  ])
          end,
          fun code:purge/1,
  [ ?_assertEqual(true, ucd_special:hyphen_dash($-))
  , ?_assertEqual(false, ucd_special:hyphen_dash($=))

  , ?_assertEqual(true, ucd_special:other_lowercase(16#00AA))
  , ?_assertEqual(false, ucd_special:other_lowercase($A))

  , ?_assertEqual(false, ucd_special:is_control($A))
  , ?_assertEqual(true, ucd_special:is_control($\n))

  , ?_assertEqual(true, ucd_special:is_left_to_right($A))
  , ?_assertEqual(false, ucd_special:is_left_to_right(16#05BE))

  , ?_assertEqual(true, ucd_special:lbk(16#000B))
  , ?_assertEqual(false, ucd_special:lbk($\s))

  , ?_assertEqual(true, ucd_special:gbk(16#903))
  , ?_assertEqual(false, ucd_special:gbk($A))

  , ?_assertEqual(true, ucd_special:mwb($:))
  , ?_assertEqual(false, ucd_special:mwb($A))

  , ?_assertEqual(true, ucd_special:scb($'))
  , ?_assertEqual(false, ucd_special:scb($A))

  , ?_assertEqual(true, ucd_special:cc(16#345))
  , ?_assertEqual(false, ucd_special:cc($A))

  , ?_assertEqual(true, ucd_special:ccn(16#33B))
  , ?_assertEqual(false, ucd_special:ccn($\s))
  , ?_assertEqual(false, ucd_special:ccn(16#33E))

  , ?_assertEqual(<<"PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET">>,
                  ucd_special:name_correction(16#FE18))

  , ?_assertEqual(16#00C0, ucd_special:composition(16#0041, 16#0300))
  , ?_assertEqual(undefined, ucd_special:composition(16#0915, 16#093C))

  , ?_assertEqual({open, [$}]}, ucd_special:bracket(${))

  , ?_assertEqual(<<"nobreakspace">>,
                  ucd_special:normalize_name(<<"NO-BREAK SPACE">>))
  , ?_assertEqual(<<"hanguljungseongo-e">>,
                  ucd_special:normalize_name(<<"HANGUL JUNGSEONG O-E">>))
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
