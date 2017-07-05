-module(ucd_db).
-export([ unicode_data/0
        , bidi_mirroring/0
        , prop_list/0
        , case_folding/0
        , special_casing/0
        , composition_exclusions/0
        , derived_normalization_props/0
        , grapheme_break_property/0
        , word_break_property/0
        , sentence_break_property/0
        , line_break/0
        , blocks/0
        , name_aliases/0
        , named_sequences/0
        , east_asian_width/0
        , hangul_syllable_type/0
        , unihan_numeric_values/0
        , bidi_class_defaults/0
        , east_asian_width_defaults/0
        , line_break_defaults/0
        ]).

-include_lib("stdlib/include/zip.hrl").
-include("ucd_db.hrl").


-spec unicode_data() -> [#unicode_data{}].
unicode_data() ->
   fold_lines(fun unicode_data/2, "UnicodeData.txt").


-spec bidi_mirroring() -> [#bidi_mirroring{}].
bidi_mirroring() ->
    fold_lines(fun bidi_mirroring/2, "BidiMirroring.txt").


-spec prop_list() -> [#prop_list{}].
prop_list() ->
    Vs = fold_lines(fun prop_list/2, "PropList.txt"),
    sort_by_codepoints(Vs, #prop_list.code).


-spec case_folding() -> [#case_folding{}].
case_folding() ->
    fold_lines(fun case_folding/2, "CaseFolding.txt").


-spec special_casing() -> [#special_casing{}].
special_casing() ->
    fold_lines(fun special_casing/2, "SpecialCasing.txt").


-spec composition_exclusions() -> [#composition_exclusions{}].
composition_exclusions() ->
    fold_lines(fun composition_exclusions/2, "CompositionExclusions.txt").


-spec derived_normalization_props() -> [#derived_normalization_props{}].
derived_normalization_props() ->
    Vs = fold_lines(fun derived_normalization_props/2, "DerivedNormalizationProps.txt"),
    sort_by_codepoints(Vs, #derived_normalization_props.code).


-spec grapheme_break_property() -> [#grapheme_break_property{}].
grapheme_break_property() ->
    Vs = fold_lines(fun grapheme_break_property/2, "auxiliary/GraphemeBreakProperty.txt"),
    sort_by_codepoints(Vs, #grapheme_break_property.code).


-spec word_break_property() -> [#word_break_property{}].
word_break_property() ->
    Vs = fold_lines(fun word_break_property/2, "auxiliary/WordBreakProperty.txt"),
    sort_by_codepoints(Vs, #word_break_property.code).


-spec sentence_break_property() -> [#sentence_break_property{}].
sentence_break_property() ->
    Vs = fold_lines(fun sentence_break_property/2, "auxiliary/SentenceBreakProperty.txt"),
    sort_by_codepoints(Vs, #sentence_break_property.code).


-spec line_break() -> [#line_break{}].
line_break() ->
    fold_lines(fun line_break/2, "LineBreak.txt").


-spec blocks() -> [#block{}].
blocks() ->
    fold_lines(fun blocks/2, "Blocks.txt").


-spec name_aliases() -> [#name_alias{}].
name_aliases() ->
    fold_lines(fun name_aliases/2, "NameAliases.txt").


-spec named_sequences() -> [#named_sequence{}].
named_sequences() ->
    fold_lines(fun named_sequences/2, "NamedSequences.txt").


-spec east_asian_width() -> [#east_asian_width{}].
east_asian_width() ->
    fold_lines(fun east_asian_width/2, "EastAsianWidth.txt").


-spec hangul_syllable_type() -> [#hangul_syllable_type{}].
hangul_syllable_type() ->
    Vs = fold_lines(fun hangul_syllable_type/2, "HangulSyllableType.txt"),
    sort_by_codepoints(Vs, #hangul_syllable_type.code).


-spec unihan_numeric_values() -> [#unihan_numeric_value{}].
unihan_numeric_values() ->
    fold_lines(fun unihan_numeric_values/2, "Unihan_NumericValues.txt").


%% default values for unassigned codepoints from DerivedBidiClass.txt
-spec bidi_class_defaults() -> [{{char(), char()}, bidi_class()}].
bidi_class_defaults() ->
    Ranges = [{{16#0600, 16#07BF}, 'AL'}
             ,{{16#08A0, 16#08FF}, 'AL'}
             ,{{16#FB50, 16#FDCF}, 'AL'}
             ,{{16#FDF0, 16#FDFF}, 'AL'}
             ,{{16#FE70, 16#FEFF}, 'AL'}
             ,{{16#1EE00, 16#1EEFF}, 'AL'}
             ,{{16#0590, 16#05FF}, 'R'}
             ,{{16#07C0, 16#089F}, 'R'}
             ,{{16#FB1D, 16#FB4F}, 'R'}
             ,{{16#10800, 16#10FFF}, 'R'}
             ,{{16#1E800, 16#1EDFF}, 'R'}
             ,{{16#1EF00, 16#1EFFF}, 'R'}
             ,{{16#20A0, 16#20CF}, 'ET'}
             ],
    sort_by_codepoints(Ranges, 1).


-spec east_asian_width_defaults() -> [{{char(), char()}, east_asian_width_type()}].
east_asian_width_defaults() -> [
    {{16#3400, 16#4DBF}, wide}
   ,{{16#4E00, 16#9FFF}, wide}
   ,{{16#F900, 16#FAFF}, wide}
   ,{{16#20000, 16#2FFFD}, wide}
   ,{{16#30000, 16#3FFFD}, wide}
].


-spec line_break_defaults() -> [{{char(), char()}, line_break_class()}].
line_break_defaults() -> [
    {{16#20A0, 16#20CF}, pr}
   ,{{16#3400, 16#4DBF}, id}
   ,{{16#4E00, 16#9FFF}, id}
   ,{{16#F900, 16#FAFF}, id}
   ,{{16#1F000, 16#1FFFD}, id}
   ,{{16#20000, 16#2FFFD}, id}
   ,{{16#30000, 16#3FFFD}, id}
].


unicode_data([CP, Name | _] = Fields, Acc) ->
    case Name of
        <<"<",Range/binary>> ->
            case binary:match(Range, <<", Last>">>) of
                {Pos, _} ->
                    unicode_data_range(CP, binary:part(Range, {0, Pos}), Acc);
                 nomatch ->
                    [unicode_data_codepoint(Fields) | Acc]
            end;
        _ ->
            [unicode_data_codepoint(Fields) | Acc]
    end.


unicode_data_range(CP2, Name, [#unicode_data{code=CP1}=H | T]) ->
    [H#unicode_data{code={CP1, parse_codepoint(CP2)}, name=Name} | T].


unicode_data_codepoint([Cp, Name, Cat, CombClass, BidiClass, Decomp
                       ,Decimal, Digit, Numeric, BidiMirrored, _ , _
                       ,Upper, Lower, Title]) ->
    #unicode_data{ code = parse_codepoint(Cp)
                 , name = Name
                 , category = unicode_data_category(Cat)
                 , combining_class = unicode_data_combining_class(CombClass)
                 , bidi_class = unicode_data_bidi_class(BidiClass)
                 , decomposition = unicode_data_decomposition(Decomp)
                 , numeric = unicode_data_numeric(Decimal, Digit, Numeric)
                 , bidi_mirrored = unicode_data_bidi_mirrored(BidiMirrored)
                 , uppercase = unicode_data_case_mapping(Upper)
                 , lowercase = unicode_data_case_mapping(Lower)
                 , titlecase = unicode_data_case_mapping(Title)
                 }.


%% LC   Cased_Letter Lu | Ll | Lt
%% L    Letter       Lu | Ll | Lt | Lm | Lo
%% M    Mark         Mn | Mc | Me
%% N    Number       Nd | Nl | No
%% P    Punctuation  Pc | Pd | Ps | Pe | Pi | Pf | Po
%% S    Symbol       Sm | Sc | Sk | So
%% Z    Separator    Zs | Zl | Zp
%% C    Other        Cc | Cf | Cs | Co | Cn
unicode_data_category(Cat) ->
    parse_atom(Cat).


unicode_data_combining_class(CombClass) ->
    parse_integer(CombClass).


%% Strong Types              L   | R   | AL
%% Weak Types                EN  | ES  | ET  | AN  | CS  | NSM | BN
%% Neutral Types             B   | S   | WS  | ON
%% Explicit Formatting Types LRE | LRO | RLE | RLO | PDF | LRI | RLI | FSI | PDI
unicode_data_bidi_class(BidiClass) ->
    parse_atom(BidiClass).


unicode_data_decomposition(<<>>) ->
    undefined;

unicode_data_decomposition(Decomp) ->
    case binary:split(Decomp, <<" ">>, [global]) of
        [<<"<font>">>     | M] -> {font,     parse_codepoints(M)};
        [<<"<noBreak>">>  | M] -> {no_break, parse_codepoints(M)};
        [<<"<initial>">>  | M] -> {initial,  parse_codepoints(M)};
        [<<"<medial>">>   | M] -> {medial,   parse_codepoints(M)};
        [<<"<final>">>    | M] -> {final,    parse_codepoints(M)};
        [<<"<isolated>">> | M] -> {isolated, parse_codepoints(M)};
        [<<"<circle>">>   | M] -> {circle,   parse_codepoints(M)};
        [<<"<super>">>    | M] -> {super,    parse_codepoints(M)};
        [<<"<sub>">>      | M] -> {sub,      parse_codepoints(M)};
        [<<"<vertical>">> | M] -> {vertical, parse_codepoints(M)};
        [<<"<wide>">>     | M] -> {wide,     parse_codepoints(M)};
        [<<"<narrow>">>   | M] -> {narrow,   parse_codepoints(M)};
        [<<"<small>">>    | M] -> {small,    parse_codepoints(M)};
        [<<"<square>">>   | M] -> {square,   parse_codepoints(M)};
        [<<"<fraction>">> | M] -> {fraction, parse_codepoints(M)};
        [<<"<compat>">>   | M] -> {compat,   parse_codepoints(M)};
        M                      -> parse_codepoints(M)
    end.


unicode_data_numeric(<<>>, <<>>, <<>>) ->
    undefined;

unicode_data_numeric(<<>>, <<>>, Numeric) ->
    Value = case binary:split(Numeric, <<"/">>) of
                [N1,N2] -> {parse_integer(N1), parse_integer(N2)};
                [N]     -> parse_integer(N)
            end,
    {numeric, Value};

unicode_data_numeric(<<>>, <<Digit:8>>, <<Digit:8>>) ->
    {digit, Digit - $0};

unicode_data_numeric(<<Decimal:8>>, <<Decimal:8>>, <<Decimal:8>>) ->
    {decimal, Decimal - $0}.


unicode_data_bidi_mirrored(<<"Y">>) -> true;
unicode_data_bidi_mirrored(<<"N">>) -> false.


unicode_data_case_mapping(<<>>) -> undefined;
unicode_data_case_mapping(Bin)  -> parse_codepoint(Bin).


bidi_mirroring([CP1, CP2], Acc)->
    [#bidi_mirroring{ codepoint = parse_codepoint(CP1)
                    , mirroring_glyph = parse_codepoint(CP2)
                    } | Acc].


prop_list([CP, V], Acc) ->
    Prop = parse_atom(string:lowercase(string:trim(V))),
    [#prop_list{ code = parse_codepoint_or_range(CP)
               , property = Prop
               } | Acc].


case_folding([CP, <<"F">>, Mapping, <<>>], Acc) ->
    [#case_folding{ codepoint = parse_codepoint(CP)
                  , type = full
                  , mapping = parse_codepoints(Mapping)
                  } | Acc];

case_folding([CP, TypeVal, Mapping, <<>>], Acc) ->
    Type = case TypeVal of
               <<"C">> -> common;
               <<"S">> -> simple;
               <<"T">> -> turkic
           end,
    [#case_folding{ codepoint = parse_codepoint(CP)
                  , type = Type
                  , mapping = parse_codepoint(Mapping)
                  } | Acc].


special_casing([CP, Lower, Title, Upper, Conditions, <<>>], Acc) ->
    Cs = binary:split(Conditions, <<" ">>, [global]),
    [special_casing(CP, Lower, Title, Upper, Cs) | Acc];

special_casing([CP, Lower, Title, Upper, <<>>], Acc) ->
    [special_casing(CP, Lower, Title, Upper, []) | Acc].


special_casing(CP, Lower, Title, Upper, Conditions) ->
    #special_casing{ codepoint = parse_codepoint(CP)
                   , lower = parse_codepoints(Lower)
                   , title = parse_codepoints(Title)
                   , upper = parse_codepoints(Upper)
                   , context = [special_casing_context(C) || C <- Conditions]
                   }.


special_casing_context(<<"Final_Sigma">>)       -> final_sigma;
special_casing_context(<<"After_Soft_Dotted">>) -> after_soft_dotted;
special_casing_context(<<"More_Above">>)        -> more_above;
special_casing_context(<<"Before_Dot">>)        -> before_dot;
special_casing_context(<<"Not_Before_Dot">>)    -> not_before_dot;
special_casing_context(<<"After_I">>)           -> after_I;
special_casing_context(Lang)                    -> Lang.


composition_exclusions([CP], Acc) ->
    [#composition_exclusions{codepoint=parse_codepoint(CP)} | Acc].


derived_normalization_props([_,<<"FC_NFKC">>,_], Acc) ->
    Acc;

derived_normalization_props([V,<<"Full_Composition_Exclusion ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = full_composition_exclusion
                                 } | Acc];

derived_normalization_props([V,<<"NFD_QC">>,<<"N ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfd_quick_check_no
                                 } | Acc];

derived_normalization_props([V,<<"NFC_QC">>,<<"N ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfc_quick_check_no
                                 } | Acc];

derived_normalization_props([V,<<"NFC_QC">>,<<"M ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfc_quick_check_maybe
                                 } | Acc];

derived_normalization_props([V,<<"NFKD_QC">>,<<"N ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfkd_quick_check_no
                                 } | Acc];

derived_normalization_props([V,<<"NFKC_QC">>,<<"N ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfkc_quick_check_no
                                 } | Acc];

derived_normalization_props([V,<<"NFKC_QC">>,<<"M ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = nfkc_quick_check_maybe
                                 } | Acc];

derived_normalization_props([_, <<"Expands_On_NFD ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFC ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFKD ">>], Acc) ->
    Acc;

derived_normalization_props([_, <<"Expands_On_NFKC ">>], Acc) ->
    Acc;

derived_normalization_props([V,<<"NFKC_CF">>,Cps], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = {nfkc_casefold, parse_codepoints(Cps)}
                                 } | Acc];

derived_normalization_props([V,<<"Changes_When_NFKC_Casefolded ">>], Acc) ->
    [#derived_normalization_props{ code = parse_codepoint_or_range(V)
                                 , property = changes_when_nfkc_casefolded
                                 } | Acc].


grapheme_break_property([V, Break], Acc) ->
    [#grapheme_break_property{code = parse_codepoint_or_range(V)
                             ,value = grapheme_break_class(Break)
                             } | Acc].

grapheme_break_class(<<"SpacingMark ">>) -> spacing_mark;
grapheme_break_class(V) -> parse_atom(string:lowercase(string:trim(V))).


word_break_property([V, Break], Acc) ->
    [#word_break_property{ code = parse_codepoint_or_range(V)
                         , value = word_break_class(Break)
                         } | Acc].

word_break_class(<<"ALetter ">>) -> a_letter;
word_break_class(<<"MidNumLet ">>) -> mid_num_let;
word_break_class(<<"MidLetter ">>) -> mid_letter;
word_break_class(<<"MidNum ">>) -> mid_num;
word_break_class(<<"ExtendNumLet ">>) -> extend_num_let;
word_break_class(V) -> parse_atom(string:lowercase(string:trim(V))).


sentence_break_property([V, Break], Acc) ->
    [#sentence_break_property{ code = parse_codepoint_or_range(V)
                             , value = sentence_break_class(Break)
                             } | Acc].

sentence_break_class(<<"OLetter ">>) -> o_letter;
sentence_break_class(<<"ATerm ">>) -> a_term;
sentence_break_class(<<"SContinue ">>) -> s_continue;
sentence_break_class(<<"STerm ">>) -> s_term;
sentence_break_class(V) -> parse_atom(string:lowercase(string:trim(V))).


line_break([V, Break], Acc) ->
    [#line_break{ code = parse_codepoint_or_range(V)
                , value = line_break_class(Break)
                } | Acc].

line_break_class(V) ->
    {Class, _} = string:take(V, " ", true),
    parse_atom(string:lowercase(Class)).


blocks([Range, Name], Acc) ->
    [#block{ range = parse_codepoint_or_range(Range)
           , name = Name
           } | Acc].


name_aliases([CP, Name, Type], Acc) ->
    [#name_alias{ codepoint = parse_codepoint(CP)
                , name = Name
                , type = parse_atom(Type)
                } | Acc].


named_sequences([Name, CPs], Acc) ->
    [#named_sequence{ name = Name
                    , codepoints = parse_codepoints(CPs)
                    } | Acc].


east_asian_width([CpOrRange, V], Acc) ->
    [#east_asian_width{ code = parse_codepoint_or_range(CpOrRange)
                      , value = east_asian_width_value(V)
                      } | Acc].

east_asian_width_value(<<"A ",_/binary>>) -> ambiguous;
east_asian_width_value(<<"F ",_/binary>>) -> full_width;
east_asian_width_value(<<"H ",_/binary>>) -> half_width;
east_asian_width_value(<<"N ",_/binary>>) -> neutral;
east_asian_width_value(<<"Na ",_/binary>>)-> narrow;
east_asian_width_value(<<"W ",_/binary>>) -> wide.


hangul_syllable_type([CpOrRange, Type], Acc) ->
    [#hangul_syllable_type{ code = parse_codepoint_or_range(CpOrRange)
                          , value = hangul_syllable_type_value(Type)
                          } | Acc].

hangul_syllable_type_value(<<"L ">>)   -> l;
hangul_syllable_type_value(<<"V ">>)   -> v;
hangul_syllable_type_value(<<"T ">>)   -> t;
hangul_syllable_type_value(<<"LV ">>)  -> lv;
hangul_syllable_type_value(<<"LVT ">>) -> lvt.


unihan_numeric_values([C,T,N], Acc) ->
    [#unihan_numeric_value{ codepoint = parse_codepoint(C)
                          , type = unihan_numeric_type(T)
                          , value = parse_integer(N)
                          } | Acc].

unihan_numeric_type(<<"kAccountingNumeric">>) -> k_accounting_numeric;
unihan_numeric_type(<<"kOtherNumeric">>)      -> k_other_numeric;
unihan_numeric_type(<<"kPrimaryNumeric">>)    -> k_primary_numeric.


fold_lines(Fun, FileName) ->
    lists:reverse(fold_lines(Fun, FileName, [])).


fold_lines(Fun, FileName, Acc) ->
    Data = file(FileName),
    {ok, MP} = re:compile("(?:\\s*;\\s*)|\t"),
    fold_lines(Fun, Acc, binary:split(Data, <<"\n">>), MP).


fold_lines(_, Acc, [<<>>], _) ->
    Acc;

fold_lines(Fun, Acc, [Bin], MP) ->
    fold_line(Fun, Acc, Bin, MP);

fold_lines(Fun, Acc0, [Bin, Rest], MP) ->
    Acc1 = fold_line(Fun, Acc0, Bin, MP),
    fold_lines(Fun, Acc1, binary:split(Rest, <<"\n">>), MP).


fold_line(Fun, Acc, Bin, MP) ->
    Line = string:trim(Bin, trailing, "\n"),
    case string:split(Line, "#") of
        [Data, _] -> fold_fields(Fun, Acc, Data, MP);
        [Data]    -> fold_fields(Fun, Acc, Data, MP)
    end.


fold_fields(_Fun, Acc, <<>>, _MP) ->
    Acc;

fold_fields(Fun, Acc, Data, MP) ->
    Fields = re:split(Data, MP, [{return, binary}]),
    Fun(Fields, Acc).


file(FileName) ->
    file(zip_file(FileName), FileName).

file(ZipFileName, FileName) ->
    case zip:unzip(ZipFileName, [{file_list, [FileName]}, memory]) of
        {ok, [{_, Data}]} -> Data;
        _                 -> error(badarg)
    end.


zip_file(FileName) ->
    ZipFile = case string:lexemes(FileName, "_") of
                  [_]          -> "UCD.zip";
                  [Prefix | _] -> Prefix ++ ".zip"
              end,
    filename:join(code:priv_dir(ucd), ZipFile).


parse_codepoint(<<"U+",Bin/binary>>) ->
    parse_codepoint(Bin);

parse_codepoint(Bin) ->
    case io_lib:fread("~16u", binary_to_list(Bin)) of
        {ok,[V],[]} ->
            V;
        {ok,[V],Sp} ->
            case lists:any(fun(Ch) -> Ch /= $\s end, Sp) of
                true  -> error({badarg, Bin});
                false -> V
            end;
        _ ->
            error({badarg, Bin})
    end.


parse_codepoint_or_range(Bin) ->
    case binary:split(Bin, <<"..">>) of
        [CP1, CP2] -> {parse_codepoint(CP1), parse_codepoint(CP2)};
        [CP]       -> parse_codepoint(CP)
    end.


parse_codepoints(<<>>) ->
    [];
parse_codepoints(CPs) when is_binary(CPs) ->
    [parse_codepoint(CP) || CP <- binary:split(CPs, <<" ">>, [global])
                          , CP /= <<>>];
parse_codepoints(CPs) ->
    [parse_codepoint(CP) || CP <- CPs].


parse_integer(V) ->
    list_to_integer(binary_to_list(V)).


parse_atom(V) ->
    erlang:binary_to_existing_atom(V, latin1).


sort_by_codepoints(Data, Pos) ->
    lists:sort(fun (X,Y) -> compare_codepoints(X,Y,Pos) end, Data).

compare_codepoints(V1, V2, Pos) ->
    compare_codepoints_1(element(Pos,V1), element(Pos, V2)).

compare_codepoints_1({_,C1}, {C2,_}) -> C1 =< C2;
compare_codepoints_1({_,C1}, C2)     -> C1 =< C2;
compare_codepoints_1(C1, {C2,_})     -> C1 =< C2;
compare_codepoints_1(C1, C2)         -> C1 =< C2.
