-module(ucd_data).
-export([ new/0
        , values/2
        , values/3
        , allowed_values/1
        , allowed_value_tag/1
        , default_value/1
        ]).

-include("ucd_db.hrl").

-type data() :: #{}.

-spec new() -> data().
new() -> #{}.


values(Kind, Data) when Kind == bidi_class
                      ; Kind == bidi_mirrored
                      ; Kind == bidi_mirroring
                      ; Kind == brackets
                      ; Kind == blocks
                      ; Kind == category
                      ; Kind == combining_class
                      ; Kind == common_case_folding
                      ; Kind == composition_exclusions
                      ; Kind == decomposition
                      ; Kind == east_asian_width
                      ; Kind == full_case_folding
                      ; Kind == grapheme_break_property
                      ; Kind == hangul_syllable_type
                      ; Kind == line_break
                      ; Kind == name
                      ; Kind == named_sequences
                      ; Kind == numeric
                      ; Kind == ranges
                      ; Kind == sentence_break_property
                      ; Kind == simple_case_folding
                      ; Kind == turkic_case_folding
                      ; Kind == unihan_numeric_values
                      ; Kind == word_break_property ->
    data_values(Kind, Data);

values(Kind, Data) when Kind == uppercase
                      ; Kind == lowercase
                      ; Kind == titlecase ->
    {Vs1, Data1} = data_values(Kind, Data),
    {Vs2, Data2} = extended_casing_values(Kind, Data1),
    {Vs1 ++ Vs2 , Data2};

values(Kind, Data) when Kind == special_uppercase
                      ; Kind == special_lowercase
                      ; Kind == special_titlecase ->
    special_casing_values(Kind, Data);

values(composition, Data) ->
    composition_values(Data);

values(Kind, Data) when Kind == full_composition_exclusion
                      ; Kind == changes_when_nfkc_casefolded
                      ; Kind == nfkc_casefold
                      ; Kind == nfd_quick_check
                      ; Kind == nfc_quick_check
                      ; Kind == nfkd_quick_check
                      ; Kind == nfkc_quick_check ->
    derived_norm_props_values(Kind, Data);

values(prop_list, Data)  ->
    prop_list_values(Data);

values(canonical_equivalence, Data) ->
    canonical_equivalence_values(Data);

values(bidi_brackets, Data) ->
    bidi_brackets_values(Data).


values(name_aliases, Types, Data) ->
    {Vs, Data1} = data(name_aliases, Data),
    {name_aliases_values(Vs, Types), Data1};

values(Kind, Filter, Data) when Kind == prop_list
                              ; Kind == line_break
                              ; Kind == grapheme_break_property
                              ; Kind == sentence_break_property
                              ; Kind == word_break_property ->
    {Vs, Data1} = values(Kind, Data),
    {codepoints_values(Vs, Filter), Data1};

values(Kind, Filter, Data) when Kind == category
                              ; Kind == bidi_class
                              ; Kind == combining_class ->
    {Vs, Data1} = data_values(Kind, Data, undefined),
    {codepoints_values(Vs, Filter), Data1}.


extended_casing_values(Kind, Data) ->
    {Vs, Data1} = data(special_casing, Data),
    {lists:filtermap(fun (V) -> extended_casing_value(Kind,V) end, Vs), Data1}.

extended_casing_value(Kind, SCValue) ->
    case special_casing_kind_value(Kind, SCValue) of
        {CP, [CP], _} -> false;
        {CP, [V], []} -> {true, {CP, V}};
        {CP, V, []}   -> {true, {CP, V}};
        _             -> false
    end.


special_casing_values(Kind, Data) ->
    {Vs, Data1} = data(special_casing, Data),
    {lists:foldl(fun (V, Acc) -> special_casing_value(Kind,V,Acc) end, [], Vs), Data1}.

special_casing_value(Kind, SCValue, Acc) ->
    case special_casing_kind_value(Kind, SCValue) of
        {CP, [CP], _}  -> Acc;
        {_, _, []}     -> Acc;
        {CP, [V], Ctx} -> special_casing_add_value(CP, V, Ctx, Acc);
        {CP, V, Ctx}   -> special_casing_add_value(CP, V, Ctx, Acc)
    end.

special_casing_add_value(CP, V, Ctx, Acc) ->
    case lists:keytake(CP, 1, Acc) of
        {value, {CP, Vs}, Rest} -> [{CP, [{Ctx, V} | Vs]} | Rest];
        false                   -> [{CP, [{Ctx, V}]} | Acc]
    end.


special_casing_kind_value(Kind, #special_casing{codepoint=CP, context=Ctx}=SC) ->
    if
        Kind == uppercase
      ; Kind == special_uppercase -> {CP, SC#special_casing.upper, Ctx};
        Kind == lowercase
      ; Kind == special_lowercase -> {CP, SC#special_casing.lower, Ctx};
        Kind == titlecase
      ; Kind == special_titlecase -> {CP, SC#special_casing.title, Ctx}
    end.


composition_values(Data) ->
    {CPs, Data1} = data(unicode_data, Data),
    {Excl, Data2} = data(composition_exclusions, Data1),
    {composition_values(CPs, Excl), Data2}.

composition_values(CPs, Excl) ->
    NonStarters = lists:foldl(fun composition_non_starter/2, sets:new(), CPs),
    Exclusions = sets:from_list([CP || #composition_exclusions{codepoint=CP} <- Excl]),
    lists:foldr(fun (CP, Acc) ->
                    composition_value(CP, NonStarters, Exclusions, Acc)
                end, #{}, CPs).

composition_non_starter(#unicode_data{combining_class=0}, Acc) ->
    Acc;
composition_non_starter(#unicode_data{code=CP}, Acc) when is_integer(CP) ->
    sets:add_element(CP, Acc).

composition_value(#unicode_data{code=CP, combining_class=0,decomposition=[CP1,CP2]}
                 , NonStarters, Exclusions, Acc) ->
    case sets:is_element(CP1, NonStarters) of
        true  -> Acc;
        false -> case sets:is_element(CP, Exclusions) of
                     true  -> Acc;
                     false -> maps:put({CP1,CP2}, CP, Acc)
                 end
    end;

composition_value(_, _, _, Acc) ->
    Acc.


derived_norm_props_values(Kind, Data) ->
    {Vs, Data1} = data(derived_normalization_props, Data),
    {lists:filtermap(fun (V) -> derived_norm_props_value(Kind,V) end, Vs) ,Data1}.

derived_norm_props_value(Kind, DPV) when Kind == full_composition_exclusion
                                       ; Kind == changes_when_nfkc_casefolded ->
    case DPV#derived_normalization_props.property of
        Kind -> {true, {DPV#derived_normalization_props.code, true}};
        _    -> false
    end;

derived_norm_props_value(nfkc_casefold, DPV) ->
    case DPV#derived_normalization_props.property of
        {nfkc_casefold, [V]} -> {true, {DPV#derived_normalization_props.code, V}};
        {nfkc_casefold, V}   -> {true, {DPV#derived_normalization_props.code, V}};
        _                    -> false
    end;

derived_norm_props_value(nfd_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfd_quick_check_no -> {true, {DPV#derived_normalization_props.code, no}};
        _                  -> false
    end;

derived_norm_props_value(nfc_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfc_quick_check_no    -> {true, {DPV#derived_normalization_props.code, no}};
        nfc_quick_check_maybe -> {true, {DPV#derived_normalization_props.code, maybe}};
        _                     -> false
    end;

derived_norm_props_value(nfkd_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfkd_quick_check_no -> {true, {DPV#derived_normalization_props.code, no}};
        _                   -> false
    end;

derived_norm_props_value(nfkc_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfkc_quick_check_no    -> {true, {DPV#derived_normalization_props.code, no}};
        nfkc_quick_check_maybe -> {true, {DPV#derived_normalization_props.code, maybe}};
        _                      -> false
    end.


prop_list_values(Data) ->
    {Vs, Data1} = data(prop_list, Data),
    Vs1 = lists:foldl(fun prop_list_values/2, #{}, Vs),
    Vs2 = lists:keysort(1, maps:to_list(Vs1)),
    {[{CP, lists:sort(Ps)} || {CP, Ps} <- Vs2], Data1}.

prop_list_values(#prop_list{code=Code, property=V}, Acc) ->
    fold(Code, fun (CP, Acc0) -> prop_list_add(CP, V, Acc0) end, Acc).

prop_list_add(CP, V, Acc) ->
    maps:update_with(CP, fun (Vs) -> [V | Vs] end, [V] , Acc).


canonical_equivalence_values(Data) ->
    {Vs1, Data1} = data_values(decomposition, Data),
    {lists:foldl(fun canonical_equivalence_values/2, [], Vs1), Data1}.

canonical_equivalence_values({CP1, [CP2]}, Acc) ->
    [{CP1, CP2}, {CP2, CP1} | Acc];

canonical_equivalence_values(_, Acc) ->
    Acc.


bidi_brackets_values(Data) ->
    {Brackets, Data1} = values(brackets, Data),
    {BidiMirroring, Data2} = values(bidi_mirroring, Data1),
    {Eq, Data3} = values(canonical_equivalence, Data2),
    {bidi_brackets_values(BidiMirroring, Brackets, Eq), Data3}.

bidi_brackets_values(BidiMirroring, Brackets, Eq) ->
    BidiMap = maps:from_list(BidiMirroring),
    BracketsMap = maps:from_list(Brackets),
    EqMap = maps:from_list(Eq),
    lists:foldr(fun (V, Acc) ->
                     bidi_brackets_data(V, BidiMap, BracketsMap, EqMap, Acc)
                end, [], BidiMirroring).

bidi_brackets_data({CP1, CP2}, BidiMap, BracketsMap, EqMap, Acc) ->
    case maps:get(CP1, BracketsMap, undefined) of
        undefined ->
            Acc;
        Type ->
            OCPs = bidi_bracket_opposites(CP1, CP2, BidiMap, EqMap),
            [{CP1, {Type, OCPs}} | Acc]
    end.

bidi_bracket_opposites(CP1, CP2, BidiMap, EqMap) ->
    case maps:get(CP1, EqMap, undefined) of
        undefined ->
            [CP2];
        CP ->
            case maps:get(CP, BidiMap, undefined) of
                undefined -> [CP2];
                DCP2      -> [CP2, DCP2]
            end
    end.


codepoints_values(Vs, Filter) ->
    Fun = value_filter(Filter),
    Vs1 = lists:foldl(fun ({C,V}, Acc) ->
                           case Fun(V) of
                               true  -> fold(C, fun (CP, Acc0) ->
                                                    Acc0#{CP => true}
                                                end, Acc);
                               false -> Acc
                           end
                      end, #{}, Vs),
    lists:sort(maps:keys(Vs1)).


name_aliases_values(Vs, Types) ->
    Vs1 = lists:foldl(fun (V,Acc) -> name_aliases_value(V,Types,Acc) end, #{}, Vs),
    lists:keysort(1, maps:to_list(Vs1)).

name_aliases_value(V, Types, Acc) ->
    case name_aliases_match(V, Types) of
        true -> name_aliases_add(V, Acc);
        false -> Acc
    end.

name_aliases_match(#name_alias{type=T}, Types) ->
    lists:member(T, Types).

name_aliases_add(#name_alias{codepoint=CP, name=V}, Acc) ->
    maps:update_with(CP, fun (V0) when is_list(V0) -> [V | V0];
                             (V0)                  -> [V , V0]
                         end, V , Acc).


-spec allowed_values(Kind) -> [atom()] | {integer(), integer()}
      when Kind :: bidi_class
                 | category
                 | combining_class
                 | grapheme_break_property
                 | line_break
                 | name_aliases
                 | prop_list
                 | sentence_break_property
                 | word_break_property.
allowed_values(bidi_class) ->
    ['L', 'R', 'AL', 'EN', 'ES', 'ET', 'AN', 'CS', 'NSM', 'BN', 'B', 'S', 'WS'
    ,'ON', 'LRE', 'LRO', 'RLE', 'RLO', 'PDF', 'LRI', 'RLI', 'FSI', 'PDI'];

allowed_values(category) ->
    ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Mn', 'Mc', 'Me', 'Nd', 'Nl', 'No', 'Pc', 'Pd'
    ,'Ps', 'Pe', 'Pi', 'Pf', 'Po', 'Sm', 'Sc', 'Sk', 'So', 'Zs', 'Zl', 'Zp', 'Cc'
    ,'Cf', 'Cs', 'Co', 'Cn'];

allowed_values(combining_class) ->
    {0, 255};

allowed_values(grapheme_break_property) ->
    [cr, lf, control, extend, zwj, regional_indicator, prepend, spacing_mark
    ,l, v, t , lv, lvt, e_base, e_modifier, glue_after_zwj, e_base_gaz];

allowed_values(line_break) ->
    [bk, cm, cr, gl, lf, nl, sg, sp, wj, zw, zwj, ai, al, b2, ba, bb, cb, cj, cl
    ,cp, eb, em, ex, h2, h3, hl, hy, id, in, is, jl, jt, jv, ns, nu, op, po, pr
    ,qu, ri, sa, sg, sy, xx];

allowed_values(name_aliases) ->
    [correction, control, alternate, figment, abbreviation];

allowed_values(prop_list) ->
    [ white_space, bidi_control, join_control, dash, hyphen, quotation_mark
    , terminal_punctuation, other_math, hex_digit, ascii_hex_digit
    , other_alphabetic, ideographic, diacritic, extender, other_lowercase
    , other_uppercase, noncharacter_code_point, other_grapheme_extend
    , ids_binary_operator, ids_trinary_operator, radical, unified_ideograph
    , deprecated, soft_dotted, logical_order_exception, other_id_start
    , other_id_continue, sentence_terminal, variation_selector
    , pattern_white_space, pattern_syntax, prepended_concatenation_mark
    , other_default_ignorable_code_point ];

allowed_values(sentence_break_property) ->
    [cr, lf, extend, sep, format, sp, lower, upper, o_letter, numeric, a_term
    ,s_continue, s_term, close];

allowed_values(word_break_property) ->
    [cr, lf, newline, extend, zwj, regional_indicator, format, katakana
    ,hebrew_letter, a_letter, single_quote, double_quote, mid_num_let
    ,mid_letter,mid_num, numeric, extend_num_let, e_base, e_modifier
    ,glue_after_zwj, e_base_gaz].


-spec allowed_value_tag(atom() | integer()) -> string().
% prop_list
allowed_value_tag(ascii_hex_digit)                    -> "ahd";
allowed_value_tag(bidi_control)                       -> "bc";
allowed_value_tag(dash)                               -> "da";
allowed_value_tag(deprecated)                         -> "de";
allowed_value_tag(diacritic)                          -> "di";
allowed_value_tag(extender)                           -> "ex";
allowed_value_tag(hex_digit)                          -> "hd";
allowed_value_tag(hyphen)                             -> "hy";
allowed_value_tag(ideographic)                        -> "id";
allowed_value_tag(ids_binary_operator)                -> "ibo";
allowed_value_tag(ids_trinary_operator)               -> "ito";
allowed_value_tag(join_control)                       -> "jc";
allowed_value_tag(logical_order_exception)            -> "loe";
allowed_value_tag(noncharacter_code_point)            -> "ncp";
allowed_value_tag(other_alphabetic)                   -> "oa";
allowed_value_tag(other_default_ignorable_code_point) -> "odicp";
allowed_value_tag(other_grapheme_extend)              -> "oge";
allowed_value_tag(other_id_start)                     -> "ois";
allowed_value_tag(other_id_continue)                  -> "oic";
allowed_value_tag(other_lowercase)                    -> "ol";
allowed_value_tag(other_math)                         -> "om";
allowed_value_tag(other_uppercase)                    -> "ou";
allowed_value_tag(pattern_syntax)                     -> "ps";
allowed_value_tag(pattern_white_space)                -> "pws";
allowed_value_tag(prepended_concatenation_mark)       -> "pcm";
allowed_value_tag(quotation_mark)                     -> "qm";
allowed_value_tag(radical)                            -> "ra";
allowed_value_tag(sentence_terminal)                  -> "se";
allowed_value_tag(soft_dotted)                        -> "sd";
allowed_value_tag(terminal_punctuation)               -> "tp";
allowed_value_tag(unified_ideograph)                  -> "ui";
allowed_value_tag(variation_selector)                 -> "vs";
allowed_value_tag(white_space)                        -> "ws";
% line, grapheme, word, sentence breaks
allowed_value_tag(a_letter)                           -> "al";
allowed_value_tag(a_term)                             -> "at";
allowed_value_tag(close)                              -> "cl";
allowed_value_tag(control)                            -> "co";
allowed_value_tag(double_quote)                       -> "dq";
allowed_value_tag(e_base)                             -> "eb";
allowed_value_tag(e_base_gaz)                         -> "ebg";
allowed_value_tag(e_modifier)                         -> "em";
allowed_value_tag(extend)                             -> "ex";
allowed_value_tag(extend_num_let)                     -> "enl";
allowed_value_tag(format)                             -> "fo";
allowed_value_tag(glue_after_zwj)                     -> "gaz";
allowed_value_tag(hebrew_letter)                      -> "hl";
allowed_value_tag(katakana)                           -> "ka";
allowed_value_tag(lower)                              -> "lo";
allowed_value_tag(mid_letter)                         -> "ml";
allowed_value_tag(mid_num)                            -> "mn";
allowed_value_tag(mid_num_let)                        -> "mnl";
allowed_value_tag(newline)                            -> "nl";
allowed_value_tag(numeric)                            -> "nu";
allowed_value_tag(o_letter)                           -> "ol";
allowed_value_tag(prepend)                            -> "pr";
allowed_value_tag(regional_indicator)                 -> "ri";
allowed_value_tag(s_continue)                         -> "sc";
allowed_value_tag(s_term)                             -> "st";
allowed_value_tag(single_quote)                       -> "sq";
allowed_value_tag(spacing_mark)                       -> "sm";
allowed_value_tag(upper)                              -> "up";
% name aliases
allowed_value_tag(correction)                         -> "cor";
allowed_value_tag(alternate)                          -> "alt";
allowed_value_tag(figment)                            -> "fig";
allowed_value_tag(abbreviation)                       -> "abr";

allowed_value_tag(V) when is_atom(V)                  -> atom_to_list(V);
allowed_value_tag(V) when is_integer(V)               -> integer_to_list(V).


-spec default_value(Kind :: atom()) -> term().
default_value(category)         -> 'Cn';
default_value(combining_class)  -> 0;
default_value(bidi_class)       -> 'L';
default_value(bidi_mirrored)    -> false;
default_value(east_asian_width) -> narrow;

default_value(nfd_quick_check) -> yes;
default_value(nfc_quick_check) -> yes;
default_value(nfkd_quick_check) -> yes;
default_value(nfkc_quick_check) -> yes;

default_value(line_break)              -> xx;
default_value(grapheme_break_property) -> other;
default_value(word_break_property)     -> other;
default_value(sentence_break_property) -> other;

default_value(block) -> <<"No_Block">>;

default_value(_) -> undefined.


data_values(Key, Data) ->
    data_values(Key, Data, default_value(Key)).

data_values(Key, Data, Skip) ->
    {Vs, Data1} = data(Key, Data),
    {lists:filtermap(fun (V) ->
                         case data_value(Key, V) of
                             undefined      -> false;
                             {_, undefined} -> false;
                             {_, Skip}      -> false;
                             V1             -> {true, V1}
                         end
                     end, Vs), Data1}.


data_value(name, #unicode_data{code=C,name=N,category=Cat}) ->
    if
        is_integer(C), Cat /= 'Cc' -> {C, N};
        true -> undefined
    end;

data_value(numeric, #unicode_data{code=C, numeric=V}) -> {C, V};
data_value(category, #unicode_data{code=C, category=V}) -> {C, V};
data_value(uppercase, #unicode_data{code=C, uppercase=V}) -> {C, V};
data_value(lowercase, #unicode_data{code=C, lowercase=V}) -> {C, V};
data_value(titlecase, #unicode_data{code=C, titlecase=V}) -> {C, V};
data_value(bidi_class, #unicode_data{code=C, bidi_class=V}) -> {C, V};
data_value(decomposition, #unicode_data{code=C, decomposition=V}) -> {C, V};
data_value(bidi_mirrored, #unicode_data{code=C, bidi_mirrored=V}) -> {C, V};
data_value(combining_class, #unicode_data{code=C, combining_class=V}) -> {C, V};

data_value(brackets, #unicode_data{bidi_class='ON', bidi_mirrored=true}=V) ->
    case V#unicode_data.category of
        'Ps' -> {V#unicode_data.code, open};
        'Pe' -> {V#unicode_data.code, close};
        _    -> undefined
    end;
data_value(brackets, _) ->
    undefined;

data_value(_, #block{range=R,name=N}) -> {R,N};
data_value(_, #line_break{code=C,value=V}) -> {C,V};
data_value(_, #composition_exclusions{codepoint=C}) -> C;
data_value(_, #east_asian_width{code=C, value=V}) -> {C, V};
data_value(_, #word_break_property{code=C,value=V}) -> {C,V};
data_value(_, #hangul_syllable_type{code=C,value=V}) -> {C,V};
data_value(_, #grapheme_break_property{code=C,value=V}) -> {C,V};
data_value(_, #sentence_break_property{code=C,value=V}) -> {C,V};
data_value(_, #named_sequence{name=N, codepoints=CPs}) -> {N, CPs};
data_value(_, #bidi_mirroring{codepoint=CP, mirroring_glyph=V}) -> {CP, V};
data_value(_, #unihan_numeric_value{codepoint=C, type=T, value=V}) -> {C, {T,V}};

data_value(common_case_folding, #case_folding{codepoint=C, type=common, mapping=V}) ->
    {C, V};
data_value(simple_case_folding, #case_folding{codepoint=C, type=simple, mapping=V}) ->
    {C, V};
data_value(full_case_folding, #case_folding{codepoint=C, type=full, mapping=V}) ->
    {C, V};
data_value(turkic_case_folding, #case_folding{codepoint=C, type=turkic, mapping=V}) ->
    {C, V};
data_value(_, #case_folding{}) ->
    undefined;

data_value(ranges, #unicode_data{code=C,name=N}) when is_tuple(C) ->
    V = case N of
            <<"CJK Ideograph">> ->
                cjk_ideograph;
            <<"CJK Ideograph Extension ", Rest/binary>> ->
                {cjk_ideograph, {extension, binary_to_list(Rest)}};
            <<"Private Use">> ->
                private_use;
            <<"Plane ",Id:2/binary," Private Use">> ->
                {private_use, {plane, list_to_integer(binary_to_list(Id))}};
            <<"Hangul Syllable">> ->
                hangul_syllable;
            <<"Tangut Ideograph">> ->
                tangut_ideograph;
            <<"Non Private Use High Surrogate">> ->
                {high_surrogate, non_private_use};
            <<"Private Use High Surrogate">> ->
                {high_surrogate, private_use};
            <<"Low Surrogate">> ->
                low_surrogate
        end,
    {C, V};
data_value(ranges, _)  ->
    undefined.


data(Key, Data) when Key == name
                   ; Key == ranges
                   ; Key == category
                   ; Key == combining_class
                   ; Key == bidi_class
                   ; Key == decomposition
                   ; Key == numeric
                   ; Key == bidi_mirrored
                   ; Key == uppercase
                   ; Key == lowercase
                   ; Key == titlecase
                   ; Key == brackets ->
    data(unicode_data, Data);

data(Key, Data) when Key == common_case_folding
                   ; Key == simple_case_folding
                   ; Key == full_case_folding
                   ; Key == turkic_case_folding ->
    data(case_folding, Data);

data(Key, Data) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            V = ucd_db:Key(),
            {V, Data#{Key => V}};
        V ->
            {V, Data}
    end.


value_filter({'not', Filter}) ->
    Fun = value_filter(Filter),
    fun (V) -> not Fun(V) end;

value_filter(Filter) ->
    fun (V) when is_list(V) ->
        lists:any(fun (V1) -> lists:member(V1, Filter) end, V);
        (V) -> lists:member(V, Filter)
    end.


fold({F, T}, Fun, Acc) -> lists:foldl(Fun, Acc, lists:seq(F,T));
fold(CP, Fun, Acc)     -> Fun(CP, Acc).
