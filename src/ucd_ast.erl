-module(ucd_ast).
-export([context/0, add/3, forms/1]).

-include("ucd_db.hrl").
-include_lib("syntax_tools/include/merl.hrl").


context() ->
    sets:new().


add(Name, [], Ctx) when Name == blocks
                      ; Name == named_sequences
                      ; Name == ranges ->
    Fun = fun_name(Name),
    {ok, ?Q("'@Fun@'()"), sets:add_element(Name, Ctx)};

add(Name, [Arg], Ctx) when Name == category
                         ; Name == combining_class
                         ; Name == bidi_class
                         ; Name == east_asian_width
                         ; Name == decomposition
                         ; Name == composition_exclusion
                         ; Name == full_composition_exclusion
                         ; Name == changes_when_nfkc_casefolded
                         ; Name == numeric
                         ; Name == bidi_mirrored
                         ; Name == bidi_mirroring
                         ; Name == uppercase
                         ; Name == lowercase
                         ; Name == titlecase
                         ; Name == special_uppercase
                         ; Name == special_lowercase
                         ; Name == special_titlecase
                         ; Name == common_case_folding
                         ; Name == simple_case_folding
                         ; Name == full_case_folding
                         ; Name == turkic_case_folding
                         ; Name == nfkc_casefold
                         ; Name == nfd_quick_check
                         ; Name == nfc_quick_check
                         ; Name == nfkd_quick_check
                         ; Name == nfkc_quick_check
                         ; Name == line_break
                         ; Name == grapheme_break_property
                         ; Name == sentence_break_property
                         ; Name == word_break_property
                         ; Name == hangul_syllable_type
                         ; Name == block
                         ; Name == name
                         ; Name == range
                         ; Name == prop_list ->
    Fun = fun_name(Name),
    {ok, ?Q("'@Fun@'(_@Arg)"), sets:add_element(Name, Ctx)};

add(Name, [Arg1, Arg2], Ctx) when Name == prop_list
                                ; Name == category
                                ; Name == bidi_class ->
    case validate_fun_arg(Name, Arg2) of
        {ok, V} ->
            Fun = fun_name(Name, V),
            {ok, ?Q("'@Fun@'(_@Arg1)"), sets:add_element({Name, V}, Ctx)};
        Error ->
            Error
    end;

add(Fun, _Args, _Ctx) ->
    {error, io_lib:format("invalid UCD function: ~s", [Fun])}.


forms(Ctx) ->
    {Forms, _} = lists:mapfoldl(fun form/2, #{}, sets:to_list(Ctx)),
    Forms.

form(Name, Data) when Name == category
                    ; Name == combining_class
                    ; Name == bidi_class
                    ; Name == east_asian_width
                    ; Name == full_composition_exclusion
                    ; Name == changes_when_nfkc_casefolded
                    ; Name == nfkc_casefold
                    ; Name == nfd_quick_check
                    ; Name == nfc_quick_check
                    ; Name == nfkd_quick_check
                    ; Name == nfkc_quick_check
                    ; Name == line_break
                    ; Name == grapheme_break_property
                    ; Name == sentence_break_property
                    ; Name == word_break_property
                    ; Name == hangul_syllable_type
                    ; Name == prop_list ->
    codepoint_data_fun(Name, Data, fun map_and_binary_index_ast/3);

form(Name, Data) when Name == decomposition
                    ; Name == composition_exclusion
                    ; Name == numeric
                    ; Name == bidi_mirrored
                    ; Name == bidi_mirroring
                    ; Name == uppercase
                    ; Name == lowercase
                    ; Name == titlecase
                    ; Name == special_uppercase
                    ; Name == special_lowercase
                    ; Name == special_titlecase
                    ; Name == common_case_folding
                    ; Name == simple_case_folding
                    ; Name == full_case_folding
                    ; Name == turkic_case_folding
                    ; Name == name ->
    codepoint_data_fun(Name, Data, fun map_ast/3);

form(Name, Data) when Name == block
                    ; Name == range ->
    codepoint_data_fun(Name, Data, fun binary_index_ast/3);

form(Name, Data) when Name == blocks
                    ; Name == ranges
                    ; Name == named_sequences ->
    static_data_fun(Name, Data);

form({Name, Arg}, Data) ->
    specialized_data_fun(Name, Arg, Data).


validate_fun_arg(prop_list, Arg) ->
    validate_atom_list_arg(Arg, prop_list_properties());

validate_fun_arg(category, Arg) ->
    validate_atom_list_arg(Arg, unicode_data_categories());

validate_fun_arg(bidi_class, Arg) ->
    validate_atom_list_arg(Arg, unicode_data_bidi_classes()).


codepoint_data_fun(Name, Data, ASTFun) ->
    Fun = fun_name(Name),
    {Values, Data1} = data_values(Name, Data),
    Default = data_default_ast(?Q("CP"), Name),
    AST = ASTFun(?Q("CP"), Values, Default),
    {?Q(["'@Fun@'(CP) when CP >= 0, CP =< 16#10ffff ->"
        ,"  _@AST;"
        ,"'@Fun@'(_) -> error(badarg)."])
    ,Data1}.


static_data_fun(Name, Data) ->
    Fun = fun_name(Name),
    {Values, Data1} = data_values(Name, Data),
    {?Q("'@Fun@'() -> _@Values@.") ,Data1}.


specialized_data_fun(Name, Arg, Data) ->
    Fun = fun_name(Name, Arg),
    {Values, Data1} = data_values(Name, Arg, Data),
    AST = map_and_binary_index_ast(?Q("CP"), Values, ?Q("false")),
    {?Q(["'@Fun@'(CP) when CP >= 0, CP =< 16#10ffff ->"
        ,"  _@AST;"
        ,"'@Fun@'(_) -> error(badarg)."])
    ,Data1}.


data_values(numeric, Data) ->
    {UcdVs, Data1} = unicode_data_values(numeric, Data),
    {UnihanVs, Data2} = data(unihan_numeric_values, Data1),
    {UcdVs ++ [{C, {T,V}} || #unihan_numeric_value{ codepoint = C
                                                  , type = T
                                                  , value =V
                                                  } <- UnihanVs]
    ,Data2};

data_values(Kind, Data) when Kind == uppercase
                           ; Kind == lowercase
                           ; Kind == titlecase ->
    {Vs1, Data1} = unicode_data_values(Kind, Data),
    {Vs2, Data2} = extended_casing_values(Kind, Data1),
    {Vs1 ++ Vs2 , Data2};

data_values(Kind, Data) when Kind == special_uppercase
                           ; Kind == special_lowercase
                           ; Kind == special_titlecase ->
    special_casing_values(Kind, Data);

data_values(bidi_mirroring, Data) ->
    bidi_mirroring_values(Data);

data_values(east_asian_width, Data) ->
    east_asian_width_values(Data);

data_values(composition_exclusion, Data) ->
    composition_exclusions_values(Data);

data_values(Kind, Data) when Kind == full_composition_exclusion
                           ; Kind == changes_when_nfkc_casefolded
                           ; Kind == nfkc_casefold
                           ; Kind == nfd_quick_check
                           ; Kind == nfc_quick_check
                           ; Kind == nfkd_quick_check
                           ; Kind == nfkc_quick_check ->
    derived_normalization_props_values(Kind, Data);

data_values(Kind, Data) when Kind == common_case_folding
                           ; Kind == simple_case_folding
                           ; Kind == full_case_folding
                           ; Kind == turkic_case_folding ->
    case_folding_values(Kind, Data);

data_values(line_break, Data) ->
    line_break_values(Data);

data_values(grapheme_break_property, Data) ->
    grapheme_break_property_values(Data);

data_values(sentence_break_property, Data) ->
    sentence_break_property_values(Data);

data_values(word_break_property, Data) ->
    word_break_property_values(Data);

data_values(hangul_syllable_type, Data) ->
    hangul_syllable_type_values(Data);

data_values(Kind, Data) when Kind == block; Kind == blocks ->
    blocks_values(Data);

data_values(Kind, Data) when Kind == named_sequences ->
    named_sequences_values(Data);

data_values(name, Data)  ->
    name_values(Data);

data_values(range, Data)  ->
    range_values(Data);

data_values(ranges, Data)  ->
    ranges_values(Data);

data_values(prop_list, Data)  ->
    prop_list_values(Data);

data_values(Kind, Data) ->
    unicode_data_values(Kind, Data).


data_values(prop_list, Props, Data) ->
    {Vs, Data1} = prop_list_values(Data),
    {[{C, true} || {C, Ps} <- Vs, (Props -- Ps) == []], Data1};

data_values(Kind, Requested, Data) ->
    {Vs, Data1} = unicode_data_values(Kind, Data, undefined),
    {codepoint_true_values(Vs, Requested), Data1}.


unicode_data_values(Kind, Data) ->
    unicode_data_values(Kind, Data, data_default(Kind)).

unicode_data_values(Kind, Data, Skip) ->
    {CPs, Data1} = data(unicode_data, Data),
    Vs = lists:filtermap(fun (#unicode_data{code=Code}=UD) ->
                                 case unicode_data_value(Kind, UD) of
                                     undefined        -> false;
                                     V when V == Skip -> false;
                                     V                -> {true, {Code, V}}
                                 end
                         end, CPs),
    {Vs, Data1}.

unicode_data_value(category, #unicode_data{category=V}) -> V;
unicode_data_value(combining_class, #unicode_data{combining_class=V}) -> V;
unicode_data_value(bidi_class, #unicode_data{bidi_class=V}) -> V;
unicode_data_value(decomposition, #unicode_data{decomposition=V}) -> V;
unicode_data_value(numeric, #unicode_data{numeric=V}) -> V;
unicode_data_value(bidi_mirrored, #unicode_data{bidi_mirrored=V}) -> V;
unicode_data_value(uppercase, #unicode_data{uppercase=V}) -> V;
unicode_data_value(lowercase, #unicode_data{lowercase=V}) -> V;
unicode_data_value(titlecase, #unicode_data{titlecase=V}) -> V.


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
        {value, {CP, Vs}, Rest} ->
            [{CP, [{Ctx, V} | Vs]} | Rest];
        false ->
            [{CP, [{Ctx, V}]} | Acc]
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


bidi_mirroring_values(Data) ->
    {Vs, Data1} = data(bidi_mirroring, Data),
    {[{CP, V} || #bidi_mirroring{codepoint=CP, mirroring_glyph=V} <- Vs], Data1}.


east_asian_width_values(Data) ->
    {Vs, Data1} = data(east_asian_width, Data),
    Default = data_default(east_asian_width),
    {[{C, V} || #east_asian_width{code=C, value=V} <- Vs, V /= Default], Data1}.


composition_exclusions_values(Data) ->
    {Vs, Data1} = data(composition_exclusions, Data),
    {[{C, true} || #composition_exclusions{codepoint=C} <- Vs], Data1}.


case_folding_values(Kind, Data) ->
    {Vs, Data1} = data(case_folding, Data),
    T1 = case Kind of
             common_case_folding -> common;
             simple_case_folding -> simple;
             full_case_folding   -> full;
             turkic_case_folding -> turkic
         end,
    {[{C, V} || #case_folding{codepoint=C, type=T2, mapping=V} <- Vs, T1 == T2]
    ,Data1}.


derived_normalization_props_values(Kind, Data) ->
    {Vs, Data1} = data(derived_normalization_props, Data),
    {lists:filtermap(fun (V) -> derived_normalization_props_value(Kind,V) end, Vs)
    ,Data1}.

derived_normalization_props_value(Kind, DPV) when Kind == full_composition_exclusion
                                                ; Kind == changes_when_nfkc_casefolded ->
    case DPV#derived_normalization_props.property of
        Kind ->
            {true, {DPV#derived_normalization_props.code, true}};
        _ ->
            false
    end;

derived_normalization_props_value(nfkc_casefold, DPV) ->
    case DPV#derived_normalization_props.property of
        {nfkc_casefold, [V]} ->
            {true, {DPV#derived_normalization_props.code, V}};
        {nfkc_casefold, V} ->
            {true, {DPV#derived_normalization_props.code, V}};
        _ ->
            false
    end;

derived_normalization_props_value(nfd_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfd_quick_check_no ->
            {true, {DPV#derived_normalization_props.code, no}};
        _ ->
            false
    end;

derived_normalization_props_value(nfc_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfc_quick_check_no ->
            {true, {DPV#derived_normalization_props.code, no}};
        nfc_quick_check_maybe ->
            {true, {DPV#derived_normalization_props.code, maybe}};
        _ ->
            false
    end;

derived_normalization_props_value(nfkd_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfkd_quick_check_no ->
            {true, {DPV#derived_normalization_props.code, no}};
        _ ->
            false
    end;

derived_normalization_props_value(nfkc_quick_check, DPV) ->
    case DPV#derived_normalization_props.property of
        nfkc_quick_check_no ->
            {true, {DPV#derived_normalization_props.code, no}};
        nfkc_quick_check_maybe ->
            {true, {DPV#derived_normalization_props.code, maybe}};
        _ ->
            false
    end.


line_break_values(Data) ->
    {Vs, Data1} = data(line_break, Data),
    {[{C,V} || #line_break{code=C,value=V} <- Vs], Data1}.


grapheme_break_property_values(Data) ->
    {Vs, Data1} = data(grapheme_break_property, Data),
    {[{C,V} || #grapheme_break_property{code=C,value=V} <- Vs], Data1}.


sentence_break_property_values(Data) ->
    {Vs, Data1} = data(sentence_break_property, Data),
    {[{C,V} || #sentence_break_property{code=C,value=V} <- Vs], Data1}.


word_break_property_values(Data) ->
    {Vs, Data1} = data(word_break_property, Data),
    {[{C,V} || #word_break_property{code=C,value=V} <- Vs], Data1}.


hangul_syllable_type_values(Data) ->
    {Vs, Data1} = data(hangul_syllable_type, Data),
    {[{C,V} || #hangul_syllable_type{code=C,value=V} <- Vs], Data1}.


blocks_values(Data) ->
    {Vs, Data1} = data(blocks, Data),
    {[{R,N} || #block{range=R,name=N} <- Vs], Data1}.


named_sequences_values(Data) ->
    {Vs, Data1} = data(named_sequences, Data),
    {[{N,CPs} || #named_sequence{name=N, codepoints=CPs} <- Vs], Data1}.


name_values(Data) ->
    {CPs, Data1} = data(unicode_data, Data),
    Vs = [{C, N} || #unicode_data{code=C,name=N,category=Cat} <- CPs
                  , is_integer(C)
                  , Cat /= 'Cc'],
    {Vs, Data1}.


range_values(Data) ->
    {CPs, Data1} = data(unicode_data, Data),
    Vs = [{C, range_value(N)} || #unicode_data{code=C,name=N} <- CPs, is_tuple(C)],
    {Vs, Data1}.

range_value(<<"CJK Ideograph">>) ->
    cjk_ideograph;

range_value(<<"CJK Ideograph Extension ",Rest/binary>>) ->
    {cjk_ideograph, {extension, binary_to_list(Rest)}};

range_value(<<"Private Use">>) ->
    private_use;

range_value(<<"Plane ",Id:2/binary," Private Use">>) ->
    {private_use, {plane, list_to_integer(binary_to_list(Id))}};

range_value(<<"Hangul Syllable">>) ->
    hangul_syllable;

range_value(<<"Tangut Ideograph">>) ->
    tangut_ideograph;

range_value(<<"Non Private Use High Surrogate">>) ->
    {high_surrogate, non_private_use};

range_value(<<"Private Use High Surrogate">>) ->
    {high_surrogate, private_use};

range_value(<<"Low Surrogate">>) ->
    low_surrogate.


ranges_values(Data) ->
    {Vs, Data1} = range_values(Data),
    {[{R,C} || {C,R} <- Vs], Data1}.


prop_list_values(Data) ->
    {Vs, Data1} = data(prop_list, Data),
    {prop_list_values_1(Vs), Data1}.

prop_list_values_1(Vs) ->
    Vs1 = lists:foldl(fun prop_list_values_1/2, #{}, Vs),
    Vs2 = lists:keysort(1, maps:to_list(Vs1)),
    [{CP, lists:sort(Ps)} || {CP, Ps} <- Vs2].

prop_list_values_1(#prop_list{code={F,T}, property=V}, Acc) ->
    lists:foldl(fun (CP, Acc0) -> prop_list_add_value(CP, V, Acc0) end,
                Acc, lists:seq(F,T));

prop_list_values_1(#prop_list{code=CP, property=V}, Acc) ->
    prop_list_add_value(CP, V, Acc).

prop_list_add_value(CP, V, Acc) ->
    maps:update_with(CP, fun (Vs) -> [V | Vs] end, [V] , Acc).


codepoint_true_values(Vs, Filter) ->
    Vs1 = lists:foldl(fun(V, Acc) -> codepoint_true_value(V, Filter, Acc) end,
                      #{}, Vs),
    lists:keysort(1, maps:to_list(Vs1)).

codepoint_true_value({C, V}, Filter, Acc) ->
    case lists:member(V, Filter) of
        false ->
            Acc;
        true ->
            case C of
                {F, T} ->
                    lists:foldl(fun (CP, Acc0) -> Acc0#{CP => true} end,
                                Acc, lists:seq(F,T));
                CP ->
                    Acc#{CP => true}
            end
    end.


data_default(category) -> 'Cn';
data_default(combining_class) -> 0;
data_default(bidi_class) -> 'L';
data_default(bidi_mirrored) -> false;
data_default(east_asian_width) -> 'N';
data_default(composition_exclusion) -> false;
data_default(full_composition_exclusion) -> false;
data_default(changes_when_nfkc_casefolded) -> false;
data_default(nfd_quick_check) -> yes;
data_default(nfc_quick_check) -> yes;
data_default(nfkd_quick_check) -> yes;
data_default(nfkc_quick_check) -> yes;
data_default(line_break) -> xx;
data_default(grapheme_break_property) -> other;
data_default(word_break_property) -> other;
data_default(sentence_break_property) -> other;
data_default(block) -> <<"No_Block">>;
data_default(prop_list) -> [];
data_default(_) -> undefined.


data_default_ast(CPVar, bidi_class) ->
    Def = data_default(bidi_class),
    data_default_ast(CPVar, ucd_db:bidi_class_defaults(), ?Q("_@Def@"));

data_default_ast(CPVar, east_asian_width) ->
    Def = data_default(east_asian_width),
    data_default_ast(CPVar, ucd_db:east_asian_width_defaults(), ?Q("_@Def@"));

data_default_ast(_, Kind) ->
    Def = data_default(Kind),
    ?Q("_@Def@").


data_default_ast(CPVar, Defaults, DefaultAST) ->
    Cases = [data_default_case_ast(CPVar, V) || V <- Defaults],
    DefCase = erl_syntax:clause([?Q("true")], [DefaultAST]),
    erl_syntax:if_expr(Cases ++ [DefCase]).

data_default_case_ast(CPVar, {{F,T} ,V}) ->
    erl_syntax:clause([?Q("_@CPVar >= _@F@"), ?Q("_@CPVar =< _@T@")], [?Q("_@V@")]).


binary_index_ast(VarAST, Data, DefaultAST) ->
    AST = binary_index_ast(VarAST, Data),
    ?Q(["case _@AST of"
       ,"   undefined -> _@DefaultAST;"
       ,"   V -> V"
       ,"end"]).

binary_index_ast(VarAST, Data) ->
    binary_index_ast_1(VarAST, split(Data)).

binary_index_ast_1(_VarAST, {[], []}) ->
    ?Q("undefined");

binary_index_ast_1(VarAST, {[{{0, To}, Value}], []}) ->
    ?Q(["if"
       ,"  _@VarAST =< _@To@ -> _@Value@;"
       ,"  true -> undefined"
       ,"end"]);

binary_index_ast_1(VarAST, {[{{From, To}, Value}], []}) ->
    ?Q(["if"
       ,"  _@VarAST >= _@From@, _@VarAST =< _@To@ -> _@Value@;"
       ,"  true -> undefined"
       ,"end"]);

binary_index_ast_1(VarAST, {[{Id, Value}], []}) ->
    ?Q(["if"
       ,"  _@VarAST == _@Id@ -> _@Value@;"
       ,"  true -> undefined"
       ,"end"]);

binary_index_ast_1(VarAST, {L1, [{Code, _}|_]=L2}) ->
    From = case Code of {Id, _} -> Id; Id -> Id end,
    AST1 = binary_index_ast_1(VarAST, split(L1)),
    AST2 = binary_index_ast_1(VarAST, split(L2)),
    ?Q(["if"
       ,"  _@VarAST < _@From@ ->"
       ,"    _@AST1;"
       ,"  true ->"
       ,"    _@AST2"
       ,"end"]).


map_ast(VarAST, Values, Default) ->
    Data = maps:from_list(Values),
    ?Q("maps:get(_@VarAST, _@Data@, _@Default)").


map_and_binary_index_ast(VarAST, Values, Default) ->
    Vs = group(Values),
    case lists:partition(fun ({C, _}) -> is_integer(C) end, Vs) of
        {CPVs, []} ->
            map_ast(VarAST, CPVs, Default);
        {[], RGVs} ->
            binary_index_ast(VarAST, RGVs, Default);
        {CPVs,RGVs} ->
            MapAST = map_ast(VarAST, CPVs, ?Q("undefined")),
            IdxAST = binary_index_ast(VarAST, RGVs, Default),
            ?Q(["case _@MapAST of"
               ,"  undefined ->"
               ,"    _@IdxAST;"
               ,"  V -> V"
               ,"end"])
    end.


fun_name(BaseName) ->
    fun_name_1(BaseName, "").

fun_name(BaseName, Vs) ->
    fun_name_1(BaseName, ["_" | [[fun_name_suffix(V), $_] || V <- Vs]]).

fun_name_1(BaseName, Suffix) ->
    list_to_atom(lists:flatten(["$ucd_", atom_to_list(BaseName), Suffix, "$"])).


fun_name_suffix(ascii_hex_digit)                    -> "ahd";
fun_name_suffix(bidi_control)                       -> "bc";
fun_name_suffix(dash)                               -> "da";
fun_name_suffix(deprecated)                         -> "de";
fun_name_suffix(diacritic)                          -> "di";
fun_name_suffix(extender)                           -> "ex";
fun_name_suffix(hex_digit)                          -> "hd";
fun_name_suffix(hyphen)                             -> "hy";
fun_name_suffix(ideographic)                        -> "id";
fun_name_suffix(ids_binary_operator)                -> "ibo";
fun_name_suffix(ids_trinary_operator)               -> "ito";
fun_name_suffix(join_control)                       -> "jc";
fun_name_suffix(logical_order_exception)            -> "loe";
fun_name_suffix(noncharacter_code_point)            -> "ncp";
fun_name_suffix(other_alphabetic)                   -> "oa";
fun_name_suffix(other_default_ignorable_code_point) -> "odicp";
fun_name_suffix(other_grapheme_extend)              -> "oge";
fun_name_suffix(other_id_start)                     -> "ois";
fun_name_suffix(other_id_continue)                  -> "oic";
fun_name_suffix(other_lowercase)                    -> "ol";
fun_name_suffix(other_math)                         -> "om";
fun_name_suffix(other_uppercase)                    -> "ou";
fun_name_suffix(pattern_syntax)                     -> "ps";
fun_name_suffix(pattern_white_space)                -> "pws";
fun_name_suffix(prepended_concatenation_mark)       -> "pcm";
fun_name_suffix(quotation_mark)                     -> "qm";
fun_name_suffix(radical)                            -> "ra";
fun_name_suffix(sentence_terminal)                  -> "se";
fun_name_suffix(soft_dotted)                        -> "sd";
fun_name_suffix(terminal_punctuation)               -> "tp";
fun_name_suffix(unified_ideograph)                  -> "ui";
fun_name_suffix(variation_selector)                 -> "vs";
fun_name_suffix(white_space)                        -> "ws";
fun_name_suffix(V)                                  -> atom_to_list(V).


validate_atom_list_arg(Arg, AllowedValues) ->
    try erl_syntax:concrete(Arg) of
        V  when is_atom(V) ->
            validate_atom_list_arg_1([V], AllowedValues);
        Vs when is_list(Vs) ->
            case lists:all(fun erlang:is_atom/1, Vs) of
                true  -> validate_atom_list_arg_1(Vs, AllowedValues);
                false -> {error, "list of atoms expected as function argument"}
            end;
        _ ->
            {error, "atom or list of atoms expected as function argument"}
    catch
        error:badarg -> {error, "invalid function argument"}
    end.


validate_atom_list_arg_1(Vs, AllowedValues) ->
    case validate_atom_list_arg_2(Vs, AllowedValues) of
        ok    -> {ok, lists:sort(Vs)};
        Error -> Error
    end.

validate_atom_list_arg_2([], _) ->
    ok;

validate_atom_list_arg_2([V|Vs], AllowedValues) ->
    case lists:member(V, AllowedValues) of
        true ->
            validate_atom_list_arg_2(Vs, AllowedValues);
        false ->
            {error, io_lib:format("invalid function argument: ~s", [V])}
    end.


prop_list_properties() ->
    [ white_space, bidi_control, join_control, dash, hyphen, quotation_mark
    , terminal_punctuation, other_math, hex_digit, ascii_hex_digit
    , other_alphabetic, ideographic, diacritic, extender, other_lowercase
    , other_uppercase, noncharacter_code_point, other_grapheme_extend
    , ids_binary_operator, ids_trinary_operator, radical, unified_ideograph
    , deprecated, soft_dotted, logical_order_exception, other_id_start
    , other_id_continue, sentence_terminal, variation_selector
    , pattern_white_space, pattern_syntax, prepended_concatenation_mark
    , other_default_ignorable_code_point ].


unicode_data_categories() ->
    ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Mn', 'Mc', 'Me', 'Nd', 'Nl', 'No', 'Pc', 'Pd'
    ,'Ps', 'Pe', 'Pi', 'Pf', 'Po', 'Sm', 'Sc', 'Sk', 'So', 'Zs', 'Zl', 'Zp', 'Cc'
    ,'Cf', 'Cs', 'Co', 'Cn'].


unicode_data_bidi_classes() ->
    ['L', 'R', 'AL', 'EN', 'ES', 'ET', 'AN', 'CS', 'NSM', 'BN', 'B', 'S', 'WS'
    ,'ON', 'LRE', 'LRO', 'RLE', 'RLO', 'PDF', 'LRI', 'RLI', 'FSI', 'PDI'].


group(Vs) ->
    group(Vs, []).

group([], Acc) ->
    lists:reverse(Acc);

group([V], []) ->
    [V];

group([{_, V1}=X | T], [{_,V2} | _] = Acc) when V1 =/= V2 ->
    group(T, [X | Acc]);

% [{1, x}, {1, x}] -> [{1,x}]
% [{{10,20}, x}, {{10,20}, x}] -> [{{10,20},x}]
group([{X, _} | T1], [{X, _} | _]=T2) ->
    group(T1, T2);

% [{{10,20}, x}, {{21,30}, x}] -> [{{10,30},x}]
group([{{From2, To2}, V} | T1], [{{From1, To1}, V} | T2])
  when From2 =< (To1 + 1), To2 >= To1 ->
    group(T1, [{{From1, To2}, V} | T2]);

% [{{10,20}, x}, {15, x}] -> [{{10,20},x}]
group([{Id, _} | T1], [{{From, To}, _} | _]=T2)
  when Id >= From, Id =< To ->
    group(T1, T2);

% [{{10,20}, x}, {21, x}] -> [{{10,21},x}]
group([{Id, V} | T1], [{{From1, To}, V} | T2])
  when Id == To + 1->
    group(T1, [{{From1, To+1}, V} | T2]);

% [{15, x}, {{16, 20}, x}] -> [{{15,20},x}]
group([{{From2, To2}, V} | T1], [{Id, V} | T2])
  when From2 == Id + 1 ->
    group(T1, [{{Id, To2}, V} | T2]);

% [{1, x}, {2, x}] -> [{{1,2},x}]
group([{Id2, V} | T1], [{Id1, V} | T2])
  when Id2 == Id1 + 1 ->
    group(T1, [{{Id1, Id2}, V} | T2]);

group([X | T], Acc) ->
    group(T, [X | Acc]).


split(L) -> split([], L, L).

split(L1, L2, []) ->
    {lists:reverse(L1), L2};

split(L1, [H|T], [_]) ->
    {lists:reverse([H | L1]), T};

split(L1, [H|T], [_,_|R]) ->
    split([H | L1], T, R).


data(Key, Data) ->
    case maps:get(Key, Data, undefined) of
        undefined ->
            V = ucd_db:Key(),
            {V, Data#{Key => V}};
        V ->
            {V, Data}
    end.
