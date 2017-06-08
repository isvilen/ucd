-module(ucd_ast).
-export([context/0, add/3, forms/1, normalize_name/1]).

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
                         ; Name == prop_list
                         ; Name == bidi_bracket ->
    Fun = fun_name(Name),
    {ok, ?Q("'@Fun@'(_@Arg)"), sets:add_element(Name, Ctx)};

add(Name, [Arg1, Arg2], Ctx) when Name == prop_list
                                ; Name == category
                                ; Name == bidi_class
                                ; Name == line_break
                                ; Name == grapheme_break_property
                                ; Name == sentence_break_property
                                ; Name == word_break_property
                                ; Name == combining_class
                                ; Name == name_aliases ->
    try validate_fun_arg(Name, Arg2) of
        V ->
            Fun = fun_name(Name, V),
            {ok, ?Q("'@Fun@'(_@Arg1)"), sets:add_element({Name, V}, Ctx)}
    catch
        throw:Error -> {error, Error}
    end;

add(Name, [Arg1, Arg2], Ctx) when Name == composition ->
    Fun = fun_name(Name),
    {ok, ?Q("'@Fun@'(_@Arg1, _@Arg2)"), sets:add_element(Name, Ctx)};

add(Name, [Arg], Ctx) when Name == normalize_name ->
    Fun = fun_name(Name),
    Ctx1 = sets:add_element(normalize_name_1, Ctx),
    {ok, ?Q("'@Fun@'(_@Arg)"), sets:add_element(Name, Ctx1)};

add(Name, [Arg], Ctx) when Name == lookup_name ->
    Fun = fun_name(Name),
    Ctx1 = sets:add_element(lookup_codepoint, Ctx),
    Ctx2 = sets:add_element(normalize_name, Ctx1),
    Ctx3 = sets:add_element(normalize_name_1, Ctx2),
    {ok, ?Q("'@Fun@'(_@Arg)"), sets:add_element(Name, Ctx3)};

add(Name, [Arg1, Arg2], Ctx) when Name == lookup_aliases ->
    try validate_fun_arg(Name, Arg2) of
        V ->
            Fun = fun_name(Name, V),
            Ctx1 = sets:add_element(lookup_codepoint, Ctx),
            Ctx2 = sets:add_element(normalize_name, Ctx1),
            Ctx3 = sets:add_element(normalize_name_1, Ctx2),
            {ok, ?Q("'@Fun@'(_@Arg1)"), sets:add_element({Name, V}, Ctx3)}
    catch
        throw:Error -> {error, Error}
    end;

add(Fun, _Args, _Ctx) ->
    {error, io_lib:format("invalid UCD function: ~s", [Fun])}.


forms(Ctx) ->
    {Forms, _} = lists:mapfoldl(fun form/2, ucd_data:new(), sets:to_list(Ctx)),
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
                    ; Name == name
                    ; Name == bidi_bracket ->
    codepoint_data_fun(Name, Data, fun map_ast/3);

form(Name, Data) when Name == block
                    ; Name == range ->
    codepoint_data_fun(Name, Data, fun binary_index_ast/3);

form(Name, Data) when Name == blocks
                    ; Name == ranges
                    ; Name == named_sequences ->
    static_data_fun(Name, Data);

form(Name, Data) when Name == composition ->
    codepoint2_data_fun(Name, Data, fun map_ast/3);

form(Name, Data) when Name == lookup_name ->
    codepoint_lookup_fun(Name, Data);

form({Name, V}, Data) when Name == lookup_aliases ->
    codepoint_lookup_fun(Name, V, Data);

form(lookup_codepoint, Data) ->
    Fun = fun_name(lookup_codepoint),
    AST = ?Q(["'@Fun@'(_, From, To, _) when From >= To -> undefined;"
             ,"'@Fun@'(Hash, From, To, Data) ->"
             ,"  Pos = From + (To - From) div 2,"
             ,"  case binary:part(Data, Pos*6, 6) of"
             ,"    <<Hash:27,CP:21>>           -> CP;"
             ,"    <<H:27,_:21>> when H > Hash -> '@Fun@'(Hash, From, Pos, Data);"
             ,"    _                           -> '@Fun@'(Hash, Pos+1, To, Data)"
             ,"  end."]),
    {AST, Data};

form(normalize_name, Data) ->
    Fun = fun_name(normalize_name),
    Fun1 = fun_name(normalize_name_1),
    AST = ?Q(["'@Fun@'(Name) when is_binary(Name) ->"
             ,"   N1 = string:lowercase(Name),"
             ,"   N2 = string:lexemes(N1, \" \"),"
             ,"   N3 = ['@Fun1@'(N) || N <- N2],"
             ,"   iolist_to_binary(N3);"
             ,"'@Fun@'(_) -> error(badarg)."
             ]),
    {AST, Data};

form(normalize_name_1, Data) ->
    Fun = fun_name(normalize_name_1),
    AST = ?Q(["'@Fun@'(<<\"o-e\">>) -> <<\"o-e\">>;"
             ,"'@Fun@'(<<\"g-o-e\">>) -> <<\"go-e\">>;"
             ,"'@Fun@'(<<A,$-,B,Rest/binary>>)"
             ,"  when A >= $a, A =< $z, B >= $a, B =< $z ->"
             ,"  [A | '@Fun@'(<<B,Rest/binary>>)];"
             ,"'@Fun@'(<<H,T/binary>>) -> [H | '@Fun@'(T)];"
             ,"'@Fun@'(<<>>)           -> []."
             ]),
    {AST, Data};

form({Name, Arg}, Data) ->
    specialized_data_fun(Name, Arg, Data).


normalize_name(Name) when is_binary(Name) ->
    N1 = string:lowercase(Name),
    N2 = string:lexemes(N1, " "),
    N3 = [normalize_name_1(N) || N <- N2],
    iolist_to_binary(N3).

% special case for U+1180 HANGUL JUNGSEONG O-E
normalize_name_1(<<"o-e">>) -> <<"o-e">>;
normalize_name_1(<<"g-o-e">>) -> <<"go-e">>;
normalize_name_1(<<A,$-,B,Rest/binary>>) when A >= $a, A =< $z,
                                              B >= $a, B =< $z ->
    [A | normalize_name_1(<<B,Rest/binary>>)];
normalize_name_1(<<H,T/binary>>) -> [H | normalize_name_1(T)];
normalize_name_1(<<>>)           -> [].


validate_fun_arg(Name, Arg) when Name == prop_list
                               ; Name == category
                               ; Name == bidi_class
                               ; Name == line_break
                               ; Name == grapheme_break_property
                               ; Name == word_break_property
                               ; Name == sentence_break_property
                               ; Name == name_aliases
                               ; Name == lookup_aliases ->
    validate_fun_arg(Name, Arg, fun validate_atom_list_arg/2);

validate_fun_arg(Name, Arg) when Name == combining_class ->
    validate_fun_arg(Name, Arg, fun validate_integer_list_arg/2).


validate_fun_arg(Name, Arg, Fun) ->
    AllowedValues = allowed_values(Name),
    case Arg of
        ?Q("not _@Arg1") -> {'not', Fun(Arg1, AllowedValues)};
        _                -> Fun(Arg, AllowedValues)
    end.


allowed_values(lookup_aliases) -> allowed_values(name_aliases);
allowed_values(Name)           -> ucd_data:allowed_values(Name).


codepoint_data_fun(Name, Data, ASTFun) ->
    Fun = fun_name(Name),
    {Values, Data1} = data_values(Name, Data),
    Default = data_default_ast(?Q("CP"), Name),
    AST = ASTFun(?Q("CP"), Values, Default),
    {?Q(["'@Fun@'(CP) when CP >= 0, CP =< 16#10ffff ->"
        ,"  _@AST;"
        ,"'@Fun@'(_) -> error(badarg)."])
    ,Data1}.

codepoint2_data_fun(Name, Data, ASTFun) ->
    Fun = fun_name(Name),
    {Values, Data1} = data_values(Name, Data),
    Default = data_default_ast(?Q("{CP1, CP2}"), Name),
    AST = ASTFun(?Q("{CP1, CP2}"), Values, Default),
    {?Q(["'@Fun@'(CP1, CP2) when CP1 >= 0, CP1 =< 16#10ffff"
        ,"                     ; CP2 >= 0, CP2 =<  16#10ffff ->"
        ,"  _@AST;"
        ,"'@Fun@'(_, _) -> error(badarg)."])
    ,Data1}.


codepoint_lookup_fun(Name, Data) ->
    {Vs, Data1} = lookup_values(Name, Data),
    {codepoint_lookup_fun_1(fun_name(Name), Vs), Data1}.

codepoint_lookup_fun(Name, Filter, Data) ->
    {Vs, Data1} = lookup_values(Name, Filter, Data),
    {codepoint_lookup_fun_1(fun_name(Name, Filter), Vs), Data1}.

codepoint_lookup_fun_1(Fun, Data) ->
    NormNameFun = fun_name(normalize_name),
    LookupFun = fun_name(lookup_codepoint),
    case Data of
        {LookupData, Size, []} ->
            ?Q(["'@Fun@'(Name) ->"
               ,"  N1 = '@NormNameFun@'(Name),"
               ,"  Hash = erlang:phash2(N1),"
               ,"  '@LookupFun@'(Hash, 0, _@Size@, _@LookupData@)."]);
        {LookupData, Size, Collisions} ->
            ?Q(["'@Fun@'(Name) ->"
               ,"  N1 = '@NormNameFun@'(Name),"
               ,"  Hash = erlang:phash2(N1),"
               ,"  case lists:keyfind(Hash, 1, _@Collisions@) of"
               ,"    false ->"
               ,"      '@LookupFun@'(Hash, 0, _@Size@, _@LookupData@);"
               ,"    {_, Collisions} ->"
               ,"      case lists:keyfind(N1, 2, Collisions) of"
               ,"        false   -> undefined;"
               ,"        {CP, _} -> CP"
               ,"      end"
               ,"  end."])
    end.


static_data_fun(Name, Data) ->
    Fun = fun_name(Name),
    {Values, Data1} = data_values(Name, Data),
    {?Q("'@Fun@'() -> _@Values@.") ,Data1}.


specialized_data_fun(Name, Arg, Data) ->
    Fun = fun_name(Name, Arg),
    {Values, Data1} = data_values(Name, Arg, Data),
    Default = data_default_ast(?Q("CP"), {Name, Arg}),
    AST = map_and_binary_index_ast(?Q("CP"), Values, Default),
    {?Q(["'@Fun@'(CP) when CP >= 0, CP =< 16#10ffff ->"
        ,"  _@AST;"
        ,"'@Fun@'(_) -> error(badarg)."])
    ,Data1}.


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


data_values(composition_exclusion, Data) ->
    {Vs, Data1} = ucd_data:values(composition_exclusions, Data),
    {[{C, true} || C <- Vs], Data1};

data_values(block, Data) ->
    ucd_data:values(blocks, Data);

data_values(range, Data) ->
    ucd_data:values(ranges, Data);

data_values(ranges, Data) ->
    {Vs, Data1} = ucd_data:values(ranges, Data),
    {[{R,C} || {C,R} <- Vs], Data1};

data_values(numeric, Data) ->
    {Vs1, Data1} = ucd_data:values(numeric, Data),
    {Vs2, Data2} = ucd_data:values(unihan_numeric_values, Data1),
    {Vs1 ++ Vs2, Data2};

data_values(bidi_bracket, Data) ->
    ucd_data:values(bidi_brackets, Data);

data_values(Kind, Data)  ->
    ucd_data:values(Kind, Data).


data_values(Kind, Filter, Data) when Kind == name_aliases ->
    ucd_data:values(Kind, Filter, Data);

data_values(Kind, Filter, Data) ->
    {Vs, Data1} = ucd_data:values(Kind, Filter, Data),
    {[{C, true} || C <- Vs], Data1}.


lookup_values(lookup_name, Data) ->
    {Vs, Data1} = ucd_data:values(name, Data),
    {lookup_values(Vs), Data1}.


lookup_values(lookup_aliases, Filter, Data) ->
    {Vs, Data1} = ucd_data:values(name_aliases, Filter, Data),
    {lookup_values(Vs), Data1}.


lookup_values(Values) ->
    ValuesMap = lookup_values_fold(Values, #{}),
    {Vs, Collisions} = lookup_values_split(ValuesMap),
    Vs1 = lists:keysort(1, Vs),
    Vs2 = << <<H:27,V:21>> || {H,V} <- Vs1 >>,
    {Vs2, length(Vs1), Collisions}.

lookup_values_fold(Data, Acc) ->
    lists:foldl(fun ({CP, Names}, Acc0) when is_list(Names) ->
                   lists:foldl(fun (N, Acc1) -> lookup_value(CP, N, Acc1) end,
                               Acc0, Names);
                    ({CP, Name}, Acc0) ->
                        lookup_value(CP, Name, Acc0)
                end, Acc, Data).

lookup_value(CP, Name, Acc) ->
    N = normalize_name(Name),
    V = {CP, N},
    maps:update_with(erlang:phash2(N),
                     fun (V0) when is_list(V0) -> [V | V0];
                         (V0)                  -> [V , V0]
                     end, V , Acc).


lookup_values_split(Values) ->
    maps:fold(fun (K, {CP, _}, {Acc1, Acc2}) -> {[{K,CP} | Acc1], Acc2};
                  (K, Vs,      {Acc1, Acc2}) -> {Acc1, [{K,Vs} | Acc2]}
              end, {[], []}, Values).


data_default(composition_exclusion)        -> false;
data_default(full_composition_exclusion)   -> false;
data_default(changes_when_nfkc_casefolded) -> false;
data_default(prop_list)                    -> [];
data_default({name_aliases, _})            -> undefined;
data_default({_, _})                       -> false;
data_default(Kind)                         -> ucd_data:default_value(Kind).


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
    Data = if
               is_list(Values) -> maps:from_list(Values);
               true            -> Values
           end,
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
    fun_name(BaseName, "", []).

fun_name(BaseName, {Op, Vs}) ->
    fun_name(BaseName, atom_to_list(Op), Vs);

fun_name(BaseName, Vs) ->
    fun_name(BaseName, "", Vs).


fun_name(BaseName, Pre, Vs) ->
    H = case Pre of "" -> "_"; _  -> ["_", Pre, "_"] end,
    T = lists:join($_, [ucd_data:allowed_value_tag(V) || V <- Vs]),
    list_to_atom(lists:flatten(["$ucd_", atom_to_list(BaseName), H, T, "$"])).


validate_atom_list_arg(Arg, AllowedValues) ->
    validate_list_arg(Arg, fun (V) -> validate_atom_arg(V, AllowedValues) end).

validate_integer_list_arg(Arg, MinMax) ->
    validate_list_arg(Arg, fun (V) -> validate_integer_arg(V, MinMax) end).


validate_list_arg(Arg, Fun) ->
    try erl_syntax:concrete(Arg) of
        Vs when is_list(Vs) -> lists:sort([Fun(V) || V <- Vs]);
        V                   -> [Fun(V)]
    catch
        error:badarg -> throw("invalid function argument")
    end.


validate_atom_arg(V, AllowedValues) when is_atom(V) ->
    case lists:member(V, AllowedValues) of
        true  -> V;
        false -> throw(io_lib:format("invalid function argument: ~s", [V]))
    end;

validate_atom_arg(_, _) ->
    throw("invalid function argument").


validate_integer_arg(V, {Min, Max}) when is_integer(V) ->
    if
        V >= Min, V =< Max -> V;
        true -> throw(io_lib:format("invalid function argument: ~p", [V]))
    end;

validate_integer_arg(_, _) ->
    throw("invalid function argument").


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
