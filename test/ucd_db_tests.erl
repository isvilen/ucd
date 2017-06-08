-module(ucd_db_tests).
-include_lib("ucd/include/ucd_db.hrl").
-include_lib("eunit/include/eunit.hrl").


unicode_data_test_() -> {setup, fun ucd_db:unicode_data/0, {with, [
  fun (Data) -> ?assertEqual(30578, length(Data)) end
 ,fun (Data) -> ?assertEqual(30564, length([C || #unicode_data{code=C} <- Data, is_integer(C)])) end
 ,fun (Data) -> ?assertEqual(14,    length([C || #unicode_data{code=C} <- Data, is_tuple(C)])) end
 ,fun (Data) ->
      CPs = [ CP || #unicode_data{code=CP, category=Cat} <- Data
            , Cat /= 'Co'
            , Cat /= 'Cs'
            , Cat /= 'Cc'
            ],
      ?assertEqual(128172, count(CPs))
  end
 ,fun (Data) ->
      Cats = ['Lu', 'Ll', 'Lt', 'Lm', 'Lo'
             ,'Mn', 'Mc', 'Me'
             ,'Nd', 'Nl', 'No'
             ,'Pc', 'Pd', 'Ps', 'Pe', 'Pi', 'Pf', 'Po'
             ,'Sm', 'Sc', 'Sk', 'So'
             ,'Zs', 'Zl', 'Zp'
             ,'Cc', 'Cf', 'Cs', 'Co', 'Cn'],
      lists:foreach(fun (#unicode_data{category=C}) ->
                        ?assert(lists:member(C, Cats), C)
                    end, Data)
  end
 ,fun (Data) ->
    Bidi = ['L', 'R', 'AL'
           ,'EN', 'ES', 'ET', 'AN', 'CS', 'NSM', 'BN'
           ,'B', 'S', 'WS', 'ON'
           ,'LRE', 'LRO', 'RLE', 'RLO', 'PDF', 'LRI', 'RLI', 'FSI', 'PDI'],
      lists:foreach(fun (#unicode_data{bidi_class=C}) ->
                        ?assert(lists:member(C, Bidi), C)
                    end, Data)
  end
]}}.


bidi_mirroring_test_() -> {setup, fun ucd_db:bidi_mirroring/0, {with, [
  fun (Data) -> ?assertEqual(364, length(Data)) end
 ,fun (Data) ->
      lists:foreach(fun (#bidi_mirroring{mirroring_glyph=C}) ->
                        ?assert(is_integer(C))
                    end, Data)
  end
]}}.


prop_list_test_() -> {setup, fun ucd_db:prop_list/0, {with, [
  fun (Data) ->
      ?assertEqual(180395, count([C || #prop_list{code=C} <- Data]))
  end
 ,fun (Data) ->
      ?assertEqual(25, count([C || #prop_list{code=C, property=white_space} <- Data]))
  end
 ,fun (Data) ->
      ?assertEqual(30, count([C || #prop_list{code=C, property=quotation_mark} <- Data]))
  end
 ,fun (Data) ->
      ?assertEqual(1362, count([C || #prop_list{code=C, property=other_math} <- Data]))
  end
 ,fun (Data) ->
      Props = [ white_space, bidi_control, join_control, dash, hyphen
              , quotation_mark, terminal_punctuation, other_math, hex_digit
              , ascii_hex_digit, other_alphabetic, ideographic, diacritic
              , extender, other_lowercase, other_uppercase, noncharacter_code_point
              , other_grapheme_extend, ids_binary_operator, ids_trinary_operator
              , radical, unified_ideograph, deprecated, soft_dotted
              , logical_order_exception, other_id_start, other_id_continue
              , sentence_terminal, variation_selector, pattern_white_space
              , pattern_syntax, prepended_concatenation_mark
              , other_default_ignorable_code_point ],
      lists:foreach(fun(#prop_list{property=P}) -> ?assert(lists:member(P, Props), P) end, Data)
  end
]}}.


unihan_numeric_values_test_() -> {setup, fun ucd_db:unihan_numeric_values/0, {with, [
  fun (Data) -> ?assertEqual(73, length(Data)) end
 ,fun (Data) ->
      Types = [k_accounting_numeric, k_other_numeric, k_primary_numeric],
      lists:foreach(fun (#unihan_numeric_value{type=T,value=V}) ->
                        ?assert(lists:member(T, Types), T),
                        ?assert(is_integer(V))
                    end, Data)
  end
]}}.


count(Cs) -> lists:sum(lists:map(fun count_1/1, Cs)).

count_1({C1,C2}) -> C2 - C1 + 1;
count_1(_)       -> 1.
