-type category() :: 'Lu'
                  | 'Ll'
                  | 'Lt'
                  | 'Lm'
                  | 'Lo'
                  | 'Mn'
                  | 'Mc'
                  | 'Me'
                  | 'Nd'
                  | 'Nl'
                  | 'No'
                  | 'Pc'
                  | 'Pd'
                  | 'Ps'
                  | 'Pe'
                  | 'Pi'
                  | 'Pf'
                  | 'Po'
                  | 'Sm'
                  | 'Sc'
                  | 'Sk'
                  | 'So'
                  | 'Zs'
                  | 'Zl'
                  | 'Zp'
                  | 'Cc'
                  | 'Cf'
                  | 'Cs'
                  | 'Co'
                  | 'Cn'.


-type bidi_class() :: 'L'
                    | 'R'
                    | 'AL'
                    | 'EN'
                    | 'ES'
                    | 'ET'
                    | 'AN'
                    | 'CS'
                    | 'NSM'
                    | 'BN'
                    | 'B'
                    | 'S'
                    | 'WS'
                    | 'ON'
                    | 'LRE'
                    | 'LRO'
                    | 'RLE'
                    | 'RLO'
                    | 'PDF'
                    | 'LRI'
                    | 'RLI'
                    | 'FSI'
                    | 'PDI'.


-type decomposition_type() :: font
                            | no_break
                            | initial
                            | medial
                            | final
                            | isolated
                            | circle
                            | super
                            | sub
                            | vertical
                            | wide
                            | narrow
                            | small
                            | square
                            | fraction
                            | compat.

-type decomposition() :: undefined
                       | {decomposition_type(), [char()]}
                       | [char()].


-type numeric() :: undefined
                 | {numeric, integer()}
                 | {numeric, {integer(), integer()}}
                 | {digit, integer()}
                 | {decimal, integer()}.


-record(unicode_data,{ code            :: char() | {char(), char()}
                     , name            :: binary()
                     , category        :: category()
                     , combining_class :: byte()
                     , bidi_class      :: bidi_class()
                     , decomposition   :: decomposition()
                     , numeric         :: numeric()
                     , bidi_mirrored   :: boolean()
                     , uppercase       :: undefined | char()
                     , lowercase       :: undefined | char()
                     , titlecase       :: undefined | char()
                     }).


-record(bidi_mirroring, { codepoint       :: char()
                        , mirroring_glyph :: char()

        }).


-record(prop_list, { code     :: char() | {char(), char()}
                   , property :: white_space
                               | bidi_control
                               | join_control
                               | dash
                               | hyphen
                               | quotation_mark
                               | terminal_punctuation
                               | other_math
                               | hex_digit
                               | ascii_hex_digit
                               | other_alphabetic
                               | ideographic
                               | diacritic
                               | extender
                               | other_lowercase
                               | other_uppercase
                               | noncharacter_code_point
                               | other_grapheme_extend
                               | ids_binary_operator
                               | ids_trinary_operator
                               | radical
                               | unified_ideograph
                               | deprecated
                               | soft_dotted
                               | logical_order_exception
                               | other_id_start
                               | other_id_continue
                               | sentence_terminal
                               | variation_selector
                               | pattern_white_space
                               | pattern_syntax
                               | prepended_concatenation_mark
                               | other_default_ignorable_code_point
                   }).


-record(case_folding, { codepoint :: char()
                      , type      :: full | common | simple | turkic
                      , mapping   :: char() | [char()]
                      }).


-type special_casing_context() :: final_sigma
                                | after_soft_dotted
                                | more_above
                                | before_dot
                                | not_before_dot
                                | after_I.

-type special_casing_lang() :: binary().

-record(special_casing, { codepoint :: char()
                        , lower     :: [char()]
                        , title     :: [char()]
                        , upper     :: [char()]
                        , context   :: [ special_casing_context()
                                       | special_casing_lang() ]
                        }).


-record(composition_exclusions, { codepoint :: char() }).


-record(derived_normalization_props, { code     :: char() | {char() | char()}
                                     , property :: full_composition_exclusion
                                                 | nfd_quick_check_no
                                                 | nfc_quick_check_no
                                                 | nfc_quick_check_maybe
                                                 | nfkd_quick_check_no
                                                 | nfkc_quick_check_no
                                                 | nfkc_quick_check_maybe
                                                 | {nfkc_casefold, [char()]}
                                                 | changes_when_nfkc_casefolded
                                     }).


-record(grapheme_break_property, { code  :: char() | {char(), char()}
                                 , value :: cr
                                          | lf
                                          | control
                                          | extend
                                          | zwj
                                          | regional_indicator
                                          | prepend
                                          | spacing_mark
                                          | l
                                          | v
                                          | t
                                          | lv
                                          | lvt
                                          | e_base
                                          | e_modifier
                                          | glue_after_zwj
                                          | e_base_gaz
                                 }).


-record(word_break_property, { code  :: char() | {char(), char()}
                             , value :: cr
                                      | lf
                                      | newline
                                      | extend
                                      | zwj
                                      | regional_indicator
                                      | format
                                      | katakana
                                      | hebrew_letter
                                      | a_letter
                                      | single_quote
                                      | double_quote
                                      | mid_num_let
                                      | mid_letter
                                      | mid_num
                                      | numeric
                                      | extend_num_let
                                      | e_base
                                      | e_modifier
                                      | glue_after_zwj
                                      | e_base_gaz
                             }).


-record(sentence_break_property, { code  :: char() | {char(), char()}
                                 , value :: cr
                                          | lf
                                          | extend
                                          | sep
                                          | format
                                          | sp
                                          | lower
                                          | upper
                                          | o_letter
                                          | numeric
                                          | a_term
                                          | s_continue
                                          | s_term
                                          | close
                                 }).


-type line_break_class() :: bk % non tailorable
                          | cm
                          | cr
                          | gl
                          | lf
                          | nl
                          | sg
                          | sp
                          | wj
                          | zw
                          | zwj
                          | ai % tailorable
                          | al
                          | b2
                          | ba
                          | bb
                          | cb
                          | cj
                          | cl
                          | cp
                          | eb
                          | em
                          | ex
                          | h2
                          | h3
                          | hl
                          | hy
                          | id
                          | in
                          | is
                          | jl
                          | jt
                          | jv
                          | ns
                          | nu
                          | op
                          | po
                          | pr
                          | qu
                          | ri
                          | sa
                          | sg
                          | sy
                          | xx.

-record(line_break, { code  :: char() | {char(), char()}
                    , value :: line_break_class()
                    }).


-record(block, { range :: {char(), char()}
               , name  :: binary()
               }).


-record(name_alias, { codepoint :: char()
                    , name      :: binary()
                    , type      :: correction
                                 | control
                                 | alternate
                                 | figment
                                 | abbreviation
                    }).


-record(named_sequence, { name       :: binary()
                        , codepoints :: [char()]
                        }).


-type east_asian_width_type() :: ambiguous
                               | full_width
                               | half_width
                               | neutral
                               | narrow
                               | wide.

-record(east_asian_width, { code  :: char() | {char(), char()}
                          , value :: east_asian_width_type()
                          }).


-record(hangul_syllable_type, { code  :: char() | {char(), char()}
                              , value :: l | v | t | lv | lvt
                              }).


-record(unihan_numeric_value, { codepoint :: char()
                              , type      :: k_accounting_numeric
                                           | k_other_numeric
                                           | k_primary_numeric
                              , value     :: integer()
                              }).
