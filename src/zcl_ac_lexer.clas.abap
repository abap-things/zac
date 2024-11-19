CLASS zcl_ac_lexer DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      tyt_input TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_token,
        type        TYPE string,
        raw_value   TYPE string,
        typed_value TYPE REF TO data,
        row         TYPE i,
        col         TYPE i,
      END OF tys_token .
    TYPES:
      tyt_token TYPE STANDARD TABLE OF tys_token WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_regex2token,
        pattern    TYPE string,
        token_type TYPE string,
        regex      TYPE REF TO cl_abap_regex,
      END OF tys_regex2token .
    TYPES:
      tyt_regex2token TYPE STANDARD TABLE OF tys_regex2token WITH DEFAULT KEY .

    CONSTANTS:
      BEGIN OF cs_key_string,
        code_prefix         TYPE string VALUE `*!`,
        subst_prefix_regex  TYPE string VALUE `\{`,
        subst_postfix_regex TYPE string VALUE `\}`,
      END OF cs_key_string .
    CONSTANTS:
      BEGIN OF cs_token_type,
        space_type      TYPE string VALUE `SPACE`,
        space_dup       TYPE string VALUE `SPACE_DUP`,
        text            TYPE string VALUE `TEXT`,
        literal         TYPE string VALUE `LITERAL`,
        int_literal     TYPE string VALUE `INT_LITERAL`,
        string_literal  TYPE string VALUE `STRING_LITERAL`,
        id              TYPE string VALUE `ID`,
        variable        TYPE string VALUE `VARIABLE`,
        fun             TYPE string VALUE `FUN`,
        opening_bracket TYPE string VALUE `OPENING_BRACKET`,
        closing_bracket TYPE string VALUE `CLOSING_BRACKET`,
        subst           TYPE string VALUE 'SUBST',
        comma           TYPE string VALUE `COMMA`,
        full_stop       TYPE string VALUE `FULL_STOP`,
        keyword         TYPE string VALUE `KEYWORD`,
        operator        TYPE string VALUE `OPERATOR`,
        unexpected      TYPE string VALUE `UNEXPECTED`,
        comment         TYPE string VALUE `COMMENT`,
        eof             TYPE string VALUE `EOF`,
      END OF cs_token_type .

    CLASS-METHODS tokenize_script
      IMPORTING
        !it_input       TYPE tyt_input
      RETURNING
        VALUE(rt_token) TYPE tyt_token .

    CLASS-METHODS tokenize_code
      IMPORTING
        !iv_input       TYPE string
      RETURNING
        VALUE(rt_token) TYPE tyt_token .

    CLASS-METHODS class_constructor .
  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA mo_library_descr TYPE REF TO cl_abap_objectdescr.

    CLASS-METHODS tokenize_line
      IMPORTING
        !iv_input       TYPE string
        !iv_row         TYPE i
      RETURNING
        VALUE(rt_token) TYPE tyt_token.

    CLASS-METHODS tokenize_text_line
      IMPORTING
        !iv_text        TYPE string
        !iv_row         TYPE i
        !iv_col         TYPE i
      RETURNING
        VALUE(rt_token) TYPE tyt_token.

    CLASS-METHODS get_regex2token_mapping
      RETURNING
        VALUE(rt_regex2token) TYPE tyt_regex2token.

    CLASS-METHODS tokenize_code_line
      IMPORTING
        !iv_code        TYPE string
        !iv_row         TYPE i
        !iv_col         TYPE i
      RETURNING
        VALUE(rt_token) TYPE tyt_token.

    CLASS-METHODS extract_string_literal
      IMPORTING
        !iv_probably_string TYPE string
      RETURNING
        VALUE(rv_literal)   TYPE string.

    CLASS-METHODS clarify_literals
      CHANGING
        ct_token TYPE tyt_token.

    CLASS-METHODS classify_ids
      CHANGING
        ct_token TYPE tyt_token .
ENDCLASS.



CLASS ZCL_AC_LEXER IMPLEMENTATION.


  METHOD tokenize_line.
    DATA(ls_found_off) = find( val = iv_input sub = cs_key_string-code_prefix ).

    IF ls_found_off < 0.
      rt_token = tokenize_text_line(
        iv_text = iv_input
        iv_row = iv_row
        iv_col = 0
      ).
      RETURN.
    ENDIF.

    DATA(lv_text_line) = iv_input(ls_found_off).
    DATA(lv_code_off) = ls_found_off + strlen( cs_key_string-code_prefix ).
    DATA(lv_code_line) = iv_input+lv_code_off.

    IF lv_text_line IS NOT INITIAL.
      rt_token = tokenize_text_line(
        iv_text = lv_text_line
        iv_row = iv_row
        iv_col = 0
      ).
    ENDIF.

    APPEND LINES OF tokenize_code_line(
      iv_code = lv_code_line
      iv_row = iv_row
      iv_col = lv_code_off
    ) TO rt_token.
  ENDMETHOD.


  METHOD clarify_literals.
*   set typed values, unify literal tokens
    LOOP AT ct_token ASSIGNING FIELD-SYMBOL(<ls_token>).
      CASE <ls_token>-type.
        WHEN cs_token_type-string_literal.
          DATA(lv_draft_string) = <ls_token>-raw_value.
          DATA(lv_terminator) = lv_draft_string(1).
          DATA(lv_cut_length) = strlen( lv_draft_string ) - 2.
          lv_draft_string = lv_draft_string+1(lv_cut_length).

          DATA(lv_index) = 0.

          WHILE lv_index < strlen( lv_draft_string ).
            DATA(lv_next) = lv_index + 1.

            IF lv_draft_string+lv_index(1) = lv_terminator
            AND lv_next < strlen( lv_draft_string )
            AND lv_draft_string+lv_next(1) = lv_terminator.
              lv_draft_string = lv_draft_string(lv_index) && lv_draft_string+lv_next.
            ENDIF.

            lv_index += 1.
          ENDWHILE.

          CREATE DATA <ls_token>-typed_value TYPE string.
          ASSIGN <ls_token>-typed_value->* TO FIELD-SYMBOL(<lv_string_value>).
          <lv_string_value> = lv_draft_string.
          <ls_token>-type = cs_token_type-literal.

        WHEN cs_token_type-int_literal.
          CREATE DATA <ls_token>-typed_value TYPE i.
          ASSIGN <ls_token>-typed_value->* TO FIELD-SYMBOL(<lv_int_value>).
          <lv_int_value> = <ls_token>-raw_value.
          <ls_token>-type = cs_token_type-literal.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD classify_ids.
    TYPES:
      BEGIN OF tys_id2type,
        id   TYPE string,
        type TYPE string,
      END OF tys_id2type.

    TYPES:
      tyt_id2type TYPE HASHED TABLE OF tys_id2type WITH UNIQUE KEY id.

    DATA(lt_id2type) = VALUE tyt_id2type(
      ( id = `IF`        type = cs_token_type-keyword )
      ( id = `ELSEIF`    type = cs_token_type-keyword )
      ( id = `ELSE`      type = cs_token_type-keyword )
      ( id = `ENDIF`     type = cs_token_type-keyword )
      ( id = `LOOP`      type = cs_token_type-keyword )
      ( id = `ENDLOOP`   type = cs_token_type-keyword )
      ( id = `AT`        type = cs_token_type-keyword )
      ( id = `INTO`      type = cs_token_type-keyword )
      ( id = `DATA`      type = cs_token_type-keyword )
      ( id = `WHERE`     type = cs_token_type-keyword )
      ( id = `DO`        type = cs_token_type-keyword )
      ( id = `TIMES`     type = cs_token_type-keyword )
      ( id = `ENDDO`     type = cs_token_type-keyword )
      ( id = `CHECK`     type = cs_token_type-keyword )
      ( id = `EXIT`      type = cs_token_type-keyword )
      ( id = `CONTINUE`  type = cs_token_type-keyword )
      ( id = `NOT`       type = cs_token_type-operator )
      ( id = `AND`       type = cs_token_type-operator )
      ( id = `OR`        type = cs_token_type-operator )
      ( id = `CO`        type = cs_token_type-operator )
      ( id = `CN`        type = cs_token_type-operator )
      ( id = `CA`        type = cs_token_type-operator )
      ( id = `NA`        type = cs_token_type-operator )
      ( id = `CS`        type = cs_token_type-operator )
      ( id = `NS`        type = cs_token_type-operator )
      ( id = `CP`        type = cs_token_type-operator )
      ( id = `NP`        type = cs_token_type-operator )
    ).

    LOOP AT mo_library_descr->methods ASSIGNING FIELD-SYMBOL(<ls_method>).
      INSERT VALUE #(
        id = to_upper( <ls_method>-name )
        type = cs_token_type-fun
      ) INTO TABLE lt_id2type.
    ENDLOOP.

    LOOP AT ct_token ASSIGNING FIELD-SYMBOL(<ls_token>) WHERE type = cs_token_type-id.
      DATA(lv_upper) = to_upper( <ls_token>-raw_value ).
      <ls_token>-raw_value = lv_upper.

      IF line_exists( lt_id2type[ id = lv_upper ] ).
        <ls_token>-type = lt_id2type[ id = lv_upper ]-type.
        CONTINUE.
      ENDIF.

      <ls_token>-type = cs_token_type-variable.
    ENDLOOP.
  ENDMETHOD.


  METHOD extract_string_literal.
    CHECK iv_probably_string IS NOT INITIAL.
    CHECK iv_probably_string(1) = `'` OR iv_probably_string(1) = '`'.

    DATA(lv_terminator) = iv_probably_string(1).

    DATA(lv_index) = 1.
    WHILE lv_index < strlen( iv_probably_string ).
      DATA(lv_next) = lv_index + 1.

      IF iv_probably_string+lv_index(1) = lv_terminator
      AND lv_next < strlen( iv_probably_string )
      AND iv_probably_string+lv_next(1) = lv_terminator.
        lv_index += 2.
        CONTINUE.
      ENDIF.

      IF iv_probably_string+lv_index(1) = lv_terminator.
        rv_literal = iv_probably_string(lv_next).
        RETURN.
      ENDIF.

      lv_index += 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_regex2token_mapping.
    rt_regex2token = VALUE tyt_regex2token(
      ( pattern = '^(\s+)'                     token_type = cs_token_type-space_type )
      ( pattern = '^(\()'                      token_type = cs_token_type-opening_bracket )
      ( pattern = '^(\))'                      token_type = cs_token_type-closing_bracket )
      ( pattern = '^(\.)'                      token_type = cs_token_type-full_stop )
      ( pattern = '^(\,)'                      token_type = cs_token_type-comma )
      ( pattern = '^(<>|<=|>=|=|<|>)'          token_type = cs_token_type-operator )
      ( pattern = '^([\-\+]?\d+)'              token_type = cs_token_type-int_literal )
      ( pattern = '^(\-|\+|\*|\/)'             token_type = cs_token_type-operator )
      ( pattern = '^([A-Z_]\w*(\-[A-Z_]\w*)*)' token_type = cs_token_type-id )
      ( pattern = '^(".*)'                     token_type = cs_token_type-comment )
      ( pattern = '^(\S+)'                     token_type = cs_token_type-unexpected )
    ).

    LOOP AT rt_regex2token ASSIGNING FIELD-SYMBOL(<ls_regex2token>).
      <ls_regex2token>-regex = NEW #( pattern = <ls_regex2token>-pattern ignore_case = abap_true ).
    ENDLOOP.

  ENDMETHOD.


  METHOD tokenize_code_line.
    CHECK iv_code IS NOT INITIAL.

    DATA(lv_col) = iv_col.
    DATA(lv_code) = iv_code.

    WHILE lv_code IS NOT INITIAL.
*     extract string token
      DATA(lv_string_literal) = extract_string_literal( lv_code ).
      IF lv_string_literal IS NOT INITIAL.
        APPEND VALUE #(
          type = cs_token_type-string_literal
          raw_value = lv_string_literal
          row = iv_row
          col = lv_col
        ) TO rt_token.

        DATA(lv_len) = strlen( lv_string_literal ).
        lv_col += lv_len.
        lv_code = lv_code+lv_len.
        CONTINUE.
      ENDIF.

*     extract regex token
      DATA(lt_regex2token) = get_regex2token_mapping( ).
      DATA(lv_found) = abap_false.

      LOOP AT lt_regex2token ASSIGNING FIELD-SYMBOL(<ls_regex2token>).
        FIND REGEX <ls_regex2token>-regex IN lv_code
          SUBMATCHES DATA(lv_token)
          MATCH LENGTH lv_len.

        IF sy-subrc = 0.
          lv_found = abap_true.

          APPEND VALUE #(
            type = <ls_regex2token>-token_type
            raw_value = lv_token
            row = iv_row
            col = lv_col
          ) TO rt_token.

          lv_col += lv_len.
          lv_code = lv_code+lv_len.
          EXIT.
        ENDIF.
      ENDLOOP.

      ASSERT lv_found = abap_true.
    ENDWHILE.

*   add terminating space token
    APPEND VALUE #(
      type = cs_token_type-space_type
      raw_value = cl_abap_char_utilities=>cr_lf
      row = iv_row
      col = lv_col
    ) TO rt_token.

*   clarity tokens
    classify_ids( CHANGING ct_token = rt_token ).
    clarify_literals( CHANGING ct_token = rt_token ).
  ENDMETHOD.


  METHOD tokenize_text_line.
    DATA(lo_subst_regex) = NEW cl_abap_regex(
      pattern = |{ cs_key_string-subst_prefix_regex }([A-Z_]\\w*(\\-[A-Z_]\\w*)*){ cs_key_string-subst_postfix_regex }|
      ignore_case = abap_true
    ).

    DATA(lv_text) = iv_text.
    DATA(lv_col) = iv_col.

    WHILE lv_text IS NOT INITIAL.
      FIND REGEX lo_subst_regex IN lv_text
        SUBMATCHES DATA(lv_token)
        MATCH OFFSET DATA(lv_moff)
        MATCH LENGTH DATA(lv_mlen).

      IF sy-subrc = 0.
        DATA(lv_text_part) = lv_text(lv_moff).
        IF lv_text IS NOT INITIAL.
          APPEND VALUE #(
            type = cs_token_type-text
            raw_value = lv_text_part
            row = iv_row
            col = lv_col
          ) TO rt_token.
        ENDIF.

        APPEND VALUE #(
          type = cs_token_type-subst
          raw_value = to_upper( lv_token )
          row = iv_row
          col = lv_col + lv_moff
        ) TO rt_token.

        lv_col += lv_moff + lv_mlen.
        lv_text = substring( val = lv_text off = lv_moff + lv_mlen ).

        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        type = cs_token_type-text
        raw_value = lv_text
        row = iv_row
        col = lv_col
      ) TO rt_token.

      lv_col += strlen( lv_text ).
      CLEAR lv_text.
    ENDWHILE.

    APPEND VALUE #(
      type = cs_token_type-text
      raw_value = cl_abap_char_utilities=>cr_lf
      row = iv_row
      col = lv_col
    ) TO rt_token.

  ENDMETHOD.


  METHOD class_constructor.
    mo_library_descr = CAST cl_abap_objectdescr(
      cl_abap_typedescr=>describe_by_name( '\CLASS=' && 'ZCL_AC_FUN_LIBRARY' )
    ).
  ENDMETHOD.


  METHOD tokenize_code.
    APPEND LINES OF tokenize_code_line(
      iv_code = iv_input
      iv_row = 1
      iv_col = 0
    ) TO rt_token.
  ENDMETHOD.


  METHOD tokenize_script.
    LOOP AT it_input ASSIGNING FIELD-SYMBOL(<lv_input>).
      DATA(lv_tabix) = sy-tabix.

      APPEND LINES OF tokenize_line(
        iv_input = <lv_input>
        iv_row = lv_tabix
      ) TO rt_token.
    ENDLOOP.

    DELETE rt_token WHERE type = cs_token_type-comment.

    LOOP AT rt_token ASSIGNING FIELD-SYMBOL(<ls_token>) FROM 2.
      CHECK <ls_token>-type = cs_token_type-space_type.
      DATA(lv_prev_type) = rt_token[ sy-tabix - 1 ]-type.

      CHECK lv_prev_type = cs_token_type-space_type
         OR lv_prev_type = cs_token_type-space_dup.

      <ls_token>-type = cs_token_type-space_dup.
    ENDLOOP.

    DELETE rt_token WHERE type = cs_token_type-space_dup.

    DATA(lv_last_token) = VALUE #( rt_token[ lines( rt_token ) ] OPTIONAL ).

    APPEND VALUE #(
      type = cs_token_type-eof
      row = lv_last_token-row
      col = lv_last_token-col + strlen( lv_last_token-raw_value )
    ) TO rt_token.
  ENDMETHOD.
ENDCLASS.
