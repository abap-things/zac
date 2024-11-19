* BNF, LL(3) grammar

*<text> ::= TOKEN(text)
*<subst> ::= TOKEN(subst)
*<variable> ::= TOKEN(variable)
*<full_stop> ::= TOKEN(".")
*<space> ::= TOKEN(space)
*<fun> ::= TOKEN(fun)

*<root> ::= <stmt_list><eof>
*<stms_list> ::= (<text>|<subst>|<asgn>|<full_stop>|<space>|<if>|<loop>|<do>|<exit>|<continue>|<check>)*[<eof>]
*<asgn> ::= <variable><space>"="<space><expr>[<space>]"."
*<loop> ::=
*  "LOOP AT"<space><variable><space>"INTO"<space>"DATA("<variable>")"
*  [<space>"WHERE"<space><expr>][<space>]"."
*  <stmt_list>
* "ENLOOP"[<space>]"."
*<do> ::=
*  "DO"<space><expr><space>"TIMES"[<space>]"."
*  <stmt_list>
* "ENDDO"[<space>]"."
*<if> ::=
*  "IF"<space><expr>[<space>]"."<space><stmt_list>
*  ["ELSEIF"<space><expr>[<space>]"."<space><stmt_list>]*
*  ["ELSE"[<space>]"."<space><stmt_list>]
*  "ENDIF"[<space>]"."
*<check> ::= "CHECK"<space><expr>[<space>]"."
*<exit> ::= "EXIT"[<space>]"."
*<continue> ::= "CONTINUE"[<space>]"."
*<expr_prio_00> ::= "("<space><expr_prio_70><space>")"|<fun_call>|<variable>|<literal>
*<expr_prio_10> ::= ["+"|"-"][<space>]<expr_prio_00>
*<expr_prio_20> ::= <expr_prio_10>[<space>("*"|"/")<space><expr_prio_10>]*
*<expr_prio_30> ::= <expr_prio_20>[<space>("+"|"-")<space><expr_prio_20>]*
*<expr_prio_40> ::= <expr_prio_30>|<expr_prio_30><space>("="|"<>"|"<"|">"|"<="|">="|"CO"|"CN"|"CA"|"NA"|"CS<"|"NS"|"CP"|"NP")<space><expr_prio_30>
*<expr_prio_50> ::= "NOT"<space><expr_prio_50>|<expr_prio_40>
*<expr_prio_60> ::= <expr_prio_50>[<space>"AND"<space><expr_prio_50>]*
*<expr_prio_70> ::= <expr_prio_60>[<space>"OR"<space><expr_prio_60>]*
*<fun_call> ::= <fun>"("<space>[<args><space>])")"[<space>].
*<args> ::= <named_param>*|<expr>                                 "inlined in PARSE_FUN_CALL method
*<named_param> ::= <variable><space>=<expr>|<space><named_param>  "inlined in PARSE_FUN_CALL method

CLASS zcl_ac_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS parse_script
      IMPORTING
        !it_input     TYPE zcl_ac_lexer=>tyt_input
      RETURNING
        VALUE(rv_ast) TYPE REF TO zcl_ac_ast_exec
      RAISING
        zcx_ac_exception .

    CLASS-METHODS parse_expression
      IMPORTING
        !iv_expression TYPE string
      RETURNING
        VALUE(rv_ast)  TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .


  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF cs_input_type,
        expression   TYPE i VALUE 0,
        script TYPE i VALUE 1,
      END OF cs_input_type.

    CONSTANTS cs_token_type LIKE zcl_ac_lexer=>cs_token_type VALUE zcl_ac_lexer=>cs_token_type ##NO_TEXT.

    DATA mt_token TYPE zcl_ac_lexer=>tyt_token .
    DATA mv_pos TYPE i .
    DATA mv_loop_deep TYPE i .

    METHODS constructor
      IMPORTING
        !iv_input_type TYPE i
        !it_input      TYPE zcl_ac_lexer=>tyt_input
      RAISING
        zcx_ac_exception .
    METHODS parse_expr
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_stmt_list
      RETURNING
        VALUE(rv_ast) TYPE REF TO zcl_ac_ast_stmt_list
      RAISING
        zcx_ac_exception .
    METHODS parse_root
      RETURNING
        VALUE(rv_ast) TYPE REF TO zcl_ac_ast_exec
      RAISING
        zcx_ac_exception .
    METHODS parse_text
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_text
      RAISING
        zcx_ac_exception .
    METHODS parse_check
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_check
      RAISING
        zcx_ac_exception .
    METHODS parse_continue
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_continue
      RAISING
        zcx_ac_exception .
    METHODS parse_fun_call
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_fun_call
      RAISING
        zcx_ac_exception .
    METHODS parse_exit
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_exit
      RAISING
        zcx_ac_exception .
    METHODS parse_subst
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_subst
      RAISING
        zcx_ac_exception .
    METHODS parse_loop
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_loop
      RAISING
        zcx_ac_exception .
    METHODS parse_do
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_do
      RAISING
        zcx_ac_exception .
    METHODS parse_if
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_if
      RAISING
        zcx_ac_exception .
    METHODS parse_asgn
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_asgn
      RAISING
        zcx_ac_exception .

    METHODS parse_expr_prio_00
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_10
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_70
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_60
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_20
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_30
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_40
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS parse_expr_prio_50
      RETURNING
        VALUE(ro_ast) TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS expect_token
      IMPORTING
        !iv_expected_type      TYPE string
        !iv_expected_raw_value TYPE string OPTIONAL
      RETURNING
        VALUE(rs_token)        TYPE zcl_ac_lexer=>tys_token
      RAISING
        zcx_ac_exception .
    METHODS expect_and_eat_keyword
      IMPORTING
        !iv_expected_raw_value TYPE string OPTIONAL
      RETURNING
        VALUE(rs_token)        TYPE zcl_ac_lexer=>tys_token
      RAISING
        zcx_ac_exception .
    METHODS expect_value_ast
      IMPORTING
        !io_ast TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS expect_logical_ast
      IMPORTING
        !io_ast TYPE REF TO zcl_ac_ast_eval
      RAISING
        zcx_ac_exception .
    METHODS expect_and_eat_token
      IMPORTING
        !iv_expected_type      TYPE string
        !iv_expected_raw_value TYPE string OPTIONAL
      RETURNING
        VALUE(rs_token)        TYPE zcl_ac_lexer=>tys_token
      RAISING
        zcx_ac_exception .
    METHODS ignore_token
      IMPORTING
        !iv_ignored_type TYPE string .
    METHODS peek_token1st
      RETURNING
        VALUE(rs_token) TYPE zcl_ac_lexer=>tys_token .
    METHODS peek_token2nd
      RETURNING
        VALUE(rs_token) TYPE zcl_ac_lexer=>tys_token .
    METHODS peek_token3d
      RETURNING
        VALUE(rs_token) TYPE zcl_ac_lexer=>tys_token .
    METHODS eat_token
      RETURNING
        VALUE(rs_token) TYPE zcl_ac_lexer=>tys_token .
ENDCLASS.



CLASS ZCL_AC_PARSER IMPLEMENTATION.


  METHOD peek_token2nd.
    IF mv_pos + 1 <= lines( mt_token ).
      rs_token = mt_token[ mv_pos + 1 ].
    ELSE.
      rs_token = VALUE #( type = cs_token_type-eof ).
    ENDIF.
  ENDMETHOD.


  METHOD peek_token1st.
    rs_token = mt_token[ mv_pos ].
  ENDMETHOD.


  METHOD parse_text.
    ro_ast = NEW zcl_ac_ast_text( expect_and_eat_token( cs_token_type-text ) ).
  ENDMETHOD.


  METHOD parse_subst.
    ro_ast = NEW zcl_ac_ast_subst( expect_and_eat_token( cs_token_type-subst ) ).
  ENDMETHOD.


  METHOD parse_stmt_list.
    DATA(lt_stmt) = VALUE zcl_ac_ast_stmt_list=>tyt_stmt( ).

    DO.
      DATA(ls_token) = peek_token1st( ).

      DATA(lv_pos_before) = mv_pos.

      CASE ls_token-type.
        WHEN cs_token_type-eof.
          EXIT.
        WHEN cs_token_type-text.
          APPEND parse_text( ) TO lt_stmt.
        WHEN cs_token_type-subst.
          APPEND parse_subst( ) TO lt_stmt.
        WHEN cs_token_type-variable.
          APPEND parse_asgn( ) TO lt_stmt.
        WHEN cs_token_type-full_stop.
          eat_token( ).
        WHEN cs_token_type-space_type.
          eat_token( ).
        WHEN cs_token_type-keyword.
          CASE ls_token-raw_value.
            WHEN 'IF'.
              APPEND parse_if( ) TO lt_stmt.
            WHEN 'LOOP'.
              APPEND parse_loop( ) TO lt_stmt.
            WHEN 'DO'.
              APPEND parse_do( ) TO lt_stmt.
            WHEN OTHERS.
              IF mv_loop_deep > 0.
                CASE ls_token-raw_value.
                  WHEN 'EXIT'.
                    APPEND parse_exit( ) TO lt_stmt.
                  WHEN 'CONTINUE'.
                    APPEND parse_continue( ) TO lt_stmt.
                  WHEN 'CHECK'.
                    APPEND parse_check( ) TO lt_stmt.
                ENDCASE.
              ENDIF.
          ENDCASE.
      ENDCASE.

      IF lv_pos_before = mv_pos.
        EXIT.
      ENDIF.
    ENDDO.

    rv_ast = NEW zcl_ac_ast_stmt_list( lt_stmt ).
  ENDMETHOD.


  METHOD parse_root.
    rv_ast = parse_stmt_list( ).
    expect_token( cs_token_type-eof ).
  ENDMETHOD.


  METHOD parse_loop.
    mv_loop_deep += 1.

    DATA(ls_loop_token) = expect_and_eat_keyword( 'LOOP' ).
    expect_and_eat_token( cs_token_type-space_type ).
    expect_and_eat_keyword( 'AT' ).
    expect_and_eat_token( cs_token_type-space_type ).
    DATA(ls_table_variable_token) = expect_and_eat_token( cs_token_type-variable ).

    expect_and_eat_token( cs_token_type-space_type ).
    expect_and_eat_keyword( 'INTO' ).
    expect_and_eat_token( cs_token_type-space_type ).
    expect_and_eat_keyword( 'DATA' ).
    expect_and_eat_token( cs_token_type-opening_bracket ).
    DATA(ls_line_variable_token) = expect_and_eat_token( cs_token_type-variable ).
    expect_and_eat_token( cs_token_type-closing_bracket ).
    ignore_token( cs_token_type-space_type ).

    IF peek_token1st( )-type = cs_token_type-keyword AND peek_token1st( )-raw_value = 'WHERE'.
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).
      DATA(lo_cond) = parse_expr( ).
      expect_logical_ast( io_ast = lo_cond ).
      ignore_token( cs_token_type-space_type ).
    ENDIF.

    expect_and_eat_token( cs_token_type-full_stop ).

    DATA(lo_body) = parse_stmt_list( ).

    expect_and_eat_keyword( 'ENDLOOP' ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).

    ro_ast = NEW zcl_ac_ast_loop(
      is_first_token    = ls_loop_token
      iv_table_variable = ls_table_variable_token-raw_value
      iv_line_variable  = ls_line_variable_token-raw_value
      io_cond           = lo_cond
      io_body           = lo_body
    ).

    mv_loop_deep -= 1.
  ENDMETHOD.


  METHOD parse_if.
    DATA(ls_if_token) = expect_and_eat_keyword( 'IF' ).
    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lo_cond) = parse_expr( ).
    expect_logical_ast( lo_cond ).

    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).
    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lt_if_branch) = VALUE zcl_ac_ast_if=>tyt_if_branch( (
      cond = lo_cond stmt_list =  parse_stmt_list( )
    ) ).

    DATA(ls_elseif_token) = peek_token1st( ).
    WHILE ls_elseif_token-type = cs_token_type-keyword AND ls_elseif_token-raw_value = 'ELSEIF'.
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      lo_cond = parse_expr( ).
      expect_logical_ast( lo_cond ).

      ignore_token( cs_token_type-space_type ).
      expect_and_eat_token( cs_token_type-full_stop ).
      expect_and_eat_token( cs_token_type-space_type ).

      APPEND VALUE #( cond = lo_cond stmt_list =  parse_stmt_list( ) ) TO lt_if_branch.

      ls_elseif_token = peek_token1st( ).
    ENDWHILE.

    DATA(ls_else_token) = peek_token1st( ).
    IF ls_else_token-type = cs_token_type-keyword AND ls_else_token-raw_value = 'ELSE'.
      eat_token( ).
      ignore_token( cs_token_type-space_type ).
      expect_and_eat_token( cs_token_type-full_stop ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_else_branch) = parse_stmt_list( ).
    ENDIF.

    expect_and_eat_keyword( 'ENDIF' ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).

    ro_ast = NEW zcl_ac_ast_if(
      is_first_token = ls_if_token
      it_if_branch   = lt_if_branch
      io_else_branch = lo_else_branch
    ).
  ENDMETHOD.


  METHOD parse_expr_prio_70.
    DATA(lo_ast) = parse_expr_prio_60( ).
    DATA(lt_ast) = VALUE zcl_ac_ast_and=>tyt_ast( ( lo_ast ) ).

    DATA(ls_token1st) = peek_token1st( ).
    DATA(ls_token2nd) = peek_token2nd( ).

    WHILE ls_token1st-type = cs_token_type-space_type
      AND ls_token2nd-type = cs_token_type-operator
      AND ls_token2nd-raw_value = 'OR'.

      eat_token( ).
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_co_ast) = parse_expr_prio_60( ).
      expect_logical_ast( lo_co_ast ).
      APPEND lo_co_ast  TO lt_ast.

      ls_token1st = peek_token1st( ).
      ls_token2nd = peek_token2nd( ).
    ENDWHILE.

    IF lines( lt_ast ) = 1.
      ro_ast = lo_ast.
    ELSE.
      ro_ast = NEW zcl_ac_ast_or( lt_ast ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_expr_prio_60.
    DATA(lo_ast) = parse_expr_prio_50( ).
    DATA(lt_ast) = VALUE zcl_ac_ast_and=>tyt_ast( ( lo_ast ) ).

    DATA(ls_token1st) = peek_token1st( ).
    DATA(ls_token2nd) = peek_token2nd( ).

    WHILE ls_token1st-type = cs_token_type-space_type
      AND ls_token2nd-type = cs_token_type-operator
      AND ls_token2nd-raw_value = 'AND'.

      eat_token( ).
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_co_ast) = parse_expr_prio_50( ).
      expect_logical_ast( lo_co_ast ).
      APPEND lo_co_ast  TO lt_ast.

      ls_token1st = peek_token1st( ).
      ls_token2nd = peek_token2nd( ).
    ENDWHILE.

    IF lines( lt_ast ) = 1.
      ro_ast = lo_ast.
    ELSE.
      ro_ast = NEW zcl_ac_ast_and( lt_ast ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_expr_prio_50.
    DATA(ls_token) = peek_token1st( ).

    IF ls_token-type = cs_token_type-operator AND ls_token-raw_value = 'NOT'.
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_ast) = parse_expr_prio_50( ).
      expect_logical_ast( lo_ast ).

      ro_ast = NEW zcl_ac_ast_not(
        is_first_token = ls_token
        io_ast         = lo_ast
      ).

      RETURN.
    ENDIF.

    ro_ast = parse_expr_prio_40( ).
  ENDMETHOD.


  METHOD parse_expr_prio_40.
    DATA(lo_left_ast) = parse_expr_prio_30( ).

    DATA(ls_token1st) = peek_token1st( ).
    DATA(ls_token2nd) = peek_token2nd( ).

    IF ls_token1st-type = cs_token_type-space_type
       AND ls_token2nd-type = cs_token_type-operator
       AND ( ls_token2nd-raw_value = '='  OR ls_token2nd-raw_value = '<>' OR
             ls_token2nd-raw_value = '>'  OR ls_token2nd-raw_value = '<'  OR
             ls_token2nd-raw_value = '>=' OR ls_token2nd-raw_value = '<=' OR
             ls_token2nd-raw_value = 'CO' OR ls_token2nd-raw_value = 'CN' OR
             ls_token2nd-raw_value = 'CA' OR ls_token2nd-raw_value = 'NA' OR
             ls_token2nd-raw_value = 'CS' OR ls_token2nd-raw_value = 'NS' OR
             ls_token2nd-raw_value = 'CP' OR ls_token2nd-raw_value = 'NP'
      ).

      eat_token( ).
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_right_ast) = parse_expr_prio_30( ).
      expect_value_ast( lo_right_ast ).

      ro_ast = NEW zcl_ac_ast_cmp(
        iv_operator  = ls_token2nd-raw_value
        io_left_ast  = lo_left_ast
        io_right_ast = lo_right_ast
      ).

      RETURN.
    ENDIF.

    ro_ast = lo_left_ast.
  ENDMETHOD.


  METHOD parse_expr_prio_30.
    DATA(lo_ast) = parse_expr_prio_20( ).
    DATA(lt_co_ast) = VALUE zcl_ac_ast_mul=>tyt_co_ast(  ).

    DATA(ls_token1st) = peek_token1st( ).
    DATA(ls_token2nd) = peek_token2nd( ).

    WHILE ls_token1st-type = cs_token_type-space_type
      AND ls_token2nd-type = cs_token_type-operator
      AND ( ls_token2nd-raw_value = '+' OR ls_token2nd-raw_value = '-').

      eat_token( ).
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_co_ast) = parse_expr_prio_20( ).
      expect_value_ast( lo_co_ast ).
      APPEND VALUE #( operator = ls_token2nd-raw_value ast = lo_co_ast ) TO lt_co_ast.

      ls_token1st = peek_token1st( ).
      ls_token2nd = peek_token2nd( ).
    ENDWHILE.

    IF lt_co_ast IS INITIAL.
      ro_ast = lo_ast.
    ELSE.
      ro_ast = NEW zcl_ac_ast_sum(
        io_ast    = lo_ast
        it_co_ast = lt_co_ast
      ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_expr_prio_20.
    DATA(lo_ast) = parse_expr_prio_10( ).
    DATA(lt_co_ast) = VALUE zcl_ac_ast_mul=>tyt_co_ast(  ).

    DATA(ls_token1st) = peek_token1st( ).
    DATA(ls_token2nd) = peek_token2nd( ).

    WHILE ls_token1st-type = cs_token_type-space_type
      AND ls_token2nd-type = cs_token_type-operator
      AND ( ls_token2nd-raw_value = '*' OR ls_token2nd-raw_value = '/').

      eat_token( ).
      eat_token( ).
      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_co_ast) = parse_expr_prio_10( ).
      expect_value_ast( lo_co_ast ).
      APPEND VALUE #( operator = ls_token2nd-raw_value ast = lo_co_ast ) TO lt_co_ast.

      ls_token1st = peek_token1st( ).
      ls_token2nd = peek_token2nd( ).
    ENDWHILE.

    IF lt_co_ast IS INITIAL.
      ro_ast = lo_ast.
    ELSE.
      ro_ast = NEW zcl_ac_ast_mul(
        io_ast    = lo_ast
        it_co_ast = lt_co_ast
      ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_expr_prio_10.
    DATA(ls_token) = peek_token1st( ).

    IF ls_token-type = cs_token_type-operator
    AND ( ls_token-raw_value = '+' OR ls_token-raw_value = '-').
      eat_token( ).
      ignore_token( cs_token_type-space_type ).

      ro_ast = NEW zcl_ac_ast_sign(
        is_first_token = ls_token
        io_ast         = parse_expr_prio_00( )
      ).

      RETURN.
    ENDIF.

    ro_ast = parse_expr_prio_00( ).
  ENDMETHOD.


  METHOD parse_expr_prio_00.
    DATA(ls_token) = peek_token1st( ).

    CASE ls_token-type.
      WHEN cs_token_type-opening_bracket.
        eat_token( ).
        expect_and_eat_token( cs_token_type-space_type ).
        ro_ast = parse_expr_prio_70( ).
        expect_and_eat_token( cs_token_type-space_type ).
        expect_and_eat_token( cs_token_type-closing_bracket ).

      WHEN cs_token_type-variable.
        ro_ast = NEW zcl_ac_ast_var( eat_token( ) ).

      WHEN cs_token_type-fun.
        ro_ast = parse_fun_call( ).

      WHEN OTHERS.
        ro_ast = NEW zcl_ac_ast_lit( expect_and_eat_token( cs_token_type-literal ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD parse_expr.
    ro_ast = parse_expr_prio_70( ).
  ENDMETHOD.


  METHOD parse_asgn.
    DATA(ls_var_token) = expect_and_eat_token( cs_token_type-variable ).
    expect_and_eat_token( cs_token_type-space_type ).

    expect_and_eat_token(
      iv_expected_type      = cs_token_type-operator
      iv_expected_raw_value = '='
    ).

    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lo_expr) = parse_expr( ).
    expect_value_ast( lo_expr ).

    ro_ast = NEW zcl_ac_ast_asgn(
      is_var_token = ls_var_token
      iv_val_expr  = lo_expr
    ) .

    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).
  ENDMETHOD.


  METHOD ignore_token.
    IF peek_token1st( )-type = iv_ignored_type.
      eat_token( ).
    ENDIF.
  ENDMETHOD.


  METHOD expect_value_ast.
    IF io_ast->get_class( ) <> zcl_ac_ast_eval=>cs_class-value.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '006'
        WITH
            io_ast->get_first_token( )-row io_ast->get_first_token( )-col.
    ENDIF.
  ENDMETHOD.


  METHOD expect_token.
    rs_token = peek_token1st( ).

    IF iv_expected_raw_value IS NOT INITIAL
    AND rs_token-raw_value <> iv_expected_raw_value.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '005'
        WITH rs_token-raw_value rs_token-row rs_token-col iv_expected_raw_value.
    ENDIF.

    IF rs_token-type <> iv_expected_type.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '002'
        WITH rs_token-type rs_token-row rs_token-col iv_expected_type.
    ENDIF.
  ENDMETHOD.


  METHOD expect_logical_ast.
    IF io_ast->get_class( ) <> zcl_ac_ast_eval=>cs_class-logical.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '009'
        WITH
            io_ast->get_first_token( )-row io_ast->get_first_token( )-col.
    ENDIF.
  ENDMETHOD.


  METHOD expect_and_eat_token.
    rs_token = expect_token(
      iv_expected_type      = iv_expected_type
      iv_expected_raw_value = iv_expected_raw_value
    ).

    eat_token( ).
  ENDMETHOD.


  METHOD expect_and_eat_keyword.
    rs_token = expect_and_eat_token(
      iv_expected_type      = cs_token_type-keyword
      iv_expected_raw_value = iv_expected_raw_value
    ).
  ENDMETHOD.


  METHOD eat_token.
    rs_token = peek_token1st( ).
    mv_pos += 1.
  ENDMETHOD.


  METHOD constructor.
    IF iv_input_type = cs_input_type-script.
      mt_token = zcl_ac_lexer=>tokenize_script( it_input ).
    ELSE.
      mt_token = zcl_ac_lexer=>tokenize_code( it_input[ 1 ] ).
    ENDIF.

    IF line_exists( mt_token[ type = cs_token_type-unexpected ] ).
      DATA(ls_unexpected_token) = mt_token[ type = cs_token_type-unexpected ].

      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '001'
        WITH ls_unexpected_token-raw_value ls_unexpected_token-row ls_unexpected_token-col.
    ENDIF.

    mv_pos = 1.
    mv_loop_deep = 0.
  ENDMETHOD.


  METHOD parse_exit.
    ro_ast = NEW zcl_ac_ast_exit( expect_and_eat_keyword( 'EXIT' ) ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).
  ENDMETHOD.


  METHOD parse_continue.
    ro_ast = NEW zcl_ac_ast_continue( expect_and_eat_keyword( 'CONTINUE' ) ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).
  ENDMETHOD.


  METHOD parse_check.
    DATA(ls_check_token) = expect_and_eat_keyword( 'CHECK' ).
    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lo_ast) = parse_expr( ).
    expect_logical_ast( lo_ast ).

    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).

    ro_ast = NEW zcl_ac_ast_check(
      is_check_token = ls_check_token
      io_cond        = lo_ast
    ).
  ENDMETHOD.


  METHOD parse_fun_call.
    DATA(ls_fun_token) = expect_and_eat_token( cs_token_type-fun ).
    expect_and_eat_token( cs_token_type-opening_bracket ).
    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lt_named_param) = VALUE zcl_ac_ast_fun_call=>tyt_named_param( ).
    DATA(ls_first_token) = peek_token1st( ).
    DATA(ls_space_token) = peek_token2nd( ).
    DATA(ls_eq_token) = peek_token3d( ).

    WHILE ls_first_token-type = cs_token_type-variable
      AND ls_space_token-type = cs_token_type-space_type
      AND ls_eq_token-raw_value = '='.

      eat_token( ).
      eat_token( ).
      eat_token( ).

      expect_and_eat_token( cs_token_type-space_type ).

      DATA(lo_ast) = parse_expr( ).
      expect_value_ast( lo_ast ).

      APPEND VALUE #(
        name = ls_first_token-raw_value
        ast = lo_ast
      ) TO lt_named_param.

      expect_and_eat_token( cs_token_type-space_type ).

      ls_first_token = peek_token1st( ).
      ls_space_token = peek_token2nd( ).
      ls_eq_token = peek_token3d( ).
    ENDWHILE.

    IF lt_named_param IS NOT INITIAL.
      expect_and_eat_token( cs_token_type-closing_bracket ).

      ro_ast = NEW #(
        is_function_token = ls_fun_token
        it_named_param = lt_named_param
      ).

      RETURN.
    ENDIF.

    IF ls_first_token-type = cs_token_type-closing_bracket.
      eat_token( ).

      ro_ast = NEW #( is_function_token = ls_fun_token ).
      RETURN.
    ENDIF.

    lo_ast = parse_expr( ).
    expect_value_ast( lo_ast ).
    expect_and_eat_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-closing_bracket ).

    ro_ast = NEW #(
      is_function_token = ls_fun_token
      io_unnamed_param = lo_ast
    ).
  ENDMETHOD.


  METHOD peek_token3d.
    IF mv_pos + 2 <= lines( mt_token ).
      rs_token = mt_token[ mv_pos + 2 ].
    ELSE.
      rs_token = VALUE #( type = cs_token_type-eof ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_do.
    mv_loop_deep += 1.

    DATA(ls_do_token) = expect_and_eat_keyword( 'DO' ).
    expect_and_eat_token( cs_token_type-space_type ).

    DATA(lo_times) = parse_expr( ).
    expect_value_ast( lo_times ).

    expect_and_eat_token( cs_token_type-space_type ).
    expect_and_eat_keyword( 'TIMES' ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).

    DATA(lo_body) = parse_stmt_list( ).

    expect_and_eat_keyword( 'ENDDO' ).
    ignore_token( cs_token_type-space_type ).
    expect_and_eat_token( cs_token_type-full_stop ).

    ro_ast = NEW zcl_ac_ast_do(
      is_first_token = ls_do_token
      io_times       = lo_times
      io_body        = lo_body
    ).

    mv_loop_deep -= 1.
  ENDMETHOD.


  METHOD parse_expression.
    DATA(lo_instanse) = NEW zcl_ac_parser(
      iv_input_type = cs_input_type-expression
      it_input = VALUE #( ( iv_expression ) )
    ).

    rv_ast = lo_instanse->parse_expr( ).
  ENDMETHOD.


  METHOD parse_script.
    DATA(lo_instanse) = NEW zcl_ac_parser(
      iv_input_type = cs_input_type-script
      it_input = it_input
    ).

    rv_ast = lo_instanse->parse_root( ).
  ENDMETHOD.
ENDCLASS.
