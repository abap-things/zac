CLASS zcl_ac_ast_cmp DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    METHODS constructor
      IMPORTING
        !iv_operator  TYPE string
        !io_left_ast  TYPE REF TO zcl_ac_ast_eval
        !io_right_ast TYPE REF TO zcl_ac_ast_eval.

    METHODS evaluate
        REDEFINITION .
    METHODS get_class
        REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_operator TYPE string.
    DATA mo_left_ast TYPE REF TO zcl_ac_ast_eval.
    DATA mo_right_ast TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_AST_CMP IMPLEMENTATION.


  METHOD constructor.
    super->constructor( io_left_ast->get_first_token( ) ).
    mv_operator = iv_operator.
    mo_left_ast = io_left_ast.
    mo_right_ast = io_right_ast.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lv_left_data) = mo_left_ast->evaluate( io_data_provider ).
    ASSIGN lv_left_data->* TO FIELD-SYMBOL(<lv_left_value>).

    DATA(lv_right_data) = mo_right_ast->evaluate( io_data_provider ).
    ASSIGN lv_right_data->* TO FIELD-SYMBOL(<lv_right_value>).

    CREATE DATA rv_value TYPE abap_bool.
    ASSIGN rv_value->* TO FIELD-SYMBOL(<rv_value>).

    TRY.
        CASE mv_operator.
          WHEN '='.
            <rv_value> = boolc( <lv_left_value> = <lv_right_value> ).
          WHEN '<>'.
            <rv_value> = boolc( <lv_left_value> <> <lv_right_value> ).
          WHEN '>'.
            <rv_value> = boolc( <lv_left_value> > <lv_right_value> ).
          WHEN '<'.
            <rv_value> = boolc( <lv_left_value> < <lv_right_value> ).
          WHEN '>='.
            <rv_value> = boolc( <lv_left_value> >= <lv_right_value> ).
          WHEN '<='.
            <rv_value> = boolc( <lv_left_value> <= <lv_right_value> ).
          WHEN 'CO'.
            <rv_value> = boolc( <lv_left_value> CO <lv_right_value> ).
          WHEN 'CN'.
            <rv_value> = boolc( <lv_left_value> CN <lv_right_value> ).
          WHEN 'CA'.
            <rv_value> = boolc( <lv_left_value> CA <lv_right_value> ).
          WHEN 'NA'.
            <rv_value> = boolc( <lv_left_value> NA <lv_right_value> ).
          WHEN 'CS'.
            <rv_value> = boolc( <lv_left_value> CS <lv_right_value> ).
          WHEN 'NS'.
            <rv_value> = boolc( <lv_left_value> NS <lv_right_value> ).
          WHEN 'CP'.
            <rv_value> = boolc( <lv_left_value> CP <lv_right_value> ).
          WHEN 'NP'.
            <rv_value> = boolc( <lv_left_value> NP <lv_right_value> ).
          WHEN OTHERS.
            ASSERT 1 = 0.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '006'
          WITH <lv_left_value> mv_operator <lv_right_value>.
    ENDTRY.

  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-logical.
  ENDMETHOD.
ENDCLASS.
