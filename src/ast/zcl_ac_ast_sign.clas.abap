CLASS zcl_ac_ast_sign DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_first_token TYPE zcl_ac_lexer=>tys_token
        !io_ast         TYPE REF TO zcl_ac_ast_eval.

    METHODS evaluate
        REDEFINITION .
    METHODS get_class
        REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_sign TYPE string.
    DATA mo_ast TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_AST_SIGN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
    mv_sign = is_first_token-raw_value.
    mo_ast = io_ast.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lv_mul) = COND #( WHEN mv_sign = '-' THEN -1 ELSE 1 ).

    DATA(lv_data) = mo_ast->evaluate( io_data_provider ).
    ASSIGN lv_data->* TO FIELD-SYMBOL(<lv_value>).

    CREATE DATA rv_value LIKE <lv_value>.
    ASSIGN rv_value->* TO FIELD-SYMBOL(<rv_value>).
    <rv_value> = lv_mul * <lv_value>.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.
ENDCLASS.
