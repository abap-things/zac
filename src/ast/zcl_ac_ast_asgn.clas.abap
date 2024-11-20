CLASS zcl_ac_ast_asgn DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_var_token TYPE zcl_ac_lexer=>tys_token
        !iv_val_expr  TYPE REF TO zcl_ac_ast_eval .

    METHODS execute REDEFINITION .

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_var TYPE string.
    DATA mo_val_expr  TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_AST_ASGN IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_var_token ).
    mv_var = is_var_token-raw_value.
    mo_val_expr = iv_val_expr.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_data) = mo_val_expr->evaluate( io_data_provider ).
    ASSIGN lv_data->* TO FIELD-SYMBOL(<lv_value>).

    io_data_provider->set_value(
      iv_variable_name = mv_var
      iv_value         =  <lv_value>
    ).

    rv_exec_state = cs_exec_state-complete.
  ENDMETHOD.
ENDCLASS.
