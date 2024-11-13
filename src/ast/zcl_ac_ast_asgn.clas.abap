class ZCL_AC_AST_ASGN definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_VAR_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN
      !IV_VAL_EXPR type ref to ZCL_AC_AST_EVAL .

  methods EXECUTE
    redefinition .
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
