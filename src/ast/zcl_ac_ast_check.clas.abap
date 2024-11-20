CLASS zcl_ac_ast_check DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_check_token TYPE zcl_ac_lexer=>tys_token
        !io_cond        TYPE REF TO zcl_ac_ast_eval.

    METHODS execute REDEFINITION .

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_cond TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_AST_CHECK IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_check_token ).
    mo_cond = io_cond.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_value) = mo_cond->evaluate( io_data_provider ).
    ASSIGN lv_value->* TO FIELD-SYMBOL(<lv_value>).

    IF <lv_value> = abap_false.
      rv_exec_state = cs_exec_state-continue.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
