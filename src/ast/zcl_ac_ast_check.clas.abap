class ZCL_AC_AST_CHECK definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_CHECK_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN
      !IO_COND type ref to ZCL_AC_AST_EVAL .

  methods EXECUTE
    redefinition .
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
