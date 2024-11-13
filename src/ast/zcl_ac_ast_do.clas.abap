class ZCL_AC_AST_DO definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  types:
    BEGIN OF tys_if_branch,
        times     TYPE i,
        stmt_list TYPE REF TO zcl_ac_ast_stmt_list,
      END OF tys_if_branch .
  types:
    tyt_if_branch TYPE STANDARD TABLE OF tys_if_branch WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN
      !IO_TIMES type ref to ZCL_AC_AST_EVAL
      !IO_BODY type ref to ZCL_AC_AST_STMT_LIST .

  methods EXECUTE
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_times TYPE REF TO zcl_ac_ast_eval.
    DATA mo_body TYPE REF TO zcl_ac_ast_stmt_list.
ENDCLASS.



CLASS ZCL_AC_AST_DO IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
    mo_times = io_times.
    mo_body = io_body.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_times) = mo_times->evaluate( io_data_provider ).
    ASSIGN lv_times->* TO FIELD-SYMBOL(<lv_times>).

    DO <lv_times> TIMES.
      io_data_provider->set_value(
        iv_variable_name = 'SY_INDEX'
        iv_value         = sy-index
      ).

      DATA(lv_exec_state) = mo_body->execute(
        io_writer        = io_writer
        io_data_provider = io_data_provider
      ).

      IF lv_exec_state = cs_exec_state-exit.
        EXIT.
      ENDIF.
    ENDDO.

    rv_exec_state = cs_exec_state-complete.

  ENDMETHOD.
ENDCLASS.
