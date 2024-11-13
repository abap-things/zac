CLASS zcl_ac_ast_do DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF tys_if_branch,
        times     TYPE i,
        stmt_list TYPE REF TO zcl_ac_ast_stmt_list,
      END OF tys_if_branch .
    TYPES:
      tyt_if_branch TYPE STANDARD TABLE OF tys_if_branch WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        !is_first_token TYPE zcl_ac_lexer=>tys_token
        !io_times       TYPE REF TO zcl_ac_ast_eval
        !io_body        TYPE REF TO zcl_ac_ast_stmt_list .

    METHODS execute
        REDEFINITION .
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
