CLASS zcl_ac_ast_stmt_list DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tyt_stmt TYPE STANDARD TABLE OF REF TO zcl_ac_ast_exec WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !it_stmt TYPE tyt_stmt.

    METHODS execute REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_stmt TYPE tyt_stmt.

ENDCLASS.



CLASS ZCL_AC_AST_STMT_LIST IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      COND #(
        WHEN it_stmt IS NOT INITIAL
        THEN it_stmt[ 1 ]->get_first_token( )
        ELSE VALUE zcl_ac_lexer=>tys_token( )
      )
    ).

    mt_stmt = it_stmt.
  ENDMETHOD.


  METHOD execute.
    LOOP AT mt_stmt ASSIGNING FIELD-SYMBOL(<lv_stms>).
      rv_exec_state = <lv_stms>->execute(
        io_writer        = io_writer
        io_data_provider = io_data_provider
      ).

      CHECK rv_exec_state = cs_exec_state-exit
         OR rv_exec_state = cs_exec_state-continue.

      RETURN.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
