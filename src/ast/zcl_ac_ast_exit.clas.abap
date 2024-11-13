CLASS zcl_ac_ast_exit DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_text_token TYPE zcl_ac_lexer=>tys_token .

    METHODS execute
        REDEFINITION .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AC_AST_EXIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_text_token ).
  ENDMETHOD.


  METHOD execute.
    rv_exec_state = cs_exec_state-exit.
  ENDMETHOD.
ENDCLASS.
