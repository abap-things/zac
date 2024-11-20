CLASS zcl_ac_ast_text DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_text_token TYPE zcl_ac_lexer=>tys_token.

    METHODS execute REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_text TYPE string.

ENDCLASS.



CLASS ZCL_AC_AST_TEXT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_text_token ).
    mv_text = is_text_token-raw_value.
  ENDMETHOD.


  METHOD execute.
    io_writer->write( mv_text ).

    rv_exec_state = cs_exec_state-complete.
  ENDMETHOD.
ENDCLASS.
