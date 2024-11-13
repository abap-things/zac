class ZCL_AC_AST_TEXT definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_TEXT_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .

  methods EXECUTE
    redefinition .
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
