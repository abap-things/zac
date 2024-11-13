class ZCL_AC_AST_EXIT definition
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

ENDCLASS.



CLASS ZCL_AC_AST_EXIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_text_token ).
  ENDMETHOD.


  METHOD execute.
    rv_exec_state = cs_exec_state-exit.
  ENDMETHOD.
ENDCLASS.
