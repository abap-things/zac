class ZCL_AC_AST definition
  public
  abstract
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .
  methods GET_FIRST_TOKEN
    returning
      value(RS_TOKEN) type ZCL_AC_LEXER=>TYS_TOKEN .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA ms_first_token TYPE zcl_ac_lexer=>tys_token.
ENDCLASS.



CLASS ZCL_AC_AST IMPLEMENTATION.


  METHOD constructor.
    ms_first_token = is_first_token.
  ENDMETHOD.


  METHOD get_first_token.
    rs_token = ms_first_token.
  ENDMETHOD.
ENDCLASS.
