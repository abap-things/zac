CLASS zcl_ac_ast DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_first_token TYPE zcl_ac_lexer=>tys_token.

    METHODS get_first_token
      RETURNING
        VALUE(rs_token) TYPE zcl_ac_lexer=>tys_token.

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
