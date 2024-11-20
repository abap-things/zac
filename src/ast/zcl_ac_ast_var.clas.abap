CLASS zcl_ac_ast_var DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !is_token TYPE zcl_ac_lexer=>tys_token.

    METHODS evaluate REDEFINITION.
    METHODS get_class REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_var TYPE string.

ENDCLASS.



CLASS ZCL_AC_AST_VAR IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_token ).
    mv_var = is_token-raw_value.
  ENDMETHOD.


  METHOD evaluate.
    rv_value = io_data_provider->get_value( mv_var ).
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.
ENDCLASS.
