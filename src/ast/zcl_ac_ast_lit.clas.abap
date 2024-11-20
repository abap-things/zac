CLASS zcl_ac_ast_lit DEFINITION
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
    DATA mv_value TYPE REF TO data.

ENDCLASS.



CLASS ZCL_AC_AST_LIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_token ).

    mv_value = is_token-typed_value.
  ENDMETHOD.


  METHOD evaluate.
    rv_value = mv_value.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.
ENDCLASS.
