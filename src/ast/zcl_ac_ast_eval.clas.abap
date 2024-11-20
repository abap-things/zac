CLASS zcl_ac_ast_eval DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cs_class,
        value   TYPE string VALUE 'VALUE',
        logical TYPE string VALUE 'LOGICAL',
      END OF cs_class .

    METHODS evaluate
      ABSTRACT
      IMPORTING
        !io_data_provider TYPE REF TO zcl_ac_data_provider
      RETURNING
        VALUE(rv_value)   TYPE REF TO data
      RAISING
        zcx_ac_exception.

    METHODS get_class
      ABSTRACT
      RETURNING
        VALUE(rv_class) TYPE string.

    METHODS constructor
      IMPORTING
        !is_first_token TYPE zcl_ac_lexer=>tys_token.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AC_AST_EVAL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
  ENDMETHOD.
ENDCLASS.
