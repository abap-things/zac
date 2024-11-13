CLASS zcl_ac_ast_exec DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cs_exec_state,
        complete TYPE i VALUE 0,
        exit     TYPE i VALUE 1,
        continue TYPE i VALUE 2,
      END OF cs_exec_state.

    METHODS constructor
      IMPORTING
        is_first_token TYPE zcl_ac_lexer=>tys_token.

    METHODS execute
      ABSTRACT
      IMPORTING
        !io_writer           TYPE REF TO cl_abap_string_c_writer
        !io_data_provider    TYPE REF TO zcl_ac_data_provider
      RETURNING
        VALUE(rv_exec_state) TYPE i
      RAISING
        zcx_ac_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AC_AST_EXEC IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
  ENDMETHOD.
ENDCLASS.
