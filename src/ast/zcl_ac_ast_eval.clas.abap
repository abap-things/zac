class ZCL_AC_AST_EVAL definition
  public
  inheriting from ZCL_AC_AST
  abstract
  create public .

public section.

  constants:
    BEGIN OF cs_class,
        value   TYPE string VALUE 'VALUE',
        logical TYPE string VALUE 'LOGICAL',
      END OF cs_class .

  methods EVALUATE
  abstract
    importing
      !IO_DATA_PROVIDER type ref to ZCL_AC_DATA_PROVIDER
    returning
      value(RV_VALUE) type ref to DATA
    raising
      ZCX_AC_EXCEPTION .
  methods GET_CLASS
  abstract
    returning
      value(RV_CLASS) type STRING .
  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AC_AST_EVAL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
  ENDMETHOD.
ENDCLASS.
