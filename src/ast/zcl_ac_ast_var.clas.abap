class ZCL_AC_AST_VAR definition
  public
  inheriting from ZCL_AC_AST_EVAL
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .

  methods EVALUATE
    redefinition .
  methods GET_CLASS
    redefinition .
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
