class ZCL_AC_AST_LIT definition
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
    DATA mv_value TYPE REF TO data .
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
