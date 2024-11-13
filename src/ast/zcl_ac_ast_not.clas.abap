class ZCL_AC_AST_NOT definition
  public
  inheriting from ZCL_AC_AST_EVAL
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN
      !IO_AST type ref to ZCL_AC_AST_EVAL .

  methods EVALUATE
    redefinition .
  methods GET_CLASS
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_ast TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_AST_NOT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
    mo_ast = io_ast.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lv_data) = mo_ast->evaluate( io_data_provider ).
    ASSIGN lv_data->* TO FIELD-SYMBOL(<lv_value>).

    CREATE DATA rv_value LIKE <lv_value>.
    ASSIGN rv_value->* TO FIELD-SYMBOL(<rv_value>).

    IF <lv_value> = abap_true.
      <rv_value> = abap_false.
    ELSEIF <lv_value> = abap_false.
      <rv_value> = abap_true.
    ELSE.
      ASSERT 1 = 0.
    ENDIF.

  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-logical.
  ENDMETHOD.
ENDCLASS.
