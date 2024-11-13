CLASS zcl_ac_ast_not DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_first_token TYPE zcl_ac_lexer=>tys_token
        !io_ast         TYPE REF TO zcl_ac_ast_eval.

    METHODS evaluate
        REDEFINITION .
    METHODS get_class
        REDEFINITION .
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
