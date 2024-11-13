CLASS zcl_ac_ast_mul DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF tys_co_ast,
        operator TYPE string,
        ast       TYPE REF TO zcl_ac_ast_eval,
      END OF tys_co_ast.

    TYPES tyt_co_ast TYPE STANDARD TABLE OF tys_co_ast WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !io_ast   TYPE REF TO zcl_ac_ast_eval
        it_co_ast TYPE tyt_co_ast.

    METHODS evaluate
        REDEFINITION .
    METHODS get_class
        REDEFINITION .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_ast TYPE REF TO zcl_ac_ast_eval.
    DATA mt_co_ast TYPE tyt_co_ast.

ENDCLASS.



CLASS ZCL_AC_AST_MUL IMPLEMENTATION.


  METHOD constructor.
    super->constructor( io_ast->get_first_token( ) ).
    mo_ast = io_ast.
    mt_co_ast = it_co_ast.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lv_data) = mo_ast->evaluate( io_data_provider ).
    ASSIGN lv_data->* TO FIELD-SYMBOL(<lv_value>).

    CREATE DATA rv_value LIKE <lv_value>.
    ASSIGN rv_value->* TO FIELD-SYMBOL(<rv_value>).
    <rv_value> = <lv_value>.

    LOOP AT mt_co_ast ASSIGNING FIELD-SYMBOL(<ls_co_ast>).
      DATA(lv_right) = <ls_co_ast>-ast->evaluate( io_data_provider ).
      ASSIGN lv_right->* TO FIELD-SYMBOL(<lv_right>).

      CASE <ls_co_ast>-operator.
        WHEN '*'.
          <rv_value> = <rv_value> * <lv_right>.
        WHEN '/'.
          <rv_value> = <rv_value> / <lv_right>.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.
ENDCLASS.
