CLASS zcl_ac_expression DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS parse
      IMPORTING
        !iv_expr       TYPE string
      RETURNING
        VALUE(ro_expr) TYPE REF TO zcl_ac_expression
      RAISING
        zcx_ac_exception.

    METHODS evaluate
      IMPORTING
        !it_variable    TYPE zcl_ac_data_provider=>tyt_variable OPTIONAL
      RETURNING
        VALUE(rv_value) TYPE REF TO data.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_ast_eval TYPE REF TO zcl_ac_ast_eval.

    METHODS constructor
      IMPORTING
        io_ast_eval TYPE REF TO zcl_ac_ast_eval.

ENDCLASS.



CLASS ZCL_AC_EXPRESSION IMPLEMENTATION.


  METHOD constructor.
    mo_ast_eval = io_ast_eval.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lo_data_provider) = NEW zcl_ac_data_provider( ).

    LOOP AT it_variable ASSIGNING FIELD-SYMBOL(<ls_variable>).
      ASSIGN <ls_variable>-value->* TO FIELD-SYMBOL(<lv_value>).

      lo_data_provider->set_value(
        iv_variable_name = <ls_variable>-name
        iv_value         = <lv_value>
      ).
    ENDLOOP.

    rv_value = mo_ast_eval->evaluate( lo_data_provider ).
  ENDMETHOD.


  METHOD parse.
    ro_expr = NEW #( zcl_ac_parser=>parse_expression( iv_expr ) ).
  ENDMETHOD.
ENDCLASS.
