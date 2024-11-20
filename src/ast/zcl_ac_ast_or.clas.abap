CLASS zcl_ac_ast_or DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tyt_ast TYPE STANDARD TABLE OF REF TO zcl_ac_ast_eval WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !it_ast TYPE tyt_ast.

    METHODS evaluate REDEFINITION.
    METHODS get_class REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_ast TYPE tyt_ast.

ENDCLASS.



CLASS ZCL_AC_AST_OR IMPLEMENTATION.


  METHOD constructor.
    super->constructor( it_ast[ 1 ]->get_first_token( ) ).
    mt_ast = it_ast.
  ENDMETHOD.


  METHOD evaluate.
    DATA(lv_data) = mt_ast[ 1 ]->evaluate( io_data_provider ).
    ASSIGN lv_data->* TO FIELD-SYMBOL(<lv_value>).

    CREATE DATA rv_value LIKE <lv_value>.
    ASSIGN rv_value->* TO FIELD-SYMBOL(<rv_value>).
    <rv_value> = <lv_value>.

    CHECK <rv_value> = abap_false.

    LOOP AT mt_ast ASSIGNING FIELD-SYMBOL(<lo_ast>) FROM 2.
      DATA(lv_right) = <lo_ast>->evaluate( io_data_provider ).
      ASSIGN lv_right->* TO FIELD-SYMBOL(<lv_right>).

      IF <lv_right> = abap_true.
        <rv_value> = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-logical.
  ENDMETHOD.
ENDCLASS.
