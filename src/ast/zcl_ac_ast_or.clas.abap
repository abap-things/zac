class ZCL_AC_AST_OR definition
  public
  inheriting from ZCL_AC_AST_EVAL
  final
  create public .

public section.

  types:
    tyt_ast TYPE STANDARD TABLE OF REF TO zcl_ac_ast_eval WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IT_AST type TYT_AST .

  methods EVALUATE
    redefinition .
  methods GET_CLASS
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_ast TYPE tyt_ast.

ENDCLASS.



CLASS ZCL_AC_AST_OR IMPLEMENTATION.


  METHOD CONSTRUCTOR.
    super->constructor( it_ast[ 1 ]->get_first_token( ) ).
    mt_ast = it_ast.
  ENDMETHOD.


  METHOD EVALUATE.
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


  METHOD GET_CLASS.
    rv_class = cs_class-logical.
  ENDMETHOD.
ENDCLASS.
