class ZCL_AC_AST_SUM definition
  public
  inheriting from ZCL_AC_AST_EVAL
  final
  create public .

public section.

  types:
    BEGIN OF tys_co_ast,
        operator TYPE string,
        ast      TYPE REF TO zcl_ac_ast_eval,
      END OF tys_co_ast .
  types:
    tyt_co_ast TYPE STANDARD TABLE OF tys_co_ast WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IO_AST type ref to ZCL_AC_AST_EVAL
      !IT_CO_AST type TYT_CO_AST .

  methods EVALUATE
    redefinition .
  methods GET_CLASS
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_ast TYPE REF TO zcl_ac_ast_eval.
    DATA mt_co_ast TYPE tyt_co_ast.

ENDCLASS.



CLASS ZCL_AC_AST_SUM IMPLEMENTATION.


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
        WHEN '+'.
          <rv_value> = <rv_value> + <lv_right>.
        WHEN '-'.
          <rv_value> = <rv_value> - <lv_right>.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.
ENDCLASS.
