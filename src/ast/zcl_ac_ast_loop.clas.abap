class ZCL_AC_AST_LOOP definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  types:
    BEGIN OF tys_if_branch,
        cond      TYPE REF TO zcl_ac_ast_eval,
        stmt_list TYPE REF TO zcl_ac_ast_stmt_list,
      END OF tys_if_branch .
  types:
    tyt_if_branch TYPE STANDARD TABLE OF tys_if_branch WITH DEFAULT KEY .

  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN
      !IV_TABLE_VARIABLE type STRING
      !IV_LINE_VARIABLE type STRING
      !IO_COND type ref to ZCL_AC_AST_EVAL optional
      !IO_BODY type ref to ZCL_AC_AST_STMT_LIST .

  methods EXECUTE
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_table_variable TYPE string.
    DATA mv_line_variable TYPE string.
    DATA mo_cond TYPE REF TO zcl_ac_ast_eval.
    DATA mo_body TYPE REF TO zcl_ac_ast_stmt_list.
ENDCLASS.



CLASS ZCL_AC_AST_LOOP IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
    mv_table_variable = iv_table_variable.
    mv_line_variable = iv_line_variable.
    mo_cond = io_cond.
    mo_body = io_body.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_data) = io_data_provider->get_value( mv_table_variable ).

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN lv_data->* TO <lt_table>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '010'
        WITH mv_table_variable.
    ENDIF.

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).
      io_data_provider->set_value(
        iv_variable_name = 'SY_TABIX'
        iv_value         = sy-tabix
      ).

      IF mo_cond IS BOUND.
        io_data_provider->set_value(
          EXPORTING
            iv_variable_name = 'TABLE_LINE'
            iv_value         = <ls_line>
        ).

        DATA(lv_value) = mo_cond->evaluate( io_data_provider ).
        ASSIGN lv_value->* TO FIELD-SYMBOL(<lv_value>).
        CHECK <lv_value> = abap_true.
      ENDIF.

      io_data_provider->set_value(
        EXPORTING
          iv_variable_name = mv_line_variable
          iv_value         = <ls_line>
      ).

      DATA(lv_exec_state) = mo_body->execute(
        io_writer        = io_writer
        io_data_provider = io_data_provider
      ).

      IF lv_exec_state = cs_exec_state-exit.
        EXIT.
      ENDIF.
    ENDLOOP.

    rv_exec_state = cs_exec_state-complete.

  ENDMETHOD.
ENDCLASS.
