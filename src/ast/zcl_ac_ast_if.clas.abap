class ZCL_AC_AST_IF definition
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
      !IT_IF_BRANCH type TYT_IF_BRANCH
      !IO_ELSE_BRANCH type ref to ZCL_AC_AST_STMT_LIST .

  methods EXECUTE
    redefinition .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_if_branch TYPE tyt_if_branch.
    DATA mo_else_branch TYPE REF TO zcl_ac_ast_stmt_list.

ENDCLASS.



CLASS ZCL_AC_AST_IF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
    mt_if_branch = it_if_branch.
    mo_else_branch = io_else_branch.
  ENDMETHOD.


  METHOD execute.
    LOOP AT mt_if_branch ASSIGNING FIELD-SYMBOL(<ms_if_branch>).
      DATA(lv_value) = <ms_if_branch>-cond->evaluate( io_data_provider ).
      ASSIGN lv_value->* TO FIELD-SYMBOL(<lv_value>).
      IF <lv_value> = abap_true.
        rv_exec_state = <ms_if_branch>-stmt_list->execute(
          io_writer        = io_writer
          io_data_provider = io_data_provider
        ).

        RETURN.
      ENDIF.
    ENDLOOP.

    IF mo_else_branch IS BOUND.
      rv_exec_state = mo_else_branch->execute(
        io_writer        = io_writer
        io_data_provider = io_data_provider
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
