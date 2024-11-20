CLASS zcl_ac_ast_subst DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_exec
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_subst_token TYPE zcl_ac_lexer=>tys_token .

    METHODS execute REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_subst TYPE string.

ENDCLASS.



CLASS ZCL_AC_AST_SUBST IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_subst_token ).
    mv_subst = is_subst_token-raw_value.
  ENDMETHOD.


  METHOD execute.
    DATA(lv_value) = io_data_provider->get_value( mv_subst ).
    ASSIGN lv_value->* TO FIELD-SYMBOL(<lv_value>).
    io_writer->write( |{ <lv_value> }| ).

    rv_exec_state = cs_exec_state-complete.
  ENDMETHOD.
ENDCLASS.
