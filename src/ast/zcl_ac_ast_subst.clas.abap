class ZCL_AC_AST_SUBST definition
  public
  inheriting from ZCL_AC_AST_EXEC
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IS_SUBST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .

  methods EXECUTE
    redefinition .
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
