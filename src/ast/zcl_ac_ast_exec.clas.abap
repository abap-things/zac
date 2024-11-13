class ZCL_AC_AST_EXEC definition
  public
  inheriting from ZCL_AC_AST
  abstract
  create public .

public section.

  constants:
    BEGIN OF cs_exec_state,
        complete TYPE i VALUE 0,
        exit     TYPE i VALUE 1,
        continue TYPE i VALUE 2,
      END OF cs_exec_state .

  methods CONSTRUCTOR
    importing
      !IS_FIRST_TOKEN type ZCL_AC_LEXER=>TYS_TOKEN .
  methods EXECUTE
  abstract
    importing
      !IO_WRITER type ref to CL_ABAP_STRING_C_WRITER
      !IO_DATA_PROVIDER type ref to ZCL_AC_DATA_PROVIDER
    returning
      value(RV_EXEC_STATE) type I
    raising
      ZCX_AC_EXCEPTION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AC_AST_EXEC IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_first_token ).
  ENDMETHOD.
ENDCLASS.
