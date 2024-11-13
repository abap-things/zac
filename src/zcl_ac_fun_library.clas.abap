class ZCL_AC_FUN_LIBRARY definition
  public
  final
  create public .

public section.

  class-methods AC_LINES
    importing
      !IT_TABLE type ANY TABLE
    returning
      value(RV_RESULT) type I .
  class-methods AC_SUBSTRING
    importing
      !IV_VAL type STRING
      !IV_OFF type I
      !IV_LEN type I
    returning
      value(RV_RESULT) type STRING .
  class-methods AC_LEFT
    importing
      !IV_VAL type STRING
      !IV_LEN type I
    returning
      value(RV_RESULT) type STRING .
  class-methods AC_REPLACE
    importing
      !IV_VAL type STRING
      !IV_SUB type STRING
      !IV_WITH type STRING
    returning
      value(RV_RESULT) type STRING .
  class-methods AC_STRLEN
    importing
      !IV_VAL type STRING
    returning
      value(RV_RESULT) type I .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AC_FUN_LIBRARY IMPLEMENTATION.


  METHOD ac_left.
    rv_result = substring( val =  iv_val len = iv_len ).
  ENDMETHOD.


  METHOD ac_lines.
    rv_result = lines( it_table ).
  ENDMETHOD.


  METHOD ac_strlen.
    rv_result = strlen( iv_val ).
  ENDMETHOD.


  METHOD ac_substring.
    IF iv_off > strlen( iv_val ).
      rv_result = ``.
      RETURN.
    ENDIF.

    rv_result = substring( val =  iv_val off = iv_off len = iv_len ).
  ENDMETHOD.


  METHOD ac_replace.
    rv_result = replace( val = iv_val sub = iv_sub with = iv_with ).
  ENDMETHOD.
ENDCLASS.
