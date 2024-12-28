CLASS zcl_ac_fun_library DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS ac_lines
      IMPORTING
        !it_table        TYPE ANY TABLE
      RETURNING
        VALUE(rv_result) TYPE i .
    CLASS-METHODS ac_substring
      IMPORTING
        !iv_val          TYPE string
        !iv_off          TYPE i
        !iv_len          TYPE i
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_left
      IMPORTING
        !iv_val          TYPE string
        !iv_len          TYPE i
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_alpha_out
      IMPORTING
        !iv_val          TYPE string
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_coalesce
      IMPORTING
        !iv_val1         TYPE string
        !iv_val2         TYPE string
        !iv_val3         TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_concat
      IMPORTING
        !iv_val1         TYPE string
        !iv_val2         TYPE string
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_cond
      IMPORTING
        !iv_cond         TYPE abap_bool
        !iv_true_val     TYPE string
        !iv_false_val    TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_replace
      IMPORTING
        !iv_val          TYPE string
        !iv_sub          TYPE string
        !iv_with         TYPE string
        !iv_occ          TYPE i DEFAULT 1
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS ac_strlen
      IMPORTING
        !iv_val          TYPE string
      RETURNING
        VALUE(rv_result) TYPE i .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AC_FUN_LIBRARY IMPLEMENTATION.


  METHOD ac_left.
    IF strlen( iv_val ) < iv_len.
      rv_result = iv_val.
      RETURN.
    ENDIF.

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
    rv_result = replace( val = iv_val sub = iv_sub with = iv_with occ = iv_occ ).
  ENDMETHOD.


  METHOD ac_concat.
    rv_result = iv_val1 && iv_val2.
  ENDMETHOD.


  METHOD ac_alpha_out.
    rv_result = |{ iv_val ALPHA = OUT WIDTH = 1 }|.
  ENDMETHOD.


  METHOD ac_coalesce.
    IF iv_val1 IS NOT INITIAL.
      rv_result = iv_val1.
      RETURN.
    ENDIF.

    IF iv_val2 IS NOT INITIAL.
      rv_result = iv_val2.
      RETURN.
    ENDIF.

    IF iv_val3 IS SUPPLIED AND iv_val3 IS NOT INITIAL.
      rv_result = iv_val3.
      RETURN.
    ENDIF.

    rv_result = ``.
  ENDMETHOD.


  METHOD ac_cond.
    IF iv_cond IS NOT INITIAL.
      rv_result = iv_true_val.
    ELSE.
      rv_result = iv_false_val.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
