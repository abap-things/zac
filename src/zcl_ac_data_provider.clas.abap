CLASS zcl_ac_data_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF tys_variable,
        name  TYPE string,
        value TYPE REF TO data,
      END OF tys_variable.

    TYPES: tyt_variable TYPE HASHED TABLE OF tys_variable WITH UNIQUE KEY name.

    METHODS get_value
      IMPORTING
        iv_variable_name TYPE string
      RETURNING
        VALUE(rv_value)  TYPE REF TO data
      RAISING
        zcx_ac_exception .

    METHODS set_value
      IMPORTING
        iv_variable_name TYPE string
        iv_value         TYPE any
      RAISING
        zcx_ac_exception .

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_variable TYPE tyt_variable.

ENDCLASS.



CLASS ZCL_AC_DATA_PROVIDER IMPLEMENTATION.


  METHOD get_value.
    DATA(iv_variable_name_upper) = to_upper( iv_variable_name ).

    IF line_exists( mt_variable[ name = 'TABLE_LINE' ] ).
      DATA(lv_table_line) = mt_variable[ name = 'TABLE_LINE' ]-value.
      ASSIGN lv_table_line->* TO FIELD-SYMBOL(<ls_table_line>).

      ASSIGN COMPONENT iv_variable_name_upper OF STRUCTURE <ls_table_line> TO FIELD-SYMBOL(<lv_field>).
      IF sy-subrc = 0.
        rv_value = REF #( <lv_field> ).
        RETURN.
      ENDIF.
    ENDIF.

    IF line_exists( mt_variable[ name = iv_variable_name_upper ] ).
      rv_value = mt_variable[ name = iv_variable_name_upper ]-value.
      RETURN.
    ENDIF.

    FIND '-' IN iv_variable_name_upper MATCH OFFSET DATA(lv_off).
    IF sy-subrc = 0.
      DATA(lv_structure_name) = iv_variable_name_upper(lv_off).
      lv_off += 1.
      DATA(lv_field_name) = iv_variable_name_upper+lv_off.

      IF line_exists( mt_variable[ name = lv_structure_name ] ).
        DATA(lv_structure) = mt_variable[ name = lv_structure_name ]-value.
        ASSIGN lv_structure->* TO FIELD-SYMBOL(<ls_structure>).

        ASSIGN COMPONENT lv_field_name OF STRUCTURE <ls_structure> TO <lv_field>.
        IF sy-subrc = 0.
          rv_value = REF #( <lv_field> ).
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    ASSIGN (iv_variable_name_upper) TO FIELD-SYMBOL(<lv_global_variable>).
    IF sy-subrc = 0.
      rv_value = REF #( <lv_global_variable> ).
      RETURN.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_ac_exception
      MESSAGE ID 'ZAC'
      NUMBER '004'
      WITH iv_variable_name_upper.

  ENDMETHOD.


  METHOD set_value.
    DATA(iv_variable_name_upper) = to_upper( iv_variable_name ).

    FIND '-' IN iv_variable_name_upper MATCH OFFSET DATA(lv_off).
    IF sy-subrc = 0.
      DATA(lv_structure_name) = iv_variable_name_upper(lv_off).
      lv_off += 1.
      DATA(lv_field_name) = iv_variable_name_upper+lv_off.

      IF NOT line_exists( mt_variable[ name = lv_structure_name ] ).
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '004'
          WITH lv_structure_name.
      ENDIF.

      DATA(lv_structure) = mt_variable[ name = lv_structure_name ]-value.
      ASSIGN lv_structure->* TO FIELD-SYMBOL(<ls_structure>).

      ASSIGN COMPONENT lv_field_name OF STRUCTURE <ls_structure> TO FIELD-SYMBOL(<lv_field>).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '008'
          WITH lv_field_name lv_structure_name.
      ENDIF.

      <lv_field> = iv_value.
      RETURN.
    ENDIF.

    IF NOT line_exists( mt_variable[ name = iv_variable_name_upper ] ).
      INSERT VALUE #( name = iv_variable_name_upper ) INTO TABLE mt_variable.
    ENDIF.

    ASSIGN mt_variable[ name = iv_variable_name_upper ] TO FIELD-SYMBOL(<ls_variable>).
    CREATE DATA <ls_variable>-value LIKE iv_value.
    ASSIGN <ls_variable>-value->* TO FIELD-SYMBOL(<lv_value>).
    <lv_value> = iv_value.
  ENDMETHOD.
ENDCLASS.
