*! ac_output_mode = cs_output_mode-class.
*! ac_object_name = ac_concat( iv_val1 = `ZCL_AC_DOM_` iv_val2 = gs_dd01l-domname ).
*! ac_object_name = ac_left( iv_val = ac_object_name iv_len = 30 ).
*! ac_object_description = ac_concat( iv_val1 = `Domain ` iv_val2 = gs_dd01l-domname ).
*! ac_object_description = ac_concat( iv_val1 = ac_object_description iv_val2 = ` wrapper` ).
*! class_name = ac_object_name.
*!"

*!" the text below will be pushed to output with substitutions: {something} will be substitited with value of the same named variable
CLASS {class_name} DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
*!  gv_type = ''.
*!  IF gs_dd01l-datatype = 'CHAR'.
*!    gv_type = 'C'.
*!  ELSEIF gs_dd01l-datatype = 'NUMC'.
*!    gv_type = 'N'.
*!  ELSEIF gs_dd01l-datatype = 'INT'.
*!    gv_type = 'I'.
*!  ENDIF.

*!  gv_size_spec = ''.
*!  IF gs_dd01l-datatype = 'CHAR'
*!  OR gs_dd01l-datatype = 'NUMC'.
*!    gv_length = ac_alpha_out( gs_dd01l-outputlen ).
*!    gv_size_spec = ac_concat( iv_val1 = ` LENGTH ` iv_val2 = gv_length ).
*!  ENDIF.
    TYPES: ty_value_type TYPE {gv_type}{gv_size_spec}.

    CONSTANTS:
      BEGIN OF cs_value,
*!      LOOP AT gt_dd07l INTO DATA(gs_dd07l).
*!        gv_prefix = ac_left( iv_val = gs_dd07l-domvalue_l iv_len = 1 ).
*!        IF 'A' <= gv_prefix AND gv_prefix <= 'Z'
*!        OR 'a' <= gv_prefix AND gv_prefix <= 'z'
*!        OR gv_prefix = '_'.
*!          gv_name = gs_dd07l-domvalue_l.
*!        ELSE.
*!          gv_name = ac_concat( iv_val1 = '_' iv_val2 = gs_dd07l-domvalue_l ).
*!        ENDIF.
        {gv_name} TYPE ty_value_type VALUE '{gs_dd07l-domvalue_l}',
*!      ENDLOOP.
*!
      END OF cs_value.

    CLASS-METHODS class_constructor.

    CLASS-METHODS value2text
      IMPORTING
        iv_value       TYPE ty_value_type
      RETURNING
        VALUE(rv_text) TYPE val_text.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF tys_value2text,
        value TYPE ty_value_type,
        text  TYPE val_text,
      END OF tys_value2text.

    TYPES: tyt_value2text TYPE HASHED TABLE OF tys_value2text WITH UNIQUE KEY value.

    CLASS-DATA mt_value2text TYPE tyt_value2text.

ENDCLASS.


CLASS {class_name} IMPLEMENTATION.
  METHOD class_constructor.
*!    LOOP AT gt_dd07l INTO DATA(gs_dd07l).
*!      LOOP AT gt_dd07t INTO DATA(gs_dd07t) WHERE valpos = gs_dd07l-valpos.
    INSERT VALUE #( value = '{gs_dd07l-domvalue_l}' text = '{gs_dd07t-ddtext}' ) INTO TABLE mt_value2text.
*!       EXIT.
*!      ENDLOOP.
*!    ENDLOOP.
  ENDMETHOD.


  METHOD value2text.
    rv_text = VALUE #( mt_value2text[ value = iv_value ]-text OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
