*! ac_output_mode = cs_output_mode-class.
*! ac_object_description = 'Generated class'.
*! class_name = ac_object_name.

CLASS {class_name} DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      tyt_tvarvc_range TYPE RANGE OF rvari_val_255.

*! LOOP AT gt_tvarvc INTO DATA(ls_tvarv) WHERE name <> ''.
*!  IF ls_tvarv-type = 'P'.
*!    ret_type = 'RVARI_VAL_255'.
*!    ret_prefix = 'V'.
*!  ELSE.
*!    ret_type = 'TYT_TVARVC_RANGE'.
*!    ret_prefix = 'T'.
*!  ENDIF.
    CLASS-METHODS get_{ls_tvarv-name}
      returning
        value(r{ret_prefix}_{ls_tvarv-name}) TYPE {ret_type}.

*! ENDLOOP.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF tys_data,
*! LOOP AT gt_tvarvc INTO DATA(ls_tvarv) WHERE name <> ''.
*!   IF ls_tvarv-type = 'P'.
*!     ret_type = 'RVARI_VAL_255'.
*!   ELSE.
*!     ret_type = 'TYT_TVARVC_RANGE'.
*!   ENDIF.
        {ls_tvarv-type}_{ls_tvarv-name} TYPE {ret_type},
*! ENDLOOP.
      END OF tys_data.

    TYPES:
      BEGIN OF tys_data_x,
*! LOOP AT gt_tvarvc INTO DATA(ls_tvarv) WHERE name <> ''.
        {ls_tvarv-type}_{ls_tvarv-name} TYPE abap_bool,
*! ENDLOOP.
      END OF tys_data_x.

    CLASS-DATA ms_data TYPE tys_data.
    CLASS-DATA ms_data_x TYPE tys_data_x.

ENDCLASS.


CLASS {class_name} IMPLEMENTATION.

*! LOOP AT gt_tvarvc INTO DATA(ls_tvarv) WHERE name <> ''.
*!   IF ls_tvarv-type = 'P'.
  METHOD get_{ls_tvarv-name}.
    IF ms_data_x-p_{ls_tvarv-name} IS INITIAL.
      SELECT SINGLE low
        INTO @ms_data-p_{ls_tvarv-name}
        FROM tvarvc
       WHERE name = '{ls_tvarv-name}'
         AND type = 'P'.

      ms_data_x-p_{ls_tvarv-name} = abap_true.
    ENDIF.

    rv_{ls_tvarv-name} = ms_data-p_{ls_tvarv-name}.
  ENDMETHOD.

*!  ELSE.
  METHOD get_{ls_tvarv-name}.
    IF ms_data_x-s_{ls_tvarv-name} IS INITIAL.
      SELECT sign,
             opti AS option,
             low,
             high
        INTO CORRESPONDING FIELDS OF TABLE @ms_data-s_{ls_tvarv-name}
        FROM tvarvc
       WHERE name = '{ls_tvarv-name}'
         AND type = 'S'.

      ms_data_x-s_{ls_tvarv-name} = abap_true.
    ENDIF.

    rt_{ls_tvarv-name} = ms_data-s_{ls_tvarv-name}.
  ENDMETHOD.
*!  ENDIF.

*! ENDLOOP.

ENDCLASS.
