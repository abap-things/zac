* Template sample: static access to STVARV parameters, this comment will be pushed to output as is



CLASS zcl_ac_stvarv_sample_1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      tyt_tvarvc_range TYPE RANGE OF rvari_val_255.

    CLASS-METHODS get_z_param_p
      RETURNING
        VALUE(rv_z_param_p) TYPE rvari_val_255.

    CLASS-METHODS get_zwrf_hier_id
      RETURNING
        VALUE(rt_zwrf_hier_id) TYPE tyt_tvarvc_range.

    CLASS-METHODS get_z_param_s
      RETURNING
        VALUE(rt_z_param_s) TYPE tyt_tvarvc_range.


  PRIVATE SECTION.
    TYPES:
      BEGIN OF tys_data,
        p_z_param_p    TYPE rvari_val_255,
        s_zwrf_hier_id TYPE tyt_tvarvc_range,
        s_z_param_s    TYPE tyt_tvarvc_range,
      END OF tys_data.

    TYPES:
      BEGIN OF tys_data_x,
        p_z_param_p    TYPE abap_bool,
        s_zwrf_hier_id TYPE abap_bool,
        s_z_param_s    TYPE abap_bool,
      END OF tys_data_x.

    CLASS-DATA ms_data TYPE tys_data.
    CLASS-DATA ms_data_x TYPE tys_data_x.

ENDCLASS.



CLASS ZCL_AC_STVARV_SAMPLE_1 IMPLEMENTATION.


  METHOD get_z_param_p.
    IF ms_data_x-p_z_param_p IS INITIAL.
      SELECT SINGLE low
        INTO @ms_data-p_z_param_p
        FROM tvarvc
       WHERE name = 'Z_PARAM_P'
         AND type = 'P'.

      ms_data_x-p_z_param_p = abap_true.
    ENDIF.

    rv_z_param_p = ms_data-p_z_param_p.
  ENDMETHOD.


  METHOD get_z_param_s.
    IF ms_data_x-s_z_param_s IS INITIAL.
      SELECT sign,
             opti AS option,
             low,
             high
        INTO CORRESPONDING FIELDS OF TABLE @ms_data-s_z_param_s
        FROM tvarvc
       WHERE name = 'Z_PARAM_S'
         AND type = 'S'.

      ms_data_x-s_z_param_s = abap_true.
    ENDIF.

    rt_z_param_s = ms_data-s_z_param_s.
  ENDMETHOD.


  METHOD get_zwrf_hier_id.
    IF ms_data_x-s_zwrf_hier_id IS INITIAL.
      SELECT sign,
             opti AS option,
             low,
             high
        INTO CORRESPONDING FIELDS OF TABLE @ms_data-s_zwrf_hier_id
        FROM tvarvc
       WHERE name = 'ZWRF_HIER_ID'
         AND type = 'S'.

      ms_data_x-s_zwrf_hier_id = abap_true.
    ENDIF.

    rt_zwrf_hier_id = ms_data-s_zwrf_hier_id.
  ENDMETHOD.
ENDCLASS.
