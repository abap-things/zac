
CLASS zcl_ac_bapi_material_get_detai DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
*     importing
      material              TYPE bapimatdet-material, " optional
      plant                 TYPE bapimatall-plant, " optional
      valuationarea         TYPE bapimatall-val_area, " optional
      valuationtype         TYPE bapimatall-val_type, " optional
      material_evg          TYPE bapimgvmatnr, " optional
      material_long         TYPE bapimatdet-material_long, " optional
*     exporting
      material_general_data TYPE bapimatdoa,
      return                TYPE bapireturn,
      materialplantdata     TYPE bapimatdoc,
      materialvaluationdata TYPE bapimatdobew,
*     changing
*     tables
*     dummy to beatify
      dummy.

    METHODS:
      call.

  PRIVATE SECTION.
    DATA: BEGIN OF ms_requested,
            material_general_data TYPE abap_bool,
            return                TYPE abap_bool,
            materialplantdata     TYPE abap_bool,
            materialvaluationdata TYPE abap_bool,
          END OF ms_requested.

    METHODS:
      prepare_parmbind RETURNING VALUE(rt_parmbind) TYPE abap_func_parmbind_tab.

ENDCLASS.



CLASS ZCL_AC_BAPI_MATERIAL_GET_DETAI IMPLEMENTATION.


  METHOD call.
    DATA:
      lt_etab TYPE abap_func_excpbind_tab.

    DATA(lt_ptab) = prepare_parmbind( ).

    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.
  ENDMETHOD.


  METHOD prepare_parmbind.
*   importing
    IF material IS NOT INITIAL.
      INSERT VALUE #(
        name = 'MATERIAL'
        kind = abap_func_importing
        value = REF #( material ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF plant IS NOT INITIAL.
      INSERT VALUE #(
        name = 'PLANT'
        kind = abap_func_importing
        value = REF #( plant ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF valuationarea IS NOT INITIAL.
      INSERT VALUE #(
        name = 'VALUATIONAREA'
        kind = abap_func_importing
        value = REF #( valuationarea ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF valuationtype IS NOT INITIAL.
      INSERT VALUE #(
        name = 'VALUATIONTYPE'
        kind = abap_func_importing
        value = REF #( valuationtype ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF material_evg IS NOT INITIAL.
      INSERT VALUE #(
        name = 'MATERIAL_EVG'
        kind = abap_func_importing
        value = REF #( material_evg ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF material_long IS NOT INITIAL.
      INSERT VALUE #(
        name = 'MATERIAL_LONG'
        kind = abap_func_importing
        value = REF #( material_long ) ) INTO TABLE rt_parmbind.
    ENDIF.

*   exporting return
    INSERT VALUE #(
      name = 'RETURN'
      kind = abap_func_exporting
      value = REF #( return ) ) INTO TABLE rt_parmbind.

*   exporting
    IF ms_requested-material_general_data = abap_true.
      INSERT VALUE #(
        name = 'MATERIAL_GENERAL_DATA'
        kind = abap_func_exporting
        value = REF #( material_general_data ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF ms_requested-materialplantdata = abap_true.
      INSERT VALUE #(
        name = 'MATERIALPLANTDATA'
        kind = abap_func_exporting
        value = REF #( materialplantdata ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF ms_requested-materialvaluationdata = abap_true.
      INSERT VALUE #(
        name = 'MATERIALVALUATIONDATA'
        kind = abap_func_exporting
        value = REF #( materialvaluationdata ) ) INTO TABLE rt_parmbind.
    ENDIF.


*   tables

  ENDMETHOD.
ENDCLASS.
