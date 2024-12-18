*! ac_output_mode = cs_output_mode-class.
*! ac_object_name = ac_left( iv_val = `ZCL_AC_` && gs_rs38l-name iv_len = 30 ).
*! ac_object_description = gs_rs38l-name && ` wrapper`.
*! class_name = ac_object_name.

CLASS {class_name} DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
*     importing
*!  LOOP AT gt_imp INTO DATA(gs_imp).
*!    gv_type = ac_coalesce( iv_val1 = gs_imp-dbfield iv_val2 = gs_imp-typ ).
*!    gv_optional_clause = ac_cond( iv_cond = gs_imp-optional iv_true_val = '"optional' ).
*!    gv_default_clause = ''.
*!    IF gs_imp-default <> ''.
*!      gv_default_clause = ` VALUE ` && gs_imp-default.
*!    ENDIF.
    {gs_imp-parameter} TYPE {gv_type}{gv_default_clause}, {gv_optional_clause}
*!  ENDLOOP.
*     exporting
*!  LOOP AT gt_exp INTO DATA(gs_exp).
*!    gv_type = ac_coalesce( iv_val1 = gs_exp-dbfield iv_val2 = gs_exp-typ ).
    {gs_exp-parameter} TYPE {gv_type},
*!  ENDLOOP.
*     changing
*!  LOOP AT gt_chg INTO DATA(gs_chg).
*!    gv_type = ac_coalesce( iv_val1 = gs_chg-dbfield iv_val2 = gs_chg-typ ).
*!    gv_optional_clause = ac_cond( iv_cond = gs_chg-optional iv_true_val = '"optional' ).
    {gs_chg-parameter} TYPE {gv_type}, {gv_optional_clause}
*!  ENDLOOP.
*     tables
*!  LOOP AT gt_tab INTO DATA(gs_tab).
*!    gv_type = ac_coalesce( iv_val1 = gs_tab-typ iv_val2 = gs_tab-dbstruct ).
*!    gv_optional_clause = ac_cond( iv_cond = gs_imp-optional iv_true_val = '"optional' ).
    {gs_tab-parameter} TYPE standard table of {gv_type} WITH empty key, {gv_optional_clause}
*!  ENDLOOP.
*     dummy to beatify
    dummy.

    METHODS:
      call.

  PRIVATE SECTION.
*! IF ac_lines( it_table = gt_exp ) > 0.
    DATA: BEGIN OF ms_requested,
*!   LOOP AT gt_exp INTO DATA(gs_exp).
            {gs_exp-parameter} TYPE abap_bool,
*!   ENDLOOP.
*!   LOOP AT gt_chg INTO DATA(gs_chg) WHERE optional = abap_true.
            {gs_chg-parameter} TYPE abap_bool,
*!   ENDLOOP.
*!   LOOP AT gt_tab INTO DATA(gs_tab) WHERE optional = abap_true.
            {gs_tab-parameter} TYPE abap_bool,
*!   ENDLOOP.
          END OF ms_requested.
*!  ENDIF.

    METHODS:
      prepare_parmbind RETURNING VALUE(rt_parmbind) TYPE abap_func_parmbind_tab.

ENDCLASS.


CLASS {class_name} IMPLEMENTATION.
  METHOD prepare_parmbind.
*   importing
*!  LOOP AT gt_imp INTO DATA(gs_imp).
*!  IF gs_imp-optional = abap_true.
*!    IF gs_imp-default <> ''.
    IF {gs_imp-parameter} <> {gs_imp-default}.
*!    ELSE.
      IF {gs_imp-parameter} IS NOT INITIAL.
*!    ENDIF.
*!  ENDIF.
        INSERT VALUE #(
          name = '{gs_imp-parameter}'
          kind = abap_func_importing
          value = REF #( {gs_imp-parameter} ) ) INTO TABLE rt_parmbind.
*!  IF gs_imp-optional = abap_true.
      ENDIF.
*!  ENDIF.

*!  ENDLOOP.
*!  LOOP AT gt_exp INTO DATA(gs_exp) WHERE parameter = 'RETURN'.
*   exporting return
      INSERT VALUE #(
        name = 'RETURN'
        kind = abap_func_exporting
        value = REF #( return ) ) INTO TABLE rt_parmbind.

*!  ENDLOOP.
*   exporting
*!  LOOP AT gt_exp INTO DATA(gs_exp) WHERE parameter <> 'RETURN'.
      IF ms_requested-{gs_exp-parameter} = abap_true.
        INSERT VALUE #(
          name = '{gs_exp-parameter}'
          kind = abap_func_exporting
          value = REF #( {gs_exp-parameter} ) ) INTO TABLE rt_parmbind.
      ENDIF.

*!  ENDLOOP.
*!  LOOP AT gt_tab INTO DATA(gs_tab) WHERE parameter = 'RETURN'.
*   tables RETURN
      INSERT VALUE #(
        name = 'RETURN'
        kind = abap_func_tables
        value = REF #( return ) ) INTO TABLE rt_parmbind.
*!  ENDLOOP.

*   tables
*!  LOOP AT gt_tab INTO DATA(gs_tab) WHERE parameter <> 'RETURN'.
*!  IF gs_tab-optional = abap_true.
      IF {gs_tab-parameter} IS NOT INITIAL OR ms_requested-{gs_tab-parameter} = abap_true.
*!  ENDIF.
        INSERT VALUE #(
          name = '{gs_tab-parameter}'
          kind = abap_func_tables
          value = REF #( {gs_tab-parameter} ) ) INTO TABLE rt_parmbind.
*!  IF gs_tab-optional = abap_true.
      ENDIF.
*!  ENDIF.

*!  ENDLOOP.

    ENDMETHOD.

    METHOD call.
      DATA:
        lt_etab TYPE abap_func_excpbind_tab.

      DATA(lt_ptab) = prepare_parmbind( ).

      CALL FUNCTION '{gs_rs38l-name}'
        PARAMETER-TABLE lt_ptab
        EXCEPTION-TABLE lt_etab.
    ENDMETHOD.
ENDCLASS.
