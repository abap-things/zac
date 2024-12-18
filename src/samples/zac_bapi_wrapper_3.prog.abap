REPORT zac_bapi_wrapper_3.

PARAMETERS p_fm TYPE rs38l-name OBLIGATORY MATCHCODE OBJECT sfunc_modules.

START-OF-SELECTION.

  DATA:
    gs_rs38l TYPE rs38l,
    gt_exc   TYPE STANDARD TABLE OF rsexc WITH EMPTY KEY,
    gt_exp   TYPE STANDARD TABLE OF rsexp WITH EMPTY KEY,
    gt_imp   TYPE STANDARD TABLE OF rsimp WITH EMPTY KEY,
    gt_chg   TYPE STANDARD TABLE OF rscha WITH EMPTY KEY,
    gt_tab   TYPE STANDARD TABLE OF rstbl WITH EMPTY KEY.

  CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
    EXPORTING
      funcname           = p_fm
    IMPORTING
      global_flag        = gs_rs38l-global
      remote_call        = gs_rs38l-remote
      update_task        = gs_rs38l-utask
    TABLES
      exception_list     = gt_exc
      export_parameter   = gt_exp
      import_parameter   = gt_imp
      changing_parameter = gt_chg
      tables_parameter   = gt_tab
    EXCEPTIONS
      error_message      = 1
      function_not_found = 2
      invalid_name       = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gs_rs38l-name = p_fm.

  TRY.
      zcl_ac_generator=>transform_include(
        iv_include_name   = 'ZAC_BAPI_WRAPPER_TEMPLATE_3'
        it_variable       = VALUE #(
          ( name = 'gs_rs38l' value = REF #( gs_rs38l ) )
          ( name = 'gt_exc'   value = REF #( gt_exc ) )
          ( name = 'gt_exp'   value = REF #( gt_exp ) )
          ( name = 'gt_imp'   value = REF #( gt_imp ) )
          ( name = 'gt_chg'   value = REF #( gt_chg ) )
          ( name = 'gt_tab'   value = REF #( gt_tab ) )
        )
      ).

    CATCH zcx_ac_exception INTO DATA(lo_ex).
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
