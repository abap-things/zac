REPORT zac_domain_wrapper_3.

PARAMETERS p_dom TYPE dd01l-domname DEFAULT 'ABAPVRS' OBLIGATORY.

START-OF-SELECTION.
  TRY.
      SELECT SINGLE * INTO @DATA(gs_dd01l)
        FROM dd01l
       WHERE domname = @p_dom.

      SELECT * INTO TABLE @DATA(gt_dd07l)
        FROM dd07l
       WHERE domname = @p_dom.

      SELECT * INTO TABLE @DATA(gt_dd07t)
        FROM dd07t
       WHERE domname = @p_dom
       ORDER BY valpos.

      DELETE ADJACENT DUPLICATES FROM gt_dd07t COMPARING ddlanguage.

      zcl_ac_generator=>transform_include(
        iv_include_name   = 'ZAC_DOMAIN_WRAPPER_TEMPLATE_3'
        it_variable       = VALUE #(
          ( name = 'gs_dd01l'
            value = REF #( gs_dd01l ) )
          ( name = 'gt_dd07l'
            value = REF #( gt_dd07l ) )
          ( name = 'gt_dd07t'
            value = REF #( gt_dd07t ) )
        )
      ).

    CATCH zcx_ac_exception INTO DATA(lo_ex).
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
