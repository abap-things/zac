REPORT zac_sample_stvarv_1.

TABLES tvarvc.

SELECT-OPTIONS s_name FOR tvarvc-name.

START-OF-SELECTION.

  TRY.
      SELECT * INTO TABLE @DATA(gt_tvarvc)
        FROM tvarvc
       WHERE name IN @s_name
         AND name LIKE 'Z_PARAM%'.

      zcl_ac_generator=>transform_include(
        iv_include_name   = 'ZAC_SAMPLE_STVARV_TEMPLATE_1'
        it_variable       = VALUE #(
          ( name = 'GT_TVARVC' value = REF #( gt_tvarvc ) )
          ( name = zcl_ac_generator=>cs_param_name-object_name value = REF #( `ZCL_AC_STVARV_SAMPLE_1` ) )
        )
      ).

    CATCH zcx_ac_exception INTO DATA(lo_ex). " Исключение кодогенерации.
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
