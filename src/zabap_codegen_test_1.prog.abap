REPORT zabap_codegen_test_1.

TABLES tvarvc.

SELECT-OPTIONS s_name FOR tvarvc-name.

START-OF-SELECTION.

  TRY.
      SELECT * INTO TABLE @DATA(gt_tvarvc)
        FROM tvarvc
       WHERE name IN @s_name
         AND name LIKE 'Z%'.

      zcl_ac_generator=>transform_include(
        iv_include_name   = 'ZABAP_CODEGEN_TEST_TEMPLATE_1'
        it_variable       = VALUE #(
          ( name = 'GT_TVARVC' value = REF #( gt_tvarvc ) )
          ( name = zcl_ac_generator=>cs_param_name-object_name value = REF #( `ZCL_AC_STVARV_SAMPLE` ) )
        )
      ).

    CATCH zcx_ac_exception INTO DATA(lo_ex). " Исключение кодогенерации.
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
