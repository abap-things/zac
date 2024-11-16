* Simple code generation sample: the static access to STVARV parameters
* We use include ZAC_SAMPLE_STVARV_TEMPLATE_1 as template to generate global class
REPORT zac_sample_stvarv_1.

TABLES tvarvc.

* We can select STVARV parameters we need
SELECT-OPTIONS s_name FOR tvarvc-name.

START-OF-SELECTION.

  TRY.
*     Read parameters and it's properties (limit with only Z-params)
      SELECT * INTO TABLE @DATA(gt_tvarvc)
        FROM tvarvc
       WHERE name IN @s_name
         AND name LIKE 'Z%'.

*     And generate a global class that will allow us to get static access to them
      zcl_ac_generator=>transform_include(
        iv_include_name   = 'ZAC_SAMPLE_STVARV_TEMPLATE_1' "this include is the source of code, pls explore its content
        it_variable       = VALUE #(
          (
            name = 'GT_TVARVC'
            value = REF #( gt_tvarvc ) )
          (
            name = zcl_ac_generator=>cs_param_name-object_name
            value = REF #( `ZCL_AC_STVARV_SAMPLE_1`  "it's the desination global class name (we can set it inside of script too)
          ) )
        )
      ).

    CATCH zcx_ac_exception INTO DATA(lo_ex). " Something error occured
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
