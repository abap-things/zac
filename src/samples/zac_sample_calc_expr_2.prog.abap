REPORT zac_calculate_expression.

START-OF-SELECTION.

  TRY.
*     Simple arythmetic expression
      CONSTANTS cs_num_expr TYPE string VALUE `1 + 30 / 5 * ( 6 - 5 )`.
      DATA(go_num_expr) = zcl_ac_expression=>parse( cs_num_expr ).
      DATA(gv_num_value)  = go_num_expr->evaluate( ).
      ASSIGN gv_num_value->* TO FIELD-SYMBOL(<gv_num_value>).

      WRITE:/ 'Numeric expression', cs_num_expr, '=', <gv_num_value>.

*     Simple logical expression
      CONSTANTS cs_log_expr TYPE string VALUE `'ABC' CS 'A' AND ( 1 = 1 ) AND NOT ( '1' = 2 )`.
      DATA(go_log_expr) = zcl_ac_expression=>parse( cs_log_expr ).
      DATA(gv_log_value)  = go_log_expr->evaluate( ).
      ASSIGN gv_log_value->* TO FIELD-SYMBOL(<gv_log_value>).

      WRITE:/ 'Logical expression', cs_log_expr, '=', <gv_log_value>.

*     String expression used outer variable and function call
      CONSTANTS cs_str_expr TYPE string VALUE `ac_concat( iv_val1 = outer_variable iv_val2 = sy-uname )`.
      DATA(go_str_expr) = zcl_ac_expression=>parse( cs_str_expr ).
      DATA(gv_str_value)  = go_str_expr->evaluate( VALUE #( ( name = 'outer_variable' value = ref #( `Logged as: ` ) ) ) ).
      ASSIGN gv_str_value->* TO FIELD-SYMBOL(<gv_str_value>).

      WRITE:/ 'String expression', cs_str_expr, '=', <gv_str_value>.

    CATCH zcx_ac_exception INTO DATA(lo_ex). " Something error occured
      WRITE:/ lo_ex->get_text( ).
  ENDTRY.
