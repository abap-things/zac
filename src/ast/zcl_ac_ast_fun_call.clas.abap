CLASS zcl_ac_ast_fun_call DEFINITION
  PUBLIC
  INHERITING FROM zcl_ac_ast_eval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF tys_named_param,
        name TYPE abap_parmname,
        ast  TYPE REF TO zcl_ac_ast_eval,
      END OF tys_named_param.

    TYPES:
      tyt_named_param TYPE STANDARD TABLE OF tys_named_param WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !is_function_token TYPE zcl_ac_lexer=>tys_token
        !io_unnamed_param  TYPE REF TO zcl_ac_ast_eval OPTIONAL
        !it_named_param    TYPE tyt_named_param OPTIONAL
      RAISING
        zcx_ac_exception .

    CLASS-METHODS class_constructor.

    METHODS evaluate REDEFINITION.
    METHODS get_class REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA mo_library_descr TYPE REF TO cl_abap_objectdescr.

    DATA mv_function_name TYPE string.
    DATA mo_unnamed_param TYPE REF TO zcl_ac_ast_eval.
    DATA mt_named_param TYPE tyt_named_param.

    METHODS compile_time_check
      RAISING
        zcx_ac_exception .

    METHODS create_data_for_param
      IMPORTING
        iv_param_name  TYPE abap_parmname
      RETURNING
        VALUE(lv_data) TYPE REF TO data.

    METHODS conv_data_for_param
      IMPORTING
        iv_param_name        TYPE abap_parmname
        iv_data              TYPE REF TO data
      RETURNING
        VALUE(lv_param_data) TYPE REF TO data.

ENDCLASS.



CLASS ZCL_AC_AST_FUN_CALL IMPLEMENTATION.


  METHOD class_constructor.
    mo_library_descr = CAST cl_abap_objectdescr(
      cl_abap_typedescr=>describe_by_name( '\CLASS=' && 'ZCL_AC_FUN_LIBRARY' )
    ).
  ENDMETHOD.


  METHOD compile_time_check.
    DATA(lv_row) = get_first_token( )-row.
    DATA(lv_col) = get_first_token( )-col.

    DATA(ls_method) = VALUE #( mo_library_descr->methods[ name = mv_function_name ] OPTIONAL ).
    IF ls_method IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '011'
        WITH mv_function_name lv_row lv_col.
    ENDIF.

    DATA(lv_all_importing_count) = 0.
    DATA(lv_req_importing_count) = 0.

    LOOP AT ls_method-parameters ASSIGNING FIELD-SYMBOL(<ls_param>)
      WHERE parm_kind = cl_abap_objectdescr=>importing.

      lv_all_importing_count += 1.

      IF <ls_param>-is_optional = abap_false.
        lv_req_importing_count += 1.
      ENDIF.
    ENDLOOP.

*   without parameters
    IF lv_all_importing_count = 0.
      IF mo_unnamed_param IS BOUND OR mt_named_param IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '012'
          WITH mv_function_name lv_row lv_col.
      ENDIF.

      RETURN.
    ENDIF.

*   one non-optional parameter
    IF mo_unnamed_param IS BOUND.
      IF lv_req_importing_count <> 1.
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '013'
          WITH mv_function_name lv_row lv_col.
      ENDIF.

      RETURN.
    ENDIF.

*   named parameter list
    LOOP AT ls_method-parameters ASSIGNING <ls_param>
      WHERE parm_kind = cl_abap_objectdescr=>importing
        AND is_optional = abap_false.

      IF NOT line_exists( mt_named_param[ name = <ls_param>-name ] ).
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '014'
          WITH <ls_param>-name mv_function_name lv_row lv_col.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_named_param ASSIGNING FIELD-SYMBOL(<ls_named_param>).
      IF NOT line_exists( ls_method-parameters[
        name = <ls_named_param>-name
        parm_kind = cl_abap_objectdescr=>importing
      ] ).
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '015'
          WITH <ls_named_param>-name mv_function_name lv_row lv_col.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( is_function_token ).
    mv_function_name = to_upper( is_function_token-raw_value ).
    mo_unnamed_param = io_unnamed_param.
    mt_named_param = it_named_param.

    compile_time_check( ).
  ENDMETHOD.


  METHOD evaluate.
    DATA(lt_parameters) = mo_library_descr->methods[ name = mv_function_name ]-parameters.
    DATA(lt_parm_bind) = VALUE abap_parmbind_tab( ).

    IF mo_unnamed_param IS BOUND.
      DATA(ls_req_param) = lt_parameters[ parm_kind = cl_abap_objectdescr=>importing is_optional = abap_false ].

      INSERT VALUE #(
        name  = ls_req_param-name
        kind = cl_abap_objectdescr=>exporting
        value = conv_data_for_param(
                  iv_param_name = ls_req_param-name
                  iv_data       = mo_unnamed_param->evaluate( io_data_provider )
                )
      ) INTO TABLE lt_parm_bind.

    ELSE.
      LOOP AT mt_named_param ASSIGNING FIELD-SYMBOL(<ls_named_param>).
        INSERT VALUE #(
          name  = <ls_named_param>-name
          kind = cl_abap_objectdescr=>exporting
          value = conv_data_for_param(
                    iv_param_name = <ls_named_param>-name
                    iv_data       = <ls_named_param>-ast->evaluate( io_data_provider )
                  )
        ) INTO TABLE lt_parm_bind.
      ENDLOOP.
    ENDIF.

    DATA(ls_ret_param) = lt_parameters[ parm_kind = cl_abap_objectdescr=>returning ].
    rv_value = create_data_for_param( ls_ret_param-name  ).

    INSERT VALUE #(
      name  = ls_ret_param-name
      kind = cl_abap_objectdescr=>receiving
      value = rv_value
    ) INTO TABLE lt_parm_bind.

    CALL METHOD zcl_ac_fun_library=>(mv_function_name) PARAMETER-TABLE lt_parm_bind.
  ENDMETHOD.


  METHOD get_class.
    rv_class = cs_class-value.
  ENDMETHOD.


  METHOD conv_data_for_param.
    DATA(ls_param) = mo_library_descr->methods[ name = mv_function_name ]-parameters[ name = iv_param_name  ].

*   pass tables as ref
    IF ls_param-type_kind = cl_abap_objectdescr=>typekind_table.
      lv_param_data = iv_data.
      RETURN.
    ENDIF.

    lv_param_data = create_data_for_param( iv_param_name ).
    ASSIGN iv_data->* TO FIELD-SYMBOL(<lv_data>).
    ASSIGN lv_param_data->* TO FIELD-SYMBOL(<lv_param_data>).
    <lv_param_data> = <lv_data>.
  ENDMETHOD.


  METHOD create_data_for_param.
    DATA(lo_ret_param_descr) = mo_library_descr->get_method_parameter_type(
      p_method_name       = mv_function_name
      p_parameter_name    = iv_param_name
    ).

    CREATE DATA lv_data TYPE HANDLE lo_ret_param_descr.
  ENDMETHOD.
ENDCLASS.
