CLASS zcl_ac_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cs_param_name,
        output_mode        TYPE string VALUE 'AC_OUTPUT_MODE',
        object_name        TYPE string VALUE 'AC_OBJECT_NAME',
        object_description TYPE string VALUE 'AC_OBJECT_DESCRIPTION',
      END OF cs_param_name.

    CONSTANTS:
      BEGIN OF cs_output_mode,
        include   TYPE string VALUE 'AC_INCLUDE',
        report    TYPE string VALUE 'AC_REPORT',
        class     TYPE string VALUE 'AC_CLASS',
        clipboard TYPE string VALUE 'AC_CLIPBOARD',
      END OF cs_output_mode.

    CLASS-METHODS transform_include
      IMPORTING
        !iv_include_name   TYPE programm
        !it_variable       TYPE zcl_ac_data_provider=>tyt_variable
        !iv_pretty_printer TYPE abap_bool DEFAULT abap_true
      RAISING
        zcx_ac_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_data_provider TYPE REF TO zcl_ac_data_provider.

    CLASS-METHODS transform
      IMPORTING
        !it_input         TYPE zcl_ac_lexer=>tyt_input
        !it_variable      TYPE zcl_ac_data_provider=>tyt_variable
        iv_pretty_printer TYPE abap_bool
      RAISING
        zcx_ac_exception.

    CLASS-METHODS write2clipboard
      IMPORTING
        it_output TYPE string_table.

    CLASS-METHODS write2include
      IMPORTING
        it_output        TYPE string_table
        io_data_provider TYPE REF TO zcl_ac_data_provider
      RAISING
        zcx_ac_exception.

    CLASS-METHODS write2report
      IMPORTING
        it_output        TYPE string_table
        io_data_provider TYPE REF TO zcl_ac_data_provider
      RAISING
        zcx_ac_exception.

    CLASS-METHODS write2class
      IMPORTING
        it_output        TYPE string_table
        io_data_provider TYPE REF TO zcl_ac_data_provider
      RAISING
        zcx_ac_exception.

ENDCLASS.



CLASS ZCL_AC_GENERATOR IMPLEMENTATION.


  METHOD transform.
    DATA(lo_data_provider) = NEW zcl_ac_data_provider( ).

    LOOP AT it_variable ASSIGNING FIELD-SYMBOL(<ls_variable>).
      ASSIGN <ls_variable>-value->* TO FIELD-SYMBOL(<lv_value>).

      lo_data_provider->set_value(
        iv_variable_name = <ls_variable>-name
        iv_value         = <lv_value>
      ).
    ENDLOOP.

    lo_data_provider->set_value(
      iv_variable_name = 'CS_OUTPUT_MODE'
      iv_value         = cs_output_mode
    ).

    DATA(lo_ast) = zcl_ac_parser=>parse_script( it_input ).

    DATA(lo_writer) = NEW cl_abap_string_c_writer( ).
    lo_ast->execute(
      io_writer = lo_writer
      io_data_provider = lo_data_provider
    ).
    lo_writer->close( ).

    SPLIT lo_writer->get_result_string( )
       AT cl_abap_char_utilities=>cr_lf
     INTO TABLE DATA(lt_output).

    IF iv_pretty_printer = abap_true.
      CALL FUNCTION 'PRETTY_PRINTER'
        EXPORTING
          inctoo = abap_false
        TABLES
          ntext  = lt_output
          otext  = lt_output
        EXCEPTIONS
          OTHERS = 0.
    ENDIF.

    DATA(lv_output_mode) = lo_data_provider->get_value( cs_param_name-output_mode ).
    ASSIGN lv_output_mode->* TO FIELD-SYMBOL(<lv_output_mode>).

    CASE <lv_output_mode>.
      WHEN cs_output_mode-include.
        write2include(
          it_output        = lt_output
          io_data_provider = lo_data_provider
        ).

      WHEN cs_output_mode-report.
        write2report(
          it_output        = lt_output
          io_data_provider = lo_data_provider
        ).

      WHEN cs_output_mode-class.
        write2class(
          it_output        = lt_output
          io_data_provider = lo_data_provider
        ).

      WHEN cs_output_mode-clipboard.
        write2clipboard( lt_output ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_ac_exception
          MESSAGE ID 'ZAC'
          NUMBER '017'
          WITH <lv_output_mode>.
    ENDCASE.
  ENDMETHOD.


  METHOD transform_include.
    DATA lt_text TYPE zcl_ac_lexer=>tyt_input.
    READ REPORT iv_include_name INTO lt_text.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ac_exception
        MESSAGE ID 'ZAC'
        NUMBER '016'
        WITH iv_include_name.
    ENDIF.

    transform(
      it_input    = lt_text
      it_variable = it_variable
      iv_pretty_printer = iv_pretty_printer
    ).
  ENDMETHOD.


  METHOD write2class.
    DATA(lv_class_name) = io_data_provider->get_value( cs_param_name-object_name ).
    ASSIGN lv_class_name->* TO FIELD-SYMBOL(<lv_class_name>).

    DATA(lv_class_descr) = io_data_provider->get_value( cs_param_name-object_description ).
    ASSIGN lv_class_descr->* TO FIELD-SYMBOL(<lv_class_descr>).

    DATA(ls_class) = VALUE vseoclass(
      clsname = <lv_class_name>
      descript = <lv_class_descr>
      fixpt = abap_true
      unicode = abap_true
    ).

    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
        overwrite       = abap_true
      CHANGING
        class           = ls_class
      EXCEPTIONS
        existing        = 1
        is_interface    = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.

    IF sy-subrc = 0 OR sy-subrc = 1.
      TRY .
          DATA(lo_factory) = cl_oo_factory=>create_instance( ).

          DATA(lo_settings) = lo_factory->create_settings(
            signature_enabled         = abap_true
            generator_mode_enabled    = abap_true
          ).

          DATA(lo_source_new) = CAST cl_oo_clif_source(
            lo_factory->create_clif_source(
              clif_name = ls_class-clsname
              settings = lo_settings
            )
          ).

          lo_source_new->lock( ).
          lo_source_new->set_source( it_output ).
          lo_source_new->pretty_print( ).
          lo_source_new->save( ).
          lo_source_new->unlock( ).

          COMMIT WORK AND WAIT.

          MESSAGE s019(zac) WITH ls_class-clsname.

        CATCH cx_root INTO DATA(lx_error).
          RAISE EXCEPTION TYPE zcx_ac_exception
            MESSAGE ID 'ZAC'
            NUMBER '018'
            WITH
                lx_error->get_longtext( ).
      ENDTRY.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD write2clipboard.
    DATA(lv_rc) = 0.
    DATA(lt_output) = it_output.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = lt_output
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        OTHERS               = 1
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MESSAGE 'Text sent to clipboard' TYPE 'S'.
  ENDMETHOD.


  METHOD write2include.
    DATA(lv_include_name) = io_data_provider->get_value( cs_param_name-object_name ).
    ASSIGN lv_include_name->* TO FIELD-SYMBOL(<lv_include_name>).

    DATA(lv_include_descr) = io_data_provider->get_value( cs_param_name-object_description ).
    ASSIGN lv_include_descr->* TO FIELD-SYMBOL(<lv_include_descr>).

    DATA(ls_progdir) = VALUE progdir(
      name = <lv_include_name>
      subc = 'I'
      occurs = abap_true
      fixpt = abap_true
      uccheck = abap_true
    ).

    CALL FUNCTION 'RS_EDTR_ATTR_ADD'
      EXPORTING
        program_name          = ls_progdir-name
        program_title         = CONV rs38m-repti( <lv_include_descr> )
        with_progdir_entry    = abap_true
        suppress_dialog       = abap_true
        activate_immediately  = abap_false
      CHANGING
        program_progdir       = ls_progdir
      EXCEPTIONS
        program_name_missing  = 1
        program_exists        = 2
        wrong_parameter_value = 3
        action_cancelled      = 4
        OTHERS                = 5.

    IF sy-subrc = 0 OR sy-subrc = 2.
      INSERT REPORT ls_progdir-name FROM it_output
        KEEPING DIRECTORY ENTRY.
    ENDIF.

  ENDMETHOD.


  METHOD write2report.
    DATA(lv_include_name) = io_data_provider->get_value( cs_param_name-object_name ).
    ASSIGN lv_include_name->* TO FIELD-SYMBOL(<lv_include_name>).

    DATA(lv_include_descr) = io_data_provider->get_value( cs_param_name-object_description ).
    ASSIGN lv_include_descr->* TO FIELD-SYMBOL(<lv_include_descr>).

    DATA(ls_progdir) = VALUE progdir(
      name = <lv_include_name>
      subc = '1'
      occurs = abap_true
      fixpt = abap_true
      uccheck = abap_true
    ).

    CALL FUNCTION 'RS_EDTR_ATTR_ADD'
      EXPORTING
        program_name          = ls_progdir-name
        program_title         = CONV rs38m-repti( <lv_include_descr> )
        with_progdir_entry    = abap_true
        suppress_dialog       = abap_true
        activate_immediately  = abap_false
      CHANGING
        program_progdir       = ls_progdir
      EXCEPTIONS
        program_name_missing  = 1
        program_exists        = 2
        wrong_parameter_value = 3
        action_cancelled      = 4
        OTHERS                = 5.

    IF sy-subrc = 0 OR sy-subrc = 2.
      INSERT REPORT ls_progdir-name FROM it_output
        KEEPING DIRECTORY ENTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
