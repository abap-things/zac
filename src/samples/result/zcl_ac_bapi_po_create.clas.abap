
CLASS zcl_ac_bapi_po_create DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
*     importing
      po_header                  TYPE bapiekkoc,
      po_header_add_data         TYPE bapiekkoa, "optional
      header_add_data_relevant   TYPE bapimmpara-selection, "optional
      po_address                 TYPE bapiaddress, "optional
      skip_items_with_error      TYPE bapimmpara-selection VALUE 'X', "optional
      item_add_data_relevant     TYPE bapimmpara-selection, "optional
      header_tech_fields         TYPE bapitech, "optional
*     exporting
      purchaseorder              TYPE bapiekkoc-po_number,
*     changing
*     tables
      po_items                   TYPE STANDARD TABLE OF bapiekpoc WITH EMPTY KEY, "optional
      po_item_add_data           TYPE STANDARD TABLE OF bapiekpoa WITH EMPTY KEY, "optional
      po_item_schedules          TYPE STANDARD TABLE OF bapieket WITH EMPTY KEY, "optional
      po_item_account_assignment TYPE STANDARD TABLE OF bapiekkn WITH EMPTY KEY, "optional
      po_item_text               TYPE STANDARD TABLE OF bapiekpotx WITH EMPTY KEY, "optional
      return                     TYPE STANDARD TABLE OF bapireturn WITH EMPTY KEY, "optional
      po_limits                  TYPE STANDARD TABLE OF bapiesuhc WITH EMPTY KEY, "optional
      po_contract_limits         TYPE STANDARD TABLE OF bapiesucc WITH EMPTY KEY, "optional
      po_services                TYPE STANDARD TABLE OF bapiesllc WITH EMPTY KEY, "optional
      po_srv_accass_values       TYPE STANDARD TABLE OF bapiesklc WITH EMPTY KEY, "optional
      po_services_text           TYPE STANDARD TABLE OF bapieslltx WITH EMPTY KEY, "optional
      po_business_partner        TYPE STANDARD TABLE OF bapiekkop WITH EMPTY KEY, "optional
      extensionin                TYPE STANDARD TABLE OF bapiparex WITH EMPTY KEY, "optional
      poaddrdelivery             TYPE STANDARD TABLE OF bapimepoaddrdelivery WITH EMPTY KEY, "optional
      nfmetallitms               TYPE STANDARD TABLE OF /nfm/bapidocitm WITH EMPTY KEY, "optional
*     dummy to beatify
      dummy.

    METHODS:
      req_po_item_add_data,
      req_po_item_account_assignment,
      req_po_item_text,
      req_po_limits,
      req_po_contract_limits,
      req_po_services,
      req_po_srv_accass_values,
      req_po_services_text,
      req_po_business_partner,
      req_extensionin,
      req_poaddrdelivery,
      req_nfmetallitms,
      req_purchaseorder,
      call.

  PRIVATE SECTION.
    DATA: BEGIN OF ms_requested,
            purchaseorder              TYPE abap_bool,
            po_item_add_data           TYPE abap_bool,
            po_item_account_assignment TYPE abap_bool,
            po_item_text               TYPE abap_bool,
            return                     TYPE abap_bool,
            po_limits                  TYPE abap_bool,
            po_contract_limits         TYPE abap_bool,
            po_services                TYPE abap_bool,
            po_srv_accass_values       TYPE abap_bool,
            po_services_text           TYPE abap_bool,
            po_business_partner        TYPE abap_bool,
            extensionin                TYPE abap_bool,
            poaddrdelivery             TYPE abap_bool,
            nfmetallitms               TYPE abap_bool,
          END OF ms_requested.

    METHODS:
      prepare_parmbind RETURNING VALUE(rt_parmbind) TYPE abap_func_parmbind_tab.

ENDCLASS.



CLASS ZCL_AC_BAPI_PO_CREATE IMPLEMENTATION.


  METHOD call.
    DATA:
      lt_etab TYPE abap_func_excpbind_tab.

    DATA(lt_ptab) = prepare_parmbind( ).

    CALL FUNCTION 'BAPI_PO_CREATE'
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.
  ENDMETHOD.


  METHOD prepare_parmbind.
*   importing
    INSERT VALUE #(
      name = 'PO_HEADER'
      kind = abap_func_importing
      value = REF #( po_header ) ) INTO TABLE rt_parmbind.

    IF po_header_add_data IS NOT INITIAL.
      INSERT VALUE #(
        name = 'PO_HEADER_ADD_DATA'
        kind = abap_func_importing
        value = REF #( po_header_add_data ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF header_add_data_relevant IS NOT INITIAL.
      INSERT VALUE #(
        name = 'HEADER_ADD_DATA_RELEVANT'
        kind = abap_func_importing
        value = REF #( header_add_data_relevant ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_address IS NOT INITIAL.
      INSERT VALUE #(
        name = 'PO_ADDRESS'
        kind = abap_func_importing
        value = REF #( po_address ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF skip_items_with_error <> 'X'.
      INSERT VALUE #(
        name = 'SKIP_ITEMS_WITH_ERROR'
        kind = abap_func_importing
        value = REF #( skip_items_with_error ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF item_add_data_relevant IS NOT INITIAL.
      INSERT VALUE #(
        name = 'ITEM_ADD_DATA_RELEVANT'
        kind = abap_func_importing
        value = REF #( item_add_data_relevant ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF header_tech_fields IS NOT INITIAL.
      INSERT VALUE #(
        name = 'HEADER_TECH_FIELDS'
        kind = abap_func_importing
        value = REF #( header_tech_fields ) ) INTO TABLE rt_parmbind.
    ENDIF.

*   exporting
    IF ms_requested-purchaseorder = abap_true.
      INSERT VALUE #(
        name = 'PURCHASEORDER'
        kind = abap_func_exporting
        value = REF #( purchaseorder ) ) INTO TABLE rt_parmbind.
    ENDIF.

*   tables RETURN
    INSERT VALUE #(
      name = 'RETURN'
      kind = abap_func_tables
      value = REF #( return ) ) INTO TABLE rt_parmbind.

*   tables
    INSERT VALUE #(
      name = 'PO_ITEMS'
      kind = abap_func_tables
      value = REF #( po_items ) ) INTO TABLE rt_parmbind.

    IF po_item_add_data IS NOT INITIAL OR ms_requested-po_item_add_data = abap_true.
      INSERT VALUE #(
        name = 'PO_ITEM_ADD_DATA'
        kind = abap_func_tables
        value = REF #( po_item_add_data ) ) INTO TABLE rt_parmbind.
    ENDIF.

    INSERT VALUE #(
      name = 'PO_ITEM_SCHEDULES'
      kind = abap_func_tables
      value = REF #( po_item_schedules ) ) INTO TABLE rt_parmbind.

    IF po_item_account_assignment IS NOT INITIAL OR ms_requested-po_item_account_assignment = abap_true.
      INSERT VALUE #(
        name = 'PO_ITEM_ACCOUNT_ASSIGNMENT'
        kind = abap_func_tables
        value = REF #( po_item_account_assignment ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_item_text IS NOT INITIAL OR ms_requested-po_item_text = abap_true.
      INSERT VALUE #(
        name = 'PO_ITEM_TEXT'
        kind = abap_func_tables
        value = REF #( po_item_text ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_limits IS NOT INITIAL OR ms_requested-po_limits = abap_true.
      INSERT VALUE #(
        name = 'PO_LIMITS'
        kind = abap_func_tables
        value = REF #( po_limits ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_contract_limits IS NOT INITIAL OR ms_requested-po_contract_limits = abap_true.
      INSERT VALUE #(
        name = 'PO_CONTRACT_LIMITS'
        kind = abap_func_tables
        value = REF #( po_contract_limits ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_services IS NOT INITIAL OR ms_requested-po_services = abap_true.
      INSERT VALUE #(
        name = 'PO_SERVICES'
        kind = abap_func_tables
        value = REF #( po_services ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_srv_accass_values IS NOT INITIAL OR ms_requested-po_srv_accass_values = abap_true.
      INSERT VALUE #(
        name = 'PO_SRV_ACCASS_VALUES'
        kind = abap_func_tables
        value = REF #( po_srv_accass_values ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_services_text IS NOT INITIAL OR ms_requested-po_services_text = abap_true.
      INSERT VALUE #(
        name = 'PO_SERVICES_TEXT'
        kind = abap_func_tables
        value = REF #( po_services_text ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF po_business_partner IS NOT INITIAL OR ms_requested-po_business_partner = abap_true.
      INSERT VALUE #(
        name = 'PO_BUSINESS_PARTNER'
        kind = abap_func_tables
        value = REF #( po_business_partner ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF extensionin IS NOT INITIAL OR ms_requested-extensionin = abap_true.
      INSERT VALUE #(
        name = 'EXTENSIONIN'
        kind = abap_func_tables
        value = REF #( extensionin ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaddrdelivery IS NOT INITIAL OR ms_requested-poaddrdelivery = abap_true.
      INSERT VALUE #(
        name = 'POADDRDELIVERY'
        kind = abap_func_tables
        value = REF #( poaddrdelivery ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF nfmetallitms IS NOT INITIAL OR ms_requested-nfmetallitms = abap_true.
      INSERT VALUE #(
        name = 'NFMETALLITMS'
        kind = abap_func_tables
        value = REF #( nfmetallitms ) ) INTO TABLE rt_parmbind.
    ENDIF.

  ENDMETHOD.


  METHOD  req_extensionin.
    ms_requested-extensionin = abap_true.
  ENDMETHOD.


  METHOD  req_nfmetallitms.
    ms_requested-nfmetallitms = abap_true.
  ENDMETHOD.


  METHOD  req_poaddrdelivery.
    ms_requested-poaddrdelivery = abap_true.
  ENDMETHOD.


  METHOD  req_po_business_partner.
    ms_requested-po_business_partner = abap_true.
  ENDMETHOD.


  METHOD  req_po_contract_limits.
    ms_requested-po_contract_limits = abap_true.
  ENDMETHOD.


  METHOD  req_po_item_account_assignment.
    ms_requested-po_item_account_assignment = abap_true.
  ENDMETHOD.


  METHOD  req_po_item_add_data.
    ms_requested-po_item_add_data = abap_true.
  ENDMETHOD.


  METHOD  req_po_item_text.
    ms_requested-po_item_text = abap_true.
  ENDMETHOD.


  METHOD  req_po_limits.
    ms_requested-po_limits = abap_true.
  ENDMETHOD.


  METHOD  req_po_services.
    ms_requested-po_services = abap_true.
  ENDMETHOD.


  METHOD  req_po_services_text.
    ms_requested-po_services_text = abap_true.
  ENDMETHOD.


  METHOD  req_po_srv_accass_values.
    ms_requested-po_srv_accass_values = abap_true.
  ENDMETHOD.


  METHOD  req_purchaseorder.
    ms_requested-purchaseorder = abap_true.
  ENDMETHOD.
ENDCLASS.
