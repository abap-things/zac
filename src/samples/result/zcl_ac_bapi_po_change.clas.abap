
CLASS zcl_ac_bapi_po_change DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
*     importing
      purchaseorder          TYPE bapimepoheader-po_number,
      poheader               TYPE bapimepoheader, "optional
      poheaderx              TYPE bapimepoheaderx, "optional
      poaddrvendor           TYPE bapimepoaddrvendor, "optional
      testrun                TYPE bapiflag-bapiflag, "optional
      memory_uncomplete      TYPE bapiflag-bapiflag, "optional
      memory_complete        TYPE bapiflag-bapiflag, "optional
      poexpimpheader         TYPE bapieikp, "optional
      poexpimpheaderx        TYPE bapieikpx, "optional
      versions               TYPE bapimedcm, "optional
      no_messaging           TYPE bapiflag-bapiflag, "optional
      no_message_req         TYPE bapiflag-bapiflag, "optional
      no_authority           TYPE bapiflag-bapiflag, "optional
      no_price_from_po       TYPE bapiflag-bapiflag, "optional
      park_uncomplete        TYPE bapiflag-bapiflag, "optional
      park_complete          TYPE bapiflag-bapiflag, "optional
*     exporting
      expheader              TYPE bapimepoheader,
      exppoexpimpheader      TYPE bapieikp,
*     changing
*     tables
      return                 TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY, "optional
      poitem                 TYPE STANDARD TABLE OF bapimepoitem WITH EMPTY KEY, "optional
      poitemx                TYPE STANDARD TABLE OF bapimepoitemx WITH EMPTY KEY, "optional
      poaddrdelivery         TYPE STANDARD TABLE OF bapimepoaddrdelivery WITH EMPTY KEY, "optional
      poschedule             TYPE STANDARD TABLE OF bapimeposchedule WITH EMPTY KEY, "optional
      poschedulex            TYPE STANDARD TABLE OF bapimeposchedulx WITH EMPTY KEY, "optional
      poaccount              TYPE STANDARD TABLE OF bapimepoaccount WITH EMPTY KEY, "optional
      poaccountprofitsegment TYPE STANDARD TABLE OF bapimepoaccountprofitsegment WITH EMPTY KEY, "optional
      poaccountx             TYPE STANDARD TABLE OF bapimepoaccountx WITH EMPTY KEY, "optional
      pocondheader           TYPE STANDARD TABLE OF bapimepocondheader WITH EMPTY KEY, "optional
      pocondheaderx          TYPE STANDARD TABLE OF bapimepocondheaderx WITH EMPTY KEY, "optional
      pocond                 TYPE STANDARD TABLE OF bapimepocond WITH EMPTY KEY, "optional
      pocondx                TYPE STANDARD TABLE OF bapimepocondx WITH EMPTY KEY, "optional
      polimits               TYPE STANDARD TABLE OF bapiesuhc WITH EMPTY KEY, "optional
      pocontractlimits       TYPE STANDARD TABLE OF bapiesucc WITH EMPTY KEY, "optional
      poservices             TYPE STANDARD TABLE OF bapiesllc WITH EMPTY KEY, "optional
      posrvaccessvalues      TYPE STANDARD TABLE OF bapiesklc WITH EMPTY KEY, "optional
      poservicestext         TYPE STANDARD TABLE OF bapieslltx WITH EMPTY KEY, "optional
      extensionin            TYPE STANDARD TABLE OF bapiparex WITH EMPTY KEY, "optional
      extensionout           TYPE STANDARD TABLE OF bapiparex WITH EMPTY KEY, "optional
      poexpimpitem           TYPE STANDARD TABLE OF bapieipo WITH EMPTY KEY, "optional
      poexpimpitemx          TYPE STANDARD TABLE OF bapieipox WITH EMPTY KEY, "optional
      potextheader           TYPE STANDARD TABLE OF bapimepotextheader WITH EMPTY KEY, "optional
      potextitem             TYPE STANDARD TABLE OF bapimepotext WITH EMPTY KEY, "optional
      allversions            TYPE STANDARD TABLE OF bapimedcm_allversions WITH EMPTY KEY, "optional
      popartner              TYPE STANDARD TABLE OF bapiekkop WITH EMPTY KEY, "optional
      pocomponents           TYPE STANDARD TABLE OF bapimepocomponent WITH EMPTY KEY, "optional
      pocomponentsx          TYPE STANDARD TABLE OF bapimepocomponentx WITH EMPTY KEY, "optional
      poshipping             TYPE STANDARD TABLE OF bapiitemship WITH EMPTY KEY, "optional
      poshippingx            TYPE STANDARD TABLE OF bapiitemshipx WITH EMPTY KEY, "optional
      poshippingexp          TYPE STANDARD TABLE OF bapimeposhippexp WITH EMPTY KEY, "optional
      pohistory              TYPE STANDARD TABLE OF bapiekbe WITH EMPTY KEY, "optional
      pohistory_totals       TYPE STANDARD TABLE OF bapiekbes WITH EMPTY KEY, "optional
      poconfirmation         TYPE STANDARD TABLE OF bapiekes WITH EMPTY KEY, "optional
      serialnumber           TYPE STANDARD TABLE OF bapimeposerialno WITH EMPTY KEY, "optional
      serialnumberx          TYPE STANDARD TABLE OF bapimeposerialnox WITH EMPTY KEY, "optional
      invplanheader          TYPE STANDARD TABLE OF bapi_invoice_plan_header WITH EMPTY KEY, "optional
      invplanheaderx         TYPE STANDARD TABLE OF bapi_invoice_plan_headerx WITH EMPTY KEY, "optional
      invplanitem            TYPE STANDARD TABLE OF bapi_invoice_plan_item WITH EMPTY KEY, "optional
      invplanitemx           TYPE STANDARD TABLE OF bapi_invoice_plan_itemx WITH EMPTY KEY, "optional
      pohistory_ma           TYPE STANDARD TABLE OF bapiekbe_ma WITH EMPTY KEY, "optional
      nfmetallitms           TYPE STANDARD TABLE OF /nfm/bapidocitm WITH EMPTY KEY, "optional
*     dummy to beatify
      dummy.

    METHODS:
      req_poitem,
      req_poitemx,
      req_poaddrdelivery,
      req_poschedule,
      req_poschedulex,
      req_poaccount,
      req_poaccountprofitsegment,
      req_poaccountx,
      req_pocondheader,
      req_pocondheaderx,
      req_pocond,
      req_pocondx,
      req_polimits,
      req_pocontractlimits,
      req_poservices,
      req_posrvaccessvalues,
      req_poservicestext,
      req_extensionin,
      req_extensionout,
      req_poexpimpitem,
      req_poexpimpitemx,
      req_potextheader,
      req_potextitem,
      req_allversions,
      req_popartner,
      req_pocomponents,
      req_pocomponentsx,
      req_poshipping,
      req_poshippingx,
      req_poshippingexp,
      req_pohistory,
      req_pohistory_totals,
      req_poconfirmation,
      req_serialnumber,
      req_serialnumberx,
      req_invplanheader,
      req_invplanheaderx,
      req_invplanitem,
      req_invplanitemx,
      req_pohistory_ma,
      req_nfmetallitms,
      req_expheader,
      req_exppoexpimpheader,
      call.

  PRIVATE SECTION.
    DATA: BEGIN OF ms_requested,
            expheader              TYPE abap_bool,
            exppoexpimpheader      TYPE abap_bool,
            return                 TYPE abap_bool,
            poitem                 TYPE abap_bool,
            poitemx                TYPE abap_bool,
            poaddrdelivery         TYPE abap_bool,
            poschedule             TYPE abap_bool,
            poschedulex            TYPE abap_bool,
            poaccount              TYPE abap_bool,
            poaccountprofitsegment TYPE abap_bool,
            poaccountx             TYPE abap_bool,
            pocondheader           TYPE abap_bool,
            pocondheaderx          TYPE abap_bool,
            pocond                 TYPE abap_bool,
            pocondx                TYPE abap_bool,
            polimits               TYPE abap_bool,
            pocontractlimits       TYPE abap_bool,
            poservices             TYPE abap_bool,
            posrvaccessvalues      TYPE abap_bool,
            poservicestext         TYPE abap_bool,
            extensionin            TYPE abap_bool,
            extensionout           TYPE abap_bool,
            poexpimpitem           TYPE abap_bool,
            poexpimpitemx          TYPE abap_bool,
            potextheader           TYPE abap_bool,
            potextitem             TYPE abap_bool,
            allversions            TYPE abap_bool,
            popartner              TYPE abap_bool,
            pocomponents           TYPE abap_bool,
            pocomponentsx          TYPE abap_bool,
            poshipping             TYPE abap_bool,
            poshippingx            TYPE abap_bool,
            poshippingexp          TYPE abap_bool,
            pohistory              TYPE abap_bool,
            pohistory_totals       TYPE abap_bool,
            poconfirmation         TYPE abap_bool,
            serialnumber           TYPE abap_bool,
            serialnumberx          TYPE abap_bool,
            invplanheader          TYPE abap_bool,
            invplanheaderx         TYPE abap_bool,
            invplanitem            TYPE abap_bool,
            invplanitemx           TYPE abap_bool,
            pohistory_ma           TYPE abap_bool,
            nfmetallitms           TYPE abap_bool,
          END OF ms_requested.

    METHODS:
      prepare_parmbind RETURNING VALUE(rt_parmbind) TYPE abap_func_parmbind_tab.

ENDCLASS.



CLASS ZCL_AC_BAPI_PO_CHANGE IMPLEMENTATION.


  METHOD call.
    DATA:
      lt_etab TYPE abap_func_excpbind_tab.

    DATA(lt_ptab) = prepare_parmbind( ).

    CALL FUNCTION 'BAPI_PO_CHANGE'
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.
  ENDMETHOD.


  METHOD prepare_parmbind.
*   importing
    INSERT VALUE #(
      name = 'PURCHASEORDER'
      kind = abap_func_importing
      value = REF #( purchaseorder ) ) INTO TABLE rt_parmbind.

    IF poheader IS NOT INITIAL.
      INSERT VALUE #(
        name = 'POHEADER'
        kind = abap_func_importing
        value = REF #( poheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poheaderx IS NOT INITIAL.
      INSERT VALUE #(
        name = 'POHEADERX'
        kind = abap_func_importing
        value = REF #( poheaderx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaddrvendor IS NOT INITIAL.
      INSERT VALUE #(
        name = 'POADDRVENDOR'
        kind = abap_func_importing
        value = REF #( poaddrvendor ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF testrun IS NOT INITIAL.
      INSERT VALUE #(
        name = 'TESTRUN'
        kind = abap_func_importing
        value = REF #( testrun ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF memory_uncomplete IS NOT INITIAL.
      INSERT VALUE #(
        name = 'MEMORY_UNCOMPLETE'
        kind = abap_func_importing
        value = REF #( memory_uncomplete ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF memory_complete IS NOT INITIAL.
      INSERT VALUE #(
        name = 'MEMORY_COMPLETE'
        kind = abap_func_importing
        value = REF #( memory_complete ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poexpimpheader IS NOT INITIAL.
      INSERT VALUE #(
        name = 'POEXPIMPHEADER'
        kind = abap_func_importing
        value = REF #( poexpimpheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poexpimpheaderx IS NOT INITIAL.
      INSERT VALUE #(
        name = 'POEXPIMPHEADERX'
        kind = abap_func_importing
        value = REF #( poexpimpheaderx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF versions IS NOT INITIAL.
      INSERT VALUE #(
        name = 'VERSIONS'
        kind = abap_func_importing
        value = REF #( versions ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF no_messaging IS NOT INITIAL.
      INSERT VALUE #(
        name = 'NO_MESSAGING'
        kind = abap_func_importing
        value = REF #( no_messaging ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF no_message_req IS NOT INITIAL.
      INSERT VALUE #(
        name = 'NO_MESSAGE_REQ'
        kind = abap_func_importing
        value = REF #( no_message_req ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF no_authority IS NOT INITIAL.
      INSERT VALUE #(
        name = 'NO_AUTHORITY'
        kind = abap_func_importing
        value = REF #( no_authority ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF no_price_from_po IS NOT INITIAL.
      INSERT VALUE #(
        name = 'NO_PRICE_FROM_PO'
        kind = abap_func_importing
        value = REF #( no_price_from_po ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF park_uncomplete IS NOT INITIAL.
      INSERT VALUE #(
        name = 'PARK_UNCOMPLETE'
        kind = abap_func_importing
        value = REF #( park_uncomplete ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF park_complete IS NOT INITIAL.
      INSERT VALUE #(
        name = 'PARK_COMPLETE'
        kind = abap_func_importing
        value = REF #( park_complete ) ) INTO TABLE rt_parmbind.
    ENDIF.

*   exporting
    IF ms_requested-expheader = abap_true.
      INSERT VALUE #(
        name = 'EXPHEADER'
        kind = abap_func_exporting
        value = REF #( expheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF ms_requested-exppoexpimpheader = abap_true.
      INSERT VALUE #(
        name = 'EXPPOEXPIMPHEADER'
        kind = abap_func_exporting
        value = REF #( exppoexpimpheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

*   tables RETURN
    INSERT VALUE #(
      name = 'RETURN'
      kind = abap_func_tables
      value = REF #( return ) ) INTO TABLE rt_parmbind.

*   tables
    IF poitem IS NOT INITIAL OR ms_requested-poitem = abap_true.
      INSERT VALUE #(
        name = 'POITEM'
        kind = abap_func_tables
        value = REF #( poitem ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poitemx IS NOT INITIAL OR ms_requested-poitemx = abap_true.
      INSERT VALUE #(
        name = 'POITEMX'
        kind = abap_func_tables
        value = REF #( poitemx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaddrdelivery IS NOT INITIAL OR ms_requested-poaddrdelivery = abap_true.
      INSERT VALUE #(
        name = 'POADDRDELIVERY'
        kind = abap_func_tables
        value = REF #( poaddrdelivery ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poschedule IS NOT INITIAL OR ms_requested-poschedule = abap_true.
      INSERT VALUE #(
        name = 'POSCHEDULE'
        kind = abap_func_tables
        value = REF #( poschedule ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poschedulex IS NOT INITIAL OR ms_requested-poschedulex = abap_true.
      INSERT VALUE #(
        name = 'POSCHEDULEX'
        kind = abap_func_tables
        value = REF #( poschedulex ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaccount IS NOT INITIAL OR ms_requested-poaccount = abap_true.
      INSERT VALUE #(
        name = 'POACCOUNT'
        kind = abap_func_tables
        value = REF #( poaccount ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaccountprofitsegment IS NOT INITIAL OR ms_requested-poaccountprofitsegment = abap_true.
      INSERT VALUE #(
        name = 'POACCOUNTPROFITSEGMENT'
        kind = abap_func_tables
        value = REF #( poaccountprofitsegment ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poaccountx IS NOT INITIAL OR ms_requested-poaccountx = abap_true.
      INSERT VALUE #(
        name = 'POACCOUNTX'
        kind = abap_func_tables
        value = REF #( poaccountx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocondheader IS NOT INITIAL OR ms_requested-pocondheader = abap_true.
      INSERT VALUE #(
        name = 'POCONDHEADER'
        kind = abap_func_tables
        value = REF #( pocondheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocondheaderx IS NOT INITIAL OR ms_requested-pocondheaderx = abap_true.
      INSERT VALUE #(
        name = 'POCONDHEADERX'
        kind = abap_func_tables
        value = REF #( pocondheaderx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocond IS NOT INITIAL OR ms_requested-pocond = abap_true.
      INSERT VALUE #(
        name = 'POCOND'
        kind = abap_func_tables
        value = REF #( pocond ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocondx IS NOT INITIAL OR ms_requested-pocondx = abap_true.
      INSERT VALUE #(
        name = 'POCONDX'
        kind = abap_func_tables
        value = REF #( pocondx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF polimits IS NOT INITIAL OR ms_requested-polimits = abap_true.
      INSERT VALUE #(
        name = 'POLIMITS'
        kind = abap_func_tables
        value = REF #( polimits ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocontractlimits IS NOT INITIAL OR ms_requested-pocontractlimits = abap_true.
      INSERT VALUE #(
        name = 'POCONTRACTLIMITS'
        kind = abap_func_tables
        value = REF #( pocontractlimits ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poservices IS NOT INITIAL OR ms_requested-poservices = abap_true.
      INSERT VALUE #(
        name = 'POSERVICES'
        kind = abap_func_tables
        value = REF #( poservices ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF posrvaccessvalues IS NOT INITIAL OR ms_requested-posrvaccessvalues = abap_true.
      INSERT VALUE #(
        name = 'POSRVACCESSVALUES'
        kind = abap_func_tables
        value = REF #( posrvaccessvalues ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poservicestext IS NOT INITIAL OR ms_requested-poservicestext = abap_true.
      INSERT VALUE #(
        name = 'POSERVICESTEXT'
        kind = abap_func_tables
        value = REF #( poservicestext ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF extensionin IS NOT INITIAL OR ms_requested-extensionin = abap_true.
      INSERT VALUE #(
        name = 'EXTENSIONIN'
        kind = abap_func_tables
        value = REF #( extensionin ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF extensionout IS NOT INITIAL OR ms_requested-extensionout = abap_true.
      INSERT VALUE #(
        name = 'EXTENSIONOUT'
        kind = abap_func_tables
        value = REF #( extensionout ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poexpimpitem IS NOT INITIAL OR ms_requested-poexpimpitem = abap_true.
      INSERT VALUE #(
        name = 'POEXPIMPITEM'
        kind = abap_func_tables
        value = REF #( poexpimpitem ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poexpimpitemx IS NOT INITIAL OR ms_requested-poexpimpitemx = abap_true.
      INSERT VALUE #(
        name = 'POEXPIMPITEMX'
        kind = abap_func_tables
        value = REF #( poexpimpitemx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF potextheader IS NOT INITIAL OR ms_requested-potextheader = abap_true.
      INSERT VALUE #(
        name = 'POTEXTHEADER'
        kind = abap_func_tables
        value = REF #( potextheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF potextitem IS NOT INITIAL OR ms_requested-potextitem = abap_true.
      INSERT VALUE #(
        name = 'POTEXTITEM'
        kind = abap_func_tables
        value = REF #( potextitem ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF allversions IS NOT INITIAL OR ms_requested-allversions = abap_true.
      INSERT VALUE #(
        name = 'ALLVERSIONS'
        kind = abap_func_tables
        value = REF #( allversions ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF popartner IS NOT INITIAL OR ms_requested-popartner = abap_true.
      INSERT VALUE #(
        name = 'POPARTNER'
        kind = abap_func_tables
        value = REF #( popartner ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocomponents IS NOT INITIAL OR ms_requested-pocomponents = abap_true.
      INSERT VALUE #(
        name = 'POCOMPONENTS'
        kind = abap_func_tables
        value = REF #( pocomponents ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pocomponentsx IS NOT INITIAL OR ms_requested-pocomponentsx = abap_true.
      INSERT VALUE #(
        name = 'POCOMPONENTSX'
        kind = abap_func_tables
        value = REF #( pocomponentsx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poshipping IS NOT INITIAL OR ms_requested-poshipping = abap_true.
      INSERT VALUE #(
        name = 'POSHIPPING'
        kind = abap_func_tables
        value = REF #( poshipping ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poshippingx IS NOT INITIAL OR ms_requested-poshippingx = abap_true.
      INSERT VALUE #(
        name = 'POSHIPPINGX'
        kind = abap_func_tables
        value = REF #( poshippingx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poshippingexp IS NOT INITIAL OR ms_requested-poshippingexp = abap_true.
      INSERT VALUE #(
        name = 'POSHIPPINGEXP'
        kind = abap_func_tables
        value = REF #( poshippingexp ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pohistory IS NOT INITIAL OR ms_requested-pohistory = abap_true.
      INSERT VALUE #(
        name = 'POHISTORY'
        kind = abap_func_tables
        value = REF #( pohistory ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pohistory_totals IS NOT INITIAL OR ms_requested-pohistory_totals = abap_true.
      INSERT VALUE #(
        name = 'POHISTORY_TOTALS'
        kind = abap_func_tables
        value = REF #( pohistory_totals ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF poconfirmation IS NOT INITIAL OR ms_requested-poconfirmation = abap_true.
      INSERT VALUE #(
        name = 'POCONFIRMATION'
        kind = abap_func_tables
        value = REF #( poconfirmation ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF serialnumber IS NOT INITIAL OR ms_requested-serialnumber = abap_true.
      INSERT VALUE #(
        name = 'SERIALNUMBER'
        kind = abap_func_tables
        value = REF #( serialnumber ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF serialnumberx IS NOT INITIAL OR ms_requested-serialnumberx = abap_true.
      INSERT VALUE #(
        name = 'SERIALNUMBERX'
        kind = abap_func_tables
        value = REF #( serialnumberx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF invplanheader IS NOT INITIAL OR ms_requested-invplanheader = abap_true.
      INSERT VALUE #(
        name = 'INVPLANHEADER'
        kind = abap_func_tables
        value = REF #( invplanheader ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF invplanheaderx IS NOT INITIAL OR ms_requested-invplanheaderx = abap_true.
      INSERT VALUE #(
        name = 'INVPLANHEADERX'
        kind = abap_func_tables
        value = REF #( invplanheaderx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF invplanitem IS NOT INITIAL OR ms_requested-invplanitem = abap_true.
      INSERT VALUE #(
        name = 'INVPLANITEM'
        kind = abap_func_tables
        value = REF #( invplanitem ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF invplanitemx IS NOT INITIAL OR ms_requested-invplanitemx = abap_true.
      INSERT VALUE #(
        name = 'INVPLANITEMX'
        kind = abap_func_tables
        value = REF #( invplanitemx ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF pohistory_ma IS NOT INITIAL OR ms_requested-pohistory_ma = abap_true.
      INSERT VALUE #(
        name = 'POHISTORY_MA'
        kind = abap_func_tables
        value = REF #( pohistory_ma ) ) INTO TABLE rt_parmbind.
    ENDIF.

    IF nfmetallitms IS NOT INITIAL OR ms_requested-nfmetallitms = abap_true.
      INSERT VALUE #(
        name = 'NFMETALLITMS'
        kind = abap_func_tables
        value = REF #( nfmetallitms ) ) INTO TABLE rt_parmbind.
    ENDIF.

  ENDMETHOD.


  METHOD  req_poexpimpitem.
    ms_requested-poexpimpitem = abap_true.
  ENDMETHOD.


  METHOD  req_poexpimpitemx.
    ms_requested-poexpimpitemx = abap_true.
  ENDMETHOD.


  METHOD  req_pohistory.
    ms_requested-pohistory = abap_true.
  ENDMETHOD.


  METHOD  req_pohistory_ma.
    ms_requested-pohistory_ma = abap_true.
  ENDMETHOD.


  METHOD  req_pohistory_totals.
    ms_requested-pohistory_totals = abap_true.
  ENDMETHOD.


  METHOD  req_poitem.
    ms_requested-poitem = abap_true.
  ENDMETHOD.


  METHOD  req_poitemx.
    ms_requested-poitemx = abap_true.
  ENDMETHOD.


  METHOD  req_polimits.
    ms_requested-polimits = abap_true.
  ENDMETHOD.


  METHOD  req_popartner.
    ms_requested-popartner = abap_true.
  ENDMETHOD.


  METHOD  req_poschedule.
    ms_requested-poschedule = abap_true.
  ENDMETHOD.


  METHOD  req_poschedulex.
    ms_requested-poschedulex = abap_true.
  ENDMETHOD.


  METHOD  req_poservices.
    ms_requested-poservices = abap_true.
  ENDMETHOD.


  METHOD  req_poservicestext.
    ms_requested-poservicestext = abap_true.
  ENDMETHOD.


  METHOD  req_poshipping.
    ms_requested-poshipping = abap_true.
  ENDMETHOD.


  METHOD  req_poshippingexp.
    ms_requested-poshippingexp = abap_true.
  ENDMETHOD.


  METHOD  req_poshippingx.
    ms_requested-poshippingx = abap_true.
  ENDMETHOD.


  METHOD  req_posrvaccessvalues.
    ms_requested-posrvaccessvalues = abap_true.
  ENDMETHOD.


  METHOD  req_potextheader.
    ms_requested-potextheader = abap_true.
  ENDMETHOD.


  METHOD  req_potextitem.
    ms_requested-potextitem = abap_true.
  ENDMETHOD.


  METHOD  req_serialnumber.
    ms_requested-serialnumber = abap_true.
  ENDMETHOD.


  METHOD  req_serialnumberx.
    ms_requested-serialnumberx = abap_true.
  ENDMETHOD.


  METHOD  req_pocontractlimits.
    ms_requested-pocontractlimits = abap_true.
  ENDMETHOD.


  METHOD  req_poconfirmation.
    ms_requested-poconfirmation = abap_true.
  ENDMETHOD.


  METHOD  req_pocondx.
    ms_requested-pocondx = abap_true.
  ENDMETHOD.


  METHOD  req_pocondheaderx.
    ms_requested-pocondheaderx = abap_true.
  ENDMETHOD.


  METHOD  req_pocondheader.
    ms_requested-pocondheader = abap_true.
  ENDMETHOD.


  METHOD  req_pocond.
    ms_requested-pocond = abap_true.
  ENDMETHOD.


  METHOD  req_pocomponentsx.
    ms_requested-pocomponentsx = abap_true.
  ENDMETHOD.


  METHOD  req_pocomponents.
    ms_requested-pocomponents = abap_true.
  ENDMETHOD.


  METHOD  req_poaddrdelivery.
    ms_requested-poaddrdelivery = abap_true.
  ENDMETHOD.


  METHOD  req_poaccountx.
    ms_requested-poaccountx = abap_true.
  ENDMETHOD.


  METHOD  req_poaccountprofitsegment.
    ms_requested-poaccountprofitsegment = abap_true.
  ENDMETHOD.


  METHOD  req_poaccount.
    ms_requested-poaccount = abap_true.
  ENDMETHOD.


  METHOD  req_nfmetallitms.
    ms_requested-nfmetallitms = abap_true.
  ENDMETHOD.


  METHOD  req_invplanitemx.
    ms_requested-invplanitemx = abap_true.
  ENDMETHOD.


  METHOD  req_invplanitem.
    ms_requested-invplanitem = abap_true.
  ENDMETHOD.


  METHOD  req_invplanheaderx.
    ms_requested-invplanheaderx = abap_true.
  ENDMETHOD.


  METHOD  req_invplanheader.
    ms_requested-invplanheader = abap_true.
  ENDMETHOD.


  METHOD  req_extensionout.
    ms_requested-extensionout = abap_true.
  ENDMETHOD.


  METHOD  req_extensionin.
    ms_requested-extensionin = abap_true.
  ENDMETHOD.


  METHOD  req_exppoexpimpheader.
    ms_requested-exppoexpimpheader = abap_true.
  ENDMETHOD.


  METHOD  req_expheader.
    ms_requested-expheader = abap_true.
  ENDMETHOD.


  METHOD  req_allversions.
    ms_requested-allversions = abap_true.
  ENDMETHOD.
ENDCLASS.
