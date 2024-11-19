
CLASS zcl_ac_dom_abapvrs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: ty_value_type TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF cs_value,
        _2 TYPE ty_value_type VALUE '2',
        _3 TYPE ty_value_type VALUE '3',
        _4 TYPE ty_value_type VALUE '4',
        _5 TYPE ty_value_type VALUE '5',
        x  TYPE ty_value_type VALUE 'X',
        _  TYPE ty_value_type VALUE '',
      END OF cs_value.

    CLASS-METHODS class_constructor.

    CLASS-METHODS value2text
      IMPORTING
        iv_value       TYPE ty_value_type
      RETURNING
        VALUE(rv_text) TYPE val_text.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF tys_value2text,
        value TYPE ty_value_type,
        text  TYPE val_text,
      END OF tys_value2text.

    TYPES: tyt_value2text TYPE HASHED TABLE OF tys_value2text WITH UNIQUE KEY value.

    CLASS-DATA mt_value2text TYPE tyt_value2text.

ENDCLASS.



CLASS ZCL_AC_DOM_ABAPVRS IMPLEMENTATION.


  METHOD class_constructor.
    INSERT VALUE #( value = '2' text = 'ABAP for key users' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = '3' text = 'Static ABAP with limited object use (obsolete)' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = '4' text = 'Standard ABAP with limited object use (obsolete)' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = '5' text = 'ABAP for SAP Cloud Platform' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'X' text = 'Standard-ABAP (Unicode)' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = '' text = 'Non-Unicode ABAP (obsolete)' ) INTO TABLE mt_value2text.
  ENDMETHOD.


  METHOD value2text.
    rv_text = VALUE #( mt_value2text[ value = iv_value ]-text OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
