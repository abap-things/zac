
CLASS zcl_ac_dom_subc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: ty_value_type TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF cs_value,
        _1 TYPE ty_value_type VALUE '1',
        x  TYPE ty_value_type VALUE 'X',
        b  TYPE ty_value_type VALUE 'B',
        s  TYPE ty_value_type VALUE 'S',
        i  TYPE ty_value_type VALUE 'I',
        m  TYPE ty_value_type VALUE 'M',
        f  TYPE ty_value_type VALUE 'F',
        t  TYPE ty_value_type VALUE 'T',
        k  TYPE ty_value_type VALUE 'K',
        q  TYPE ty_value_type VALUE 'Q',
        j  TYPE ty_value_type VALUE 'J',
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



CLASS ZCL_AC_DOM_SUBC IMPLEMENTATION.


  METHOD class_constructor.
    INSERT VALUE #( value = '1' text = 'Executable program' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'X' text = 'Transformation (XSLT- oder ST-Programm)' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'B' text = 'Behavior-Definition' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'S' text = 'Subroutinenpool' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'I' text = 'INCLUDE program' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'M' text = 'Modulpool' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'F' text = 'Funktionsgruppe' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'T' text = 'Type pool' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'K' text = 'Class pool' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'Q' text = 'Datenbankprozedur-Proxy' ) INTO TABLE mt_value2text.
    INSERT VALUE #( value = 'J' text = 'Interface pool' ) INTO TABLE mt_value2text.
  ENDMETHOD.


  METHOD value2text.
    rv_text = VALUE #( mt_value2text[ value = iv_value ]-text OPTIONAL ).
  ENDMETHOD.
ENDCLASS.
