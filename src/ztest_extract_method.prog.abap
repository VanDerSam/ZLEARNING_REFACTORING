REPORT ztest_extract_method.

CLASS lcl_order DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      get_amount RETURNING VALUE(r_result) TYPE f.
ENDCLASS.

CLASS lcl_order IMPLEMENTATION.
  METHOD get_amount.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      main.

    METHODS:
      print_owing IMPORTING i_amount TYPE f.
  PRIVATE SECTION.
    DATA name TYPE string.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
  ENDMETHOD.

  METHOD print_owing.
    DATA: orders      TYPE STANDARD TABLE OF REF TO lcl_order WITH DEFAULT KEY,
          outstanding TYPE f VALUE '0.0'.

    " Show banner
    WRITE `******************************`.
    WRITE `******** Client owing ********`.
    WRITE `******************************`.

    " Calculate owing
    LOOP AT orders ASSIGNING FIELD-SYMBOL(<order>).
      outstanding = outstanding + <order>->get_amount( ).
    ENDLOOP.

    " Show details
    WRITE `Name: ` && name.
    WRITE `Amount ` && outstanding.
  ENDMETHOD.
ENDCLASS.
