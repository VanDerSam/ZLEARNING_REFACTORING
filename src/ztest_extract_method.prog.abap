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
    DATA: name   TYPE string,
          orders TYPE STANDARD TABLE OF REF TO lcl_order WITH DEFAULT KEY.
    METHODS print_banner.
    METHODS print_details
      IMPORTING
        i_outstanding TYPE f.
    METHODS get_outstanding
      RETURNING
        VALUE(r_result) TYPE f.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
  ENDMETHOD.

  METHOD print_owing.
    DATA: outstanding TYPE f VALUE '0.0'.

    print_banner( ).
    outstanding = get_outstanding( ).
    print_details( outstanding ).
  ENDMETHOD.

  METHOD print_banner.
    " Show banner
    WRITE `******************************`.
    WRITE `******* Customer owes ********`.
    WRITE `******************************`.
  ENDMETHOD.

  METHOD print_details.
    " Show details
    WRITE `Name: ` && name.
    WRITE `Amount ` && i_outstanding.
  ENDMETHOD.

  METHOD get_outstanding.
    " Calculate owing
    LOOP AT orders ASSIGNING FIELD-SYMBOL(<order>).
      r_result = r_result + <order>->get_amount( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
