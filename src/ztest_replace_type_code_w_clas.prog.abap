REPORT ztest_replace_type_code_w_clas.

CLASS lcl_blood_group DEFINITION
      CREATE PRIVATE.
PUBLIC SECTION.
  CLASS-DATA: co_o  TYPE REF TO lcl_blood_group READ-ONLY,
              co_a  TYPE REF TO lcl_blood_group READ-ONLY,
              co_b  TYPE REF TO lcl_blood_group READ-ONLY,
              co_ab TYPE REF TO lcl_blood_group READ-ONLY.

  CLASS-METHODS:
    class_constructor.

PRIVATE SECTION.
  DATA: _code TYPE i.
  CLASS-DATA: _values TYPE STANDARD TABLE OF REF TO lcl_blood_group WITH KEY table_line.

  class-methods:
    code IMPORTING i_arg        TYPE i
           RETURNING VALUE(r_obj) TYPE REF TO lcl_blood_group.
  METHODS:
    constructor IMPORTING i_code TYPE i,
      get_code RETURNING VALUE(r_code) TYPE i.
ENDCLASS.

CLASS lcl_blood_group IMPLEMENTATION.
  METHOD constructor.
    _code = i_code.
  ENDMETHOD.

  METHOD get_code.
    r_code = _code.
  ENDMETHOD.

  METHOD code.
    read table _values index i_arg ASSIGNING FIELD-SYMBOL(<value>).
    if ( sy-subrc = 0 ).
      r_obj = <value>.
    endif.
  ENDMETHOD.

  METHOD class_constructor.
    co_o = NEW #( 0 ).
    APPEND co_o TO _values.
    co_a = NEW #( 1 ).
    APPEND co_a TO _values.
    co_b = NEW #( 2 ).
    APPEND co_b TO _values.
    co_ab = NEW #( 3 ).
    APPEND co_ab TO _values.
  ENDMETHOD.
ENDCLASS.

class lcl_person DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING i_blood_group TYPE ref to lcl_blood_group,
      set_blood_group IMPORTING i_arg TYPE ref to lcl_blood_group.

private section.
  data: blood_group type ref to lcl_blood_group.
endclass.

CLASS lcl_person IMPLEMENTATION.
  METHOD constructor.
    blood_group = i_blood_group.
  ENDMETHOD.

  METHOD set_blood_group.
    blood_group = i_arg.
  ENDMETHOD.
ENDCLASS.

class lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      main.
  ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA: person1 TYPE REF TO lcl_person,
          person2 TYPE REF TO lcl_person,
          person3 type ref to lcl_person.

    person1 = NEW #( i_blood_group = lcl_blood_group=>co_o ).
    person2 = NEW #( i_blood_group = lcl_blood_group=>co_ab ).
*    person3 = NEW #( i_blood_group = new #( 4 ) ). " Unexpected value is not applicable.
  ENDMETHOD.
ENDCLASS.
