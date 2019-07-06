REPORT ztest_replace_type_code_w_clas.

class lcl_person DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      co_o  TYPE i VALUE 0,
      co_a  TYPE i VALUE 1,
      co_b  TYPE i VALUE 2,
      co_ab TYPE i VALUE 3.

      methods:
      constructor IMPORTING i_blood_group type i,
      set_blood_group IMPORTING i_arg type i,
      get_blood_group RETURNING VALUE(r_result) type i.

private section.
  data: blood_group type i.
endclass.

CLASS lcl_person IMPLEMENTATION.
  METHOD constructor.
    blood_group = i_blood_group.
  ENDMETHOD.

  METHOD set_blood_group.
    blood_group = i_arg.
  ENDMETHOD.

  METHOD get_blood_group.
    r_result = blood_group.
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

    person1 = NEW #( i_blood_group = 1 ).
    person2 = NEW #( i_blood_group = lcl_person=>co_ab ).
    person3 = NEW #( i_blood_group = 4 ). " Unexpected value is applicable.
  ENDMETHOD.
ENDCLASS.
