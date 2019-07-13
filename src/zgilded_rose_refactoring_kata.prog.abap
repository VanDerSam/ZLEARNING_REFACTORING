REPORT zgilded_rose_refactoring_kata.

*&---------------------------------------------------------------------*
*&   Gilded Rose Requirements Specification
*&---------------------------------------------------------------------*
*&
*& Hi and welcome to team Gilded Rose. As you know, we are a small inn with
*& a prime location in a prominent city ran by a friendly innkeeper named
*& Allison. We also buy and sell only the finest goods. Unfortunately, our
*& goods are constantly degrading in quality as they approach their sell by
*& date. We have a system in place that updates our inventory for us. It
*& was developed by a no-nonsense type named Leeroy, who has moved on to
*& new adventures. Your task is to add the new feature to our system so that
*& we can begin selling a new category of items.
*&
*& First an introduction to our system:
*&
*&  - All items have a Sell In value which denotes the number of
*&           days we have to sell the item
*&  - All items have a Quality value which denotes how valuable the item is
*&  - At the end of each day our system lowers both values for every item
*&
*& Seems pretty simple, right? Well this is where it gets interesting:
*&
*&  - Once the sell by date has passed, Quality degrades twice as fast
*&  - The Quality of an item is never negative
*&  - "Aged Brie" actually increases in Quality the older it gets
*&  - The Quality of an item is never more than 50
*&  - "Sulfuras", being a legendary item, never has to be sold or
*&           decreases in Quality
*&  - "Backstage passes", like aged brie, increases in Quality as its
*&           Sell In value approaches; Quality increases by 2 when there
*&           are 10 days or less and by 3 when there are 5 days or less
*&           but Quality drops to 0 after the concert
*&
*& We have recently signed a supplier of conjured items. This requires an
*& update to our system:
*&
*&  - "Conjured" items degrade in Quality twice as fast as normal items
*&
*& Feel free to make any changes to the Update Quality method and add any new
*& code as long as everything still works correctly. However, do not alter
*& the Item class directly or Items table attribute as those belong to the
*& goblin in the corner who will insta-rage and one-shot you as he doesn't
*& believe in shared code ownership (you can make the Update Quality method
*& and Items property static if you must, we'll cover for you).
*&
*& Just for clarification, an item can never have its Quality increase
*& above 50, however "Sulfuras" is a legendary item and as such its Quality
*& is 80 and it never alters.

*& Production Code - Class Library
CLASS lcl_item DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name    TYPE string
                  iv_sell_in TYPE i
                  iv_quality TYPE i,
      description
        RETURNING VALUE(rv_string) TYPE string.
    DATA:
      mv_name    TYPE string,
      mv_sell_in TYPE i,
      mv_quality TYPE i.
ENDCLASS.

CLASS lcl_item_controller DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      update IMPORTING i_item TYPE REF TO lcl_item.

  PROTECTED SECTION.
    DATA: item TYPE REF TO lcl_item.

    METHODS:
      increase_quality,
      decrease_quality,
      decrease_sellin.
ENDCLASS.

CLASS lcl_item_controller IMPLEMENTATION.
  METHOD update.
    item = i_item.
  ENDMETHOD.

  METHOD increase_quality.
    IF item->mv_quality < 50.
      item->mv_quality = item->mv_quality + 1.
    ENDIF.
  ENDMETHOD.

  METHOD decrease_quality.
    IF item->mv_quality > 0.
      item->mv_quality = item->mv_quality - 1.
    ENDIF.
  ENDMETHOD.

  METHOD decrease_sellin.
    item->mv_sell_in = item->mv_sell_in - 1.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_normal DEFINITION INHERITING FROM lcl_item_controller.
  PUBLIC SECTION.
    METHODS: update REDEFINITION.
ENDCLASS.

CLASS lcl_item_controller_normal IMPLEMENTATION.
  METHOD update.
    super->update( i_item ).
    decrease_quality( ).
    IF item->mv_sell_in <= 0.
      decrease_quality( ).
    ENDIF.
    decrease_sellin( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_agedbrie DEFINITION INHERITING FROM lcl_item_controller.
  PUBLIC SECTION.
    METHODS: update REDEFINITION.
ENDCLASS.

CLASS lcl_item_controller_agedbrie IMPLEMENTATION.
  METHOD update.
    super->update( i_item ).
    increase_quality( ).
    IF item->mv_sell_in <= 0.
      increase_quality( ).
    ENDIF.
    decrease_sellin( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_sulfuras DEFINITION INHERITING FROM lcl_item_controller.
  PUBLIC SECTION.
    METHODS: update REDEFINITION.
ENDCLASS.

CLASS lcl_item_controller_sulfuras IMPLEMENTATION.
  METHOD update.
    super->update( i_item ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_backstage DEFINITION INHERITING FROM lcl_item_controller.
  PUBLIC SECTION.
    METHODS: update REDEFINITION.
ENDCLASS.

CLASS lcl_item_controller_backstage IMPLEMENTATION.
  METHOD update.
    super->update( i_item ).
    IF i_item->mv_sell_in <= 0.
      i_item->mv_quality = 0.
    ELSE.
      increase_quality( ).
      IF i_item->mv_sell_in <= 5.
        increase_quality( ).
        increase_quality( ).
      ELSEIF i_item->mv_sell_in <= 10.
        increase_quality( ).
      ENDIF.
    ENDIF.
    decrease_sellin( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_conjured DEFINITION INHERITING FROM lcl_item_controller.
  PUBLIC SECTION.
    METHODS: update REDEFINITION.
ENDCLASS.

CLASS lcl_item_controller_conjured IMPLEMENTATION.
  METHOD update.
    super->update( i_item ).
    decrease_quality( ).
    decrease_quality( ).
    decrease_sellin( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item_controller_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_controller IMPORTING i_item              TYPE REF TO lcl_item
                     RETURNING VALUE(r_controller) TYPE REF TO lcl_item_controller.
ENDCLASS.

CLASS lcl_item_controller_factory IMPLEMENTATION.
  METHOD get_controller.
    CASE i_item->mv_name.
      WHEN `Aged Brie`.
        r_controller = NEW lcl_item_controller_agedbrie( ).

      WHEN `Sulfuras, Hand of Ragnaros`.
        r_controller = NEW lcl_item_controller_sulfuras( ).

      WHEN `Backstage passes to a TAFKAL80ETC concert`.
        r_controller = NEW lcl_item_controller_backstage( ).

      WHEN `Conjured Mana Cake`.
        r_controller = NEW lcl_item_controller_conjured( ).

      WHEN OTHERS.
        r_controller = NEW lcl_item_controller_normal( ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_gilded_rose DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_items TYPE STANDARD TABLE OF REF TO lcl_item WITH EMPTY KEY.
    METHODS:
      constructor
        IMPORTING it_items TYPE tt_items,
      update_quality.

  PRIVATE SECTION.
    DATA:
      mt_items TYPE tt_items.
ENDCLASS.

CLASS lcl_gilded_rose IMPLEMENTATION.
  METHOD constructor.
    mt_items = it_items.
  ENDMETHOD.

  METHOD update_quality.
    LOOP AT mt_items ASSIGNING FIELD-SYMBOL(<item>).
      DATA(controller) = lcl_item_controller_factory=>get_controller( <item> ).
      controller->update( <item> ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_item IMPLEMENTATION.
  METHOD constructor.
    mv_name    = iv_name.
    mv_sell_in = iv_sell_in.
    mv_quality = iv_quality.
  ENDMETHOD.

  METHOD description.
    rv_string = |{ mv_name }, { mv_sell_in }, { mv_quality }|.
  ENDMETHOD.
ENDCLASS.


*& Test Code - Executable Text Test Fixture
CLASS lth_texttest_fixture DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lth_texttest_fixture IMPLEMENTATION.
  METHOD main.
    DATA(lo_out) = cl_demo_output=>new( )->write_text( |OMGHAI!| ).

    DATA(lt_items) = VALUE lcl_gilded_rose=>tt_items(
                        ( NEW #( iv_name    = |+5 Dexterity Vest|
                                 iv_sell_in = 10
                                 iv_quality = 20 ) )
                        ( NEW #( iv_name    = |Aged Brie|
                                 iv_sell_in = 2
                                 iv_quality = 0 ) )
                        ( NEW #( iv_name    = |Elixir of the Mongoose|
                                 iv_sell_in = 5
                                 iv_quality = 7 ) )
                        ( NEW #( iv_name    = |Sulfuras, Hand of Ragnaros|
                                 iv_sell_in = 0
                                 iv_quality = 80 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 15
                                 iv_quality = 20 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 10
                                 iv_quality = 49 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 5
                                 iv_quality = 49 ) )
                        "This conjured item does not work properly yet
                        ( NEW #( iv_name    = |Conjured Mana Cake|
                                 iv_sell_in = 3
                                 iv_quality = 6 ) ) ).

    DATA(lo_app) = NEW lcl_gilded_rose( it_items = lt_items ).

    DATA(lv_days) = 2.
    cl_demo_input=>request( EXPORTING text = |Number of Days?|
                            CHANGING field = lv_days ).

    DO lv_days TIMES.
      lo_out->next_section( |-------- day { sy-index } --------| ).
      lo_out->write_text( |Name, Sell_In, Quality| ).
      LOOP AT lt_items INTO DATA(lo_item).
        lo_out->write_text( lo_item->description( ) ).
      ENDLOOP.
      lo_app->update_quality( ).
    ENDDO.

    lo_out->display( ).
  ENDMETHOD.
ENDCLASS.

*& Test Code - Currently Broken
CLASS ltc_gilded_rose DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      equals IMPORTING
                       i_item_1        TYPE REF TO lcl_item
                       i_item_2        TYPE REF TO lcl_item
             RETURNING VALUE(r_result) TYPE abap_bool,

      test_update_quality FOR TESTING. " DDT test/Parameterized test
ENDCLASS.

CLASS ltc_gilded_rose IMPLEMENTATION.
  METHOD equals.
    IF i_item_1->mv_name = i_item_2->mv_name AND i_item_1->mv_sell_in = i_item_2->mv_sell_in AND i_item_1->mv_quality = i_item_2->mv_quality.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD test_update_quality.
    TYPES: BEGIN OF test_data_per_test_t,
             test_id          TYPE string,
             item_before      TYPE REF TO lcl_item,
             days_to_simulate TYPE i,
             item_after       TYPE REF TO lcl_item,
           END OF   test_data_per_test_t,

           test_data_t TYPE STANDARD TABLE OF test_data_per_test_t WITH DEFAULT KEY.

    DATA: test_data TYPE test_data_t,
          items     TYPE lcl_gilded_rose=>tt_items.

    test_data = VALUE #( ( test_id = `NormalItemDecreasedBy1`
                           item_before = NEW #( iv_name = `Test item` iv_sell_in = 10 iv_quality = 10 )
                           days_to_simulate = 1
                           item_after = NEW #( iv_name = `Test item` iv_sell_in = 9 iv_quality = 9 ) )
                         ( test_id = `NormalItemWithSellin0DecreasedBy2`
                           item_before = NEW #( iv_name = `Test item` iv_sell_in = 0 iv_quality = 10 )
                           days_to_simulate = 1
                           item_after = NEW #( iv_name = `Test item` iv_sell_in = -1 iv_quality = 8 ) )
                         ( test_id = `QualityOfItemIsNeverNegative`
                           item_before = NEW #( iv_name = `Test item` iv_sell_in = 10 iv_quality = 3 )
                           days_to_simulate = 5
                           item_after = NEW #( iv_name = `Test item` iv_sell_in = 5 iv_quality = 0 ) )
                         ( test_id = `AgedBrieItemIncreasesQuality`
                           item_before = NEW #( iv_name = `Aged Brie` iv_sell_in = 3 iv_quality = 0 )
                           days_to_simulate = 3
                           item_after = NEW #( iv_name = `Aged Brie` iv_sell_in = 0 iv_quality = 3 ) )
                         ( test_id = `AgedBrieItemWithSellin0IncreasesBy2`
                           item_before = NEW #( iv_name = `Aged Brie` iv_sell_in = 3 iv_quality = 0 )
                           days_to_simulate = 5
                           item_after = NEW #( iv_name = `Aged Brie` iv_sell_in = -2 iv_quality = 7 ) )
                         ( test_id = `QualityOfItemIsNeverMoreThan50_1`
                           item_before = NEW #( iv_name = `Aged Brie` iv_sell_in = 5 iv_quality = 50 )
                           days_to_simulate = 5
                           item_after = NEW #( iv_name = `Aged Brie` iv_sell_in = 0 iv_quality = 50 ) )
                         ( test_id = `QualityOfItemIsNeverMoreThan50_2`
                           item_before = NEW #( iv_name = `Aged Brie` iv_sell_in = 5 iv_quality = 47 )
                           days_to_simulate = 5
                           item_after = NEW #( iv_name = `Aged Brie` iv_sell_in = 0 iv_quality = 50 ) )
                         ( test_id = `SulfurasItemHasConstantQuality`
                           item_before = NEW #( iv_name = `Sulfuras, Hand of Ragnaros` iv_sell_in = 5 iv_quality = 60 )
                           days_to_simulate = 5
                           item_after = NEW #( iv_name = `Sulfuras, Hand of Ragnaros` iv_sell_in = 5 iv_quality = 60 ) )
                         ( test_id = `BackstagePassesItemIncreasesBy1`
                           item_before = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 12 iv_quality = 10 )
                           days_to_simulate = 2
                           item_after = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 10 iv_quality = 12 ) )
                         ( test_id = `BackstagePassesItemIncreasesBy2WhenSellin<=10`
                           item_before = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 10 iv_quality = 10 )
                           days_to_simulate = 2
                           item_after = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 8 iv_quality = 14 ) )
                         ( test_id = `BackstagePassesItemIncreasesBy3WhenSellin<=5`
                           item_before = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 5 iv_quality = 10 )
                           days_to_simulate = 2
                           item_after = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 3 iv_quality = 16 ) )
                         ( test_id = `BackstagePassesItemDropsQualityTo0WhenSellin<0`
                           item_before = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = 2 iv_quality = 0 )
                           days_to_simulate = 3
                           item_after = NEW #( iv_name = `Backstage passes to a TAFKAL80ETC concert` iv_sell_in = -1 iv_quality = 0 ) )
                         " A new requirement/feature
                         ( test_id = `ConjuredDegradeBy2`
                           item_before = NEW #( iv_name = `Conjured Mana Cake` iv_sell_in = 2 iv_quality = 10 )
                           days_to_simulate = 2
                           item_after = NEW #( iv_name = `Conjured Mana Cake` iv_sell_in = 0 iv_quality = 6 ) )
                         ( test_id = `ConjuredHasQuality>0`
                           item_before = NEW #( iv_name = `Conjured Mana Cake` iv_sell_in = 0 iv_quality = 0 )
                           days_to_simulate = 2
                           item_after = NEW #( iv_name = `Conjured Mana Cake` iv_sell_in = -2 iv_quality = 0 ) )
                       ).

    LOOP AT test_data ASSIGNING FIELD-SYMBOL(<test_data_per_test>).
      CLEAR items.
      INSERT <test_data_per_test>-item_before INTO TABLE items.

      DATA(inn) = NEW lcl_gilded_rose( it_items = items ).

      DO <test_data_per_test>-days_to_simulate TIMES.
        inn->update_quality( ).
      ENDDO.

      cl_abap_unit_assert=>assert_equals( exp = abap_true
                                          act = equals( i_item_1 = <test_data_per_test>-item_after
                                                        i_item_2 = items[ 1 ] )
                                          msg = |Test = { <test_data_per_test>-test_id }| ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lth_texttest_fixture=>main( ).
