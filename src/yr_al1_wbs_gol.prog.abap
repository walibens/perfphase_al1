REPORT yr_al1_wbs_gol.

CLASS cx_gol_wrong_value DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS cx_gol_wrong_value IMPLEMENTATION.
ENDCLASS.

CLASS lcl_cell DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING iv_state TYPE abap_bool,
      state RETURNING VALUE(rv_state) TYPE abap_bool,
      set_dead,
      set_alive.

  PRIVATE SECTION.
    DATA :
      mv_state        TYPE abap_bool.
ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.
  METHOD set_alive.
    mv_state = abap_true.
  ENDMETHOD.

  METHOD set_dead.
    mv_state = abap_false.
  ENDMETHOD.

  METHOD state.
    rv_state = mv_state.
  ENDMETHOD.

  METHOD constructor.
    mv_state        = iv_state.
  ENDMETHOD.
ENDCLASS.

CLASS ltc_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA :
             mo_cut TYPE REF TO lcl_cell.
    METHODS:
      setup,
      is_dead FOR TESTING RAISING cx_static_check,
      is_alive FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_cell IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( abap_true ).
  ENDMETHOD.

  METHOD is_dead.
    mo_cut->set_dead( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_false act = mo_cut->state( ) ).
  ENDMETHOD.

  METHOD is_alive.
    mo_cut->set_alive( ).
    cl_abap_unit_assert=>assert_equals( exp = abap_true act = mo_cut->state( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_grid DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES :
      BEGIN OF ty_grid,
        coordinate_x TYPE i,
        coordinate_y TYPE i,
        cell         TYPE REF TO lcl_cell,
      END OF ty_grid.
    DATA :
      mt_grid TYPE STANDARD TABLE OF ty_grid.
    METHODS :
      constructor IMPORTING iv_grid_size TYPE i,
      get_nb_of_living_neighbors IMPORTING iv_coordinate_x  TYPE i
                                           iv_coordinate_y  TYPE i
                                 RETURNING VALUE(rv_return) TYPE i.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_grid IMPLEMENTATION.

  METHOD constructor.
    DATA :
      lv_x TYPE i,
      lv_y TYPE i.

    DO iv_grid_size TIMES.
      lv_x += 1.
      DO iv_grid_size TIMES.
        lv_y += 1.
        APPEND VALUE #( coordinate_x = lv_x
                        coordinate_y = lv_y
                        cell = NEW #( abap_false ) ) TO mt_grid.
      ENDDO.
      CLEAR lv_y.
    ENDDO.
  ENDMETHOD.

  METHOD get_nb_of_living_neighbors.

  ENDMETHOD.

ENDCLASS.



CLASS lcl_gol DEFINITION FINAL.

  PUBLIC SECTION.
    DATA :
      mo_grid TYPE REF TO lcl_grid.
    METHODS :
      constructor IMPORTING iv_grid_size TYPE i
                  RAISING   cx_gol_wrong_value,
      play        IMPORTING iv_iteration TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_grid_size TYPE i.
    METHODS validate RAISING cx_gol_wrong_value.
    METHODS initialize_grid.
    METHODS is_alive_and_less_2_neighbors
      IMPORTING iv_y             TYPE i
                iv_x             TYPE i
      RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS is_alive_and_more_2_neighbors
      IMPORTING iv_y             TYPE i
                iv_x             TYPE i
      RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS is_alive_and_2_3_neighbors
      IMPORTING iv_x             TYPE i
                iv_y             TYPE i
      RETURNING VALUE(rv_result) TYPE abap_bool.
    METHODS is_dead_and_more_2_neighbors
      IMPORTING iv_x             TYPE i
                iv_y             TYPE i
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_gol IMPLEMENTATION.

  METHOD constructor.
    mv_grid_size = iv_grid_size.

    TRY.
        validate( ).
      CATCH cx_gol_wrong_value.
        RAISE EXCEPTION TYPE cx_gol_wrong_value.
    ENDTRY.

    initialize_grid(  ).

  ENDMETHOD.


  METHOD play.
    DATA :
      lv_x TYPE i,
      lv_y TYPE i.

    DO iv_iteration TIMES.
      DO mv_grid_size TIMES.
        lv_x += 1.
        DO mv_grid_size TIMES.
          lv_y += 1.

          IF is_alive_and_less_2_neighbors( iv_y = lv_y  iv_x = lv_x ) OR
             is_alive_and_more_2_neighbors( iv_y = lv_y iv_x = lv_x ) OR
             is_alive_and_2_3_neighbors( iv_x = lv_x iv_y = lv_y ).
            mo_grid->mt_grid[ coordinate_x = lv_x coordinate_y = lv_y ]-cell->set_dead(  ).
          ENDIF.
          IF is_dead_and_more_2_neighbors( iv_x = lv_x iv_y = lv_y ).
            mo_grid->mt_grid[ coordinate_x = lv_x coordinate_y = lv_y ]-cell->set_alive(  ).
          ENDIF.
        ENDDO.
        CLEAR lv_y.
      ENDDO.
      CLEAR lv_x.
    ENDDO.
  ENDMETHOD.

  METHOD is_dead_and_more_2_neighbors.

  ENDMETHOD.

  METHOD is_alive_and_2_3_neighbors.

  ENDMETHOD.

  METHOD is_alive_and_more_2_neighbors.

  ENDMETHOD.

  METHOD is_alive_and_less_2_neighbors.

  ENDMETHOD.

  METHOD validate.
    IF mv_grid_size <= 1.
      RAISE EXCEPTION TYPE cx_gol_wrong_value.
    ENDIF.
  ENDMETHOD.


  METHOD initialize_grid.
    mo_grid = NEW #( mv_grid_size ).
  ENDMETHOD.

ENDCLASS.



CLASS ltc_gol DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_gol.
    METHODS:
      setup,
      acceptance_test FOR TESTING RAISING cx_dynamic_check,
      grid_is_initialized FOR TESTING RAISING cx_static_check,
      raise_exception_in_validate FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_gol IMPLEMENTATION.

  METHOD setup.

  ENDMETHOD.

  METHOD grid_is_initialized.
    mo_cut = NEW #( iv_grid_size = 3 ).
    cl_abap_unit_assert=>assert_bound( act = mo_cut->mo_grid ).
  ENDMETHOD.

  METHOD raise_exception_in_validate.
    TRY.
        mo_cut = NEW #( iv_grid_size = -3 ).
      CATCH cx_gol_wrong_value.
        DATA(exception_catched) = abap_true.
    ENDTRY.

    cl_abap_unit_assert=>assert_true( exception_catched ).
  ENDMETHOD.

  METHOD acceptance_test.
    mo_cut = NEW #( iv_grid_size = 5 ).
    mo_cut->play( iv_iteration = 3 ).
  ENDMETHOD.

ENDCLASS.
