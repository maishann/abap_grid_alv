*&---------------------------------------------------------------------*
*& Report ZTEST_ALV_GRID                                               *
*&---------------------------------------------------------------------*
*& Date:   06.09.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description:                                                        *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
REPORT ztest_alv_grid.
DATA ok_code_0100 TYPE sy-ucomm.
DATA gv_title     TYPE sy-title.
DATA go_grid      TYPE REF TO zbc_grid_alv.
DATA lt_column    TYPE zbc_grid_alv=>tt_column.

INITIALIZATION.

START-OF-SELECTION.
  gv_title = sy-title.

  SELECT * FROM bkpf
    WHERE bukrs = '9000'
    INTO TABLE @DATA(lt_bkpf).

  TRY.
      go_grid = NEW zbc_grid_alv( iv_container = 'CONTAINER' ).
    CATCH zcx_grid_alv INTO DATA(error).
      MESSAGE error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
  ENDTRY.

  APPEND VALUE #( columnname = 'BELNR'
                  hotspot    = abap_true )
         TO lt_column.

  CALL SCREEN 100.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR  'TITLE_0100' WITH gv_title.

  DATA(lt_fcat) = go_grid->generate_fcat( it_outtab = lt_bkpf[]
                                          it_column = lt_column[] ).

  TRY.
      go_grid->set_table_for_first_display( EXPORTING iv_repid        = sy-repid
                                                      iv_save         = 'A'
                                            CHANGING  it_outtab       = lt_bkpf[]
                                                      it_fieldcatalog = lt_fcat[] ).
    CATCH zcx_grid_alv INTO DATA(error2).
      MESSAGE error2->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_grid->handle_user_command( e_ucomm = ok_code_0100 ).
ENDMODULE.
