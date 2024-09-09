*&---------------------------------------------------------------------*
*& Report ZZ_PROTECT_ALV_LAYOUT                                        *
*&---------------------------------------------------------------------*
*& Date:   09.09.2024                                                  *
*& Author: Hannes Maisch (HANNESM)                                     *
*& Company:                                                            *
*& Requested from:                                                     *
*& Description:                                                        *
*&---------------------------------------------------------------------*
*& Change History                                                      *
*& Date        | Author   | CR &  Description                          *
*&---------------------------------------------------------------------*
REPORT zz_protect_alv_layout.

DATA c_protection_character TYPE c LENGTH 1 VALUE '~'.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  PARAMETERS  p_report TYPE repid      OBLIGATORY.
  PARAMETERS p_handle TYPE slis_handl OBLIGATORY.
  PARAMETERS p_layout TYPE slis_vari  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_layout.

AT SELECTION-SCREEN ON p_layout.
  IF p_layout(1) <> '/'.
    MESSAGE 'Nur ungeschütze Standardlayouts (beginnend mit /) können geschützt werden' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  PERFORM protect.

*&---------------------------------------------------------------------*
*&      Form  PROTECT
*&---------------------------------------------------------------------*
* Replace first character in ALV-Layout with "unallowed" character
* This way this variant can't be saved any more and thus not be overwritten
*&---------------------------------------------------------------------*
FORM protect.
  DATA lt_ltdx  TYPE STANDARD TABLE OF ltdx WITH NON-UNIQUE DEFAULT KEY.
  DATA ls_ltdx  LIKE LINE OF lt_ltdx.
  DATA lt_ltdxt TYPE STANDARD TABLE OF ltdxt WITH NON-UNIQUE DEFAULT KEY.
  DATA ls_ltdxt LIKE LINE OF lt_ltdxt.

  " == read layout (main data)
  SELECT * INTO TABLE lt_ltdx
    FROM ltdx
    WHERE relid   = 'LT'
      AND report  = p_report
      AND handle  = p_handle
      AND variant = p_layout
      AND type    = 'F'.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " ---------------------------------------------------------------------
  " Replace first character in variant with unallowed character
  " ---------------------------------------------------------------------
  ls_ltdx-variant    = p_layout.
  ls_ltdx-variant(1) = c_protection_character.

  MODIFY lt_ltdx FROM ls_ltdx TRANSPORTING variant WHERE variant <> ls_ltdx-variant.

  TRY.
      " == Insert layout (main data)
      INSERT ltdx FROM TABLE lt_ltdx.
      " == read layout (texts)
      SELECT * FROM ltdxt
        INTO TABLE lt_ltdxt
        WHERE relid   = 'LT'
          AND report  = p_report
          AND handle  = p_handle
          AND variant = p_layout
          AND type    = 'F'.
      IF sy-subrc = 0.
        " == Insert Layout (text)
        ls_ltdxt-variant = ls_ltdx-variant.
        MODIFY lt_ltdxt FROM ls_ltdxt TRANSPORTING variant WHERE variant <> ls_ltdxt-variant.
        INSERT ltdxt FROM TABLE lt_ltdxt.
      ENDIF.
      COMMIT WORK.
      MESSAGE i000(oo) WITH 'Geschützte Variante' ls_ltdx-variant 'wurde neu angelegt'.
    CATCH cx_sy_open_sql_db.
      MESSAGE 'ALV-Layout existiert schon und wird nicht überschrieben' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_LAYOUT
*&---------------------------------------------------------------------*
FORM f4_layout.
  DATA lv_repid      TYPE syrepid.
  DATA lv_dynnr      TYPE sydynnr.
  DATA lt_dynpfields TYPE STANDARD TABLE OF dynpread WITH NON-UNIQUE DEFAULT KEY.
  DATA ls_variant    TYPE disvariant.

  FIELD-SYMBOLS <ls_dynpfield> LIKE LINE OF lt_dynpfields.

  " == Layout parameters
  ls_variant-report  = p_report.
  ls_variant-variant = p_layout.
  ls_variant-handle  = p_handle.

  " == updating from current screen
  " ---------------------------------------------------------------------
  lv_repid = sy-repid.
  lv_dynnr = '1000'.

  CALL FUNCTION 'DYNP_VALUES_READ'                          "#EC
    EXPORTING dyname             = lv_repid
              dynumb             = lv_dynnr
              translate_to_upper = 'X'
              request            = 'A'
    TABLES    dynpfields         = lt_dynpfields
              exceptions         = 11.

  ASSIGN lt_dynpfields[ fieldname = 'P_REPORT' ] TO <ls_dynpfield>.
  IF sy-subrc = 0.
    ls_variant-report = <ls_dynpfield>-fieldvalue.
  ENDIF.

  ASSIGN lt_dynpfields[ fieldname = 'P_HANDLE' ] TO <ls_dynpfield>.
  IF sy-subrc = 0.
    ls_variant-handle = <ls_dynpfield>-fieldvalue.
  ENDIF.

  " ---------------------------------------------------------------------
  " Standard ALV-F4
  " ---------------------------------------------------------------------
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING  is_variant         = ls_variant
               i_save             = 'X'
               i_display_via_grid = 'X'  " Only standard variants here
    IMPORTING  es_variant         = ls_variant
    EXCEPTIONS not_found          = 1
               program_error      = 2
               OTHERS             = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
  IF ls_variant-variant IS INITIAL.
    RETURN.
  ENDIF.
  p_layout = ls_variant-variant.
ENDFORM.
