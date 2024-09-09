CLASS zbc_grid_alv DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF lty_column,
        columnname TYPE lvc_fname,
        hotspot    TYPE boole_d,
      END OF lty_column.
    TYPES tt_column TYPE STANDARD TABLE OF lty_column.

    CLASS-DATA mo_container TYPE REF TO cl_gui_custom_container.
    CLASS-DATA mo_grid      TYPE REF TO cl_gui_alv_grid.
    CLASS-DATA ms_layout    TYPE lvc_s_layo.
    CLASS-DATA ms_variant   TYPE disvariant.

    METHODS constructor
      IMPORTING VALUE(iv_container) TYPE c
      RAISING   zcx_grid_alv.

    METHODS free.

    METHODS generate_fcat
      IMPORTING VALUE(iv_structure_name) TYPE dd02l-tabname  OPTIONAL
                VALUE(it_outtab)         TYPE STANDARD TABLE OPTIONAL
                VALUE(it_column)         TYPE tt_column      OPTIONAL
      RETURNING VALUE(rt_fcat)           TYPE lvc_t_fcat.

    METHODS set_layout
      IMPORTING VALUE(iv_sel_mode) TYPE lvc_libox DEFAULT 'D'.

    METHODS set_variant
      IMPORTING VALUE(iv_variant) TYPE slis_vari OPTIONAL
                VALUE(iv_repid)   TYPE sy-repid.

    METHODS set_table_for_first_display
      IMPORTING VALUE(iv_buffer_active)     TYPE any                    OPTIONAL
                VALUE(iv_bypassing_buffer)  TYPE char01                 OPTIONAL
                VALUE(iv_consistency_check) TYPE char1                  OPTIONAL
                VALUE(iv_structure_name)    TYPE dd02l-tabname          OPTIONAL
                VALUE(iv_save)              TYPE char01                 OPTIONAL
                VALUE(iv_default)           TYPE char01                 OPTIONAL
                VALUE(is_print)             TYPE lvc_s_prnt             OPTIONAL
                VALUE(it_special_groups)    TYPE lvc_t_sgrp             OPTIONAL
                VALUE(it_toolbar_excluding) TYPE ui_functions           OPTIONAL
                VALUE(it_hyperlink)         TYPE lvc_t_hype             OPTIONAL
                VALUE(it_alv_graphics)      TYPE dtc_t_tc               OPTIONAL
                VALUE(it_except_qinfo)      TYPE lvc_t_qinf             OPTIONAL
                VALUE(ir_salv_adapter)      TYPE REF TO if_salv_adapter OPTIONAL
                VALUE(iv_repid)             TYPE sy-repid
                VALUE(iv_variant)           TYPE slis_vari              OPTIONAL
      CHANGING  it_outtab                   TYPE STANDARD TABLE
                VALUE(it_fieldcatalog)      TYPE lvc_t_fcat             OPTIONAL
                VALUE(it_sort)              TYPE lvc_t_sort             OPTIONAL
                VALUE(it_filter)            TYPE lvc_t_filt             OPTIONAL
      RAISING   zcx_grid_alv.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.

    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no.
ENDCLASS.


CLASS zbc_grid_alv IMPLEMENTATION.
  METHOD constructor.
    IF mo_container IS INITIAL.
      CREATE OBJECT mo_container
        EXPORTING
*                   parent                      = " Parent container
                   container_name              = iv_container     " Name of the dynpro CustCtrl name to link this container to
*                   style                       = " Windows style attributes applied to this container
*                   lifetime                    = lifetime_default " Lifetime
*                   repid                       = " Dynpro to which this container is linked to
*                   dynnr                       = " Report to which this container is linked to
*                   no_autodef_progid_dynnr     = " dont autodefine progid and dynnr?
        EXCEPTIONS cntl_error                  = 1                " CNTL_ERROR
                   cntl_system_error           = 2                " CNTL_SYSTEM_ERROR
                   create_error                = 3                " CREATE_ERROR
                   lifetime_error              = 4                " LIFETIME_ERROR
                   lifetime_dynpro_dynpro_link = 5                " LIFETIME_DYNPRO_DYNPRO_LINK
                   OTHERS                      = 6.
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_grid_alv( textid = zcx_grid_alv=>error_cont ).
      ENDIF.
    ENDIF.

    IF mo_container IS NOT BOUND.
      RETURN.
    ENDIF.

    IF mo_grid IS INITIAL.
      CREATE OBJECT mo_grid
        EXPORTING
*                   i_shellstyle      = 0                " Control Style
*                   i_lifetime        = " Lifetime
                   i_parent          = mo_container     " Parent-Container
*                   i_appl_events     = space            " Ereignisse als Applikationsevents registrieren
*                   i_parentdbg       = " Internal, donnot use.
*                   i_applogparent    = " Container for application log
*                   i_graphicsparent  = " Container for graphics
*                   i_name            = " Name
*                   i_fcat_complete   = space            " boolsche Variable (X=true, space=false)
*                   o_previous_sral_handler =
        EXCEPTIONS error_cntl_create = 1                " Fehler beim Erzeugen des Controls
                   error_cntl_init   = 2                " Fehler beim Initialisieren des Controls
                   error_cntl_link   = 3                " Fehler beim Linken des Controls
                   error_dp_create   = 4                " Fehler beim Erzeugen des DataProvider Control
                   OTHERS            = 5.
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zcx_grid_alv( textid = zcx_grid_alv=>error_grid ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD free.
    FREE mo_container.
    FREE mo_grid.
  ENDMETHOD.

  METHOD generate_fcat.
    DATA lv_dnam      TYPE dd04l-rollname.
    DATA ls_dd04l     TYPE dd04l.
    DATA ls_fcat      TYPE lvc_s_fcat.
    DATA lt_dd04t     TYPE TABLE OF dd04t.
    DATA lo_struc_ref TYPE REF TO cl_abap_structdescr.
    DATA lo_elem_ref  TYPE REF TO cl_abap_elemdescr.

    FIELD-SYMBOLS <wa_comp>  TYPE abap_compdescr.
    FIELD-SYMBOLS <field>    TYPE any.
    FIELD-SYMBOLS <wa_dd04t> TYPE dd04t.
    FIELD-SYMBOLS <line>     TYPE any.

    CLEAR rt_fcat.
    IF iv_structure_name IS NOT INITIAL.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING  i_structure_name       = iv_structure_name
                   i_client_never_display = 'X'
        CHANGING   ct_fieldcat            = rt_fcat
        EXCEPTIONS inconsistent_interface = 1
                   program_error          = 2
                   OTHERS                 = 3.
      IF sy-subrc <> 0.
        " Implement suitable error handling here
      ENDIF.
    ENDIF.

    IF it_outtab IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN it_outtab[ 1 ] TO <line>.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO it_outtab.
      ASSIGN it_outtab[ 1 ] TO <line>.
    ENDIF.

    " --- get type infos of internal table
    lo_struc_ref ?= cl_abap_typedescr=>describe_by_data( <line> ).
    LOOP AT lo_struc_ref->components ASSIGNING <wa_comp>.
      ASSIGN COMPONENT <wa_comp>-name OF STRUCTURE <line> TO <field>.
      IF <field> IS NOT ASSIGNED.            " must be the case !!
        CONTINUE.
      ENDIF.
      CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.
        lo_elem_ref ?= cl_abap_typedescr=>describe_by_data( <field> ).
      ENDCATCH.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      CLEAR ls_fcat.
      ls_fcat-fieldname = <wa_comp>-name.
      lv_dnam = lo_elem_ref->help_id.
      " --- get texts and type descriptions of table component
      CALL FUNCTION 'DD_DTEL_GET'
        EXPORTING  langu         = sy-langu
                   withtext      = 'X'
                   roll_name     = lv_dnam
        IMPORTING  dd04l_wa_a    = ls_dd04l
        TABLES     dd04t_tab_a   = lt_dd04t
        EXCEPTIONS illegal_value = 1
                   OTHERS        = 2.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_dd04l TO ls_fcat.
        " --- some conversions (add entitytab for conversions)
        ls_fcat-ref_table = ls_dd04l-entitytab.
        IF ls_fcat-ref_table IS NOT INITIAL.
          ls_fcat-ref_field = ls_dd04l-domname.
        ENDIF.
        IF ls_dd04l-valexi = 'X'.
          ls_fcat-ref_table = ls_dd04l-domname.
        ENDIF.
        ASSIGN lt_dd04t[ 1 ] TO <wa_dd04t>.
        IF sy-subrc = 0 AND <wa_dd04t> IS ASSIGNED.
          MOVE-CORRESPONDING <wa_dd04t> TO ls_fcat.
        ENDIF.
      ENDIF.

      ASSIGN it_column[ columnname = ls_fcat-fieldname ] TO FIELD-SYMBOL(<column>).
      IF sy-subrc = 0.
        IF <column>-hotspot = abap_true.
          ls_fcat-hotspot = abap_true.
        ENDIF.
      ENDIF.
      APPEND ls_fcat TO rt_fcat.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout.
    CLEAR ms_layout.

    ms_layout = VALUE lvc_s_layo( zebra      = abap_true
*                                  edit       =
*                                  edit_mode  =
*                                  no_keyfix  =
*                                  frontend   =
*                                  object_key =
*                                  doc_id     =
*                                  template   =
*                                  language   =
*                                  graphics   =
*                                  smalltitle =
*                                  no_hgridln =
*                                  no_vgridln =
*                                  no_headers =
*                                  no_merging =
                                  cwidth_opt = abap_true
*                                  totals_bef =
*                                  no_totarr  =
*                                  no_totexp  =
*                                  no_rowmove =
*                                  no_rowins  =
*                                  no_colexpd =
*                                  no_f4      =
*                                  countfname =
*                                  col_opt    =
*                                  val_data   =
*                                  blob_scope =
*                                  blob_flavour =
*                                  blob_name  =
*                                  blob_key   =
*                                  blob_type  =
*                                  stylefname =
*                                  no_rowmark =
*                                  no_toolbar =
*                                  grid_title =
                                  sel_mode   = iv_sel_mode " A/B/C/D
*                                  box_fname  =
*                                  sgl_clk_hd =
*                                  no_totline =
*                                  numc_total =
*                                  no_utsplit =
*                                  excp_fname =
*                                  excp_rolln =
*                                  excp_conds =
*                                  excp_led   =
*                                  excp_group =
*                                  detailinit =
*                                  detailtitl =
*                                  keyhot     =
*                                  no_author  =
*                                  xifunckey  =
*                                  xidirect   =
*                                  s_dragdrop =
*                                  info_fname =
*                                  ctab_fname =
*                                  weblook    =
*                                  webstyle   =
*                                  webrows    =
*                                  webxwidth  =
*                                  webxheight =
    ).
  ENDMETHOD.

  METHOD set_variant.
    ms_variant-report = VALUE disvariant( report   = iv_repid
                                          handle   = '1'
*                                          log_group =
                                          username = sy-uname
                                          variant  = iv_variant
*                                          text     =
*                                          dependvars =
).
  ENDMETHOD.

  METHOD handle_double_click.
  ENDMETHOD.

  METHOD handle_hotspot_click.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN OTHERS.
        SET SCREEN 0.
        LEAVE SCREEN.
        free( ).
    ENDCASE.
  ENDMETHOD.

  METHOD set_table_for_first_display.
    " TODO: parameter IV_DEFAULT is never used (ABAP cleaner)

    set_layout( iv_sel_mode = 'D' ).

    set_variant( iv_variant = iv_variant                 " Layout
                 iv_repid   = iv_repid ).

    SET HANDLER handle_double_click  FOR mo_grid.
    SET HANDLER handle_hotspot_click FOR mo_grid.
    SET HANDLER handle_user_command  FOR mo_grid.

    mo_grid->set_table_for_first_display( EXPORTING  i_buffer_active               = iv_buffer_active                 " Pufferung aktiv
                                                     i_bypassing_buffer            = iv_bypassing_buffer                 " Puffer ausschalten
                                                     i_consistency_check           = iv_consistency_check                 " Starte Konsistenzverprobung für Schnittstellefehlererkennung
                                                     i_structure_name              = iv_structure_name                 " Strukturname der internen Ausgabetabelle
                                                     is_variant                    = ms_variant                 " Anzeigevariante
                                                     i_save                        = iv_save                 " Anzeigevariante sichern
                                                     i_default                     = 'X'              " Defaultanzeigevariante
                                                     is_layout                     = ms_layout                 " Layout
                                                     is_print                      = is_print                 " Drucksteuerung
                                                     it_special_groups             = it_special_groups[]          " Feldgruppen
                                                     it_toolbar_excluding          = it_toolbar_excluding[]                 " excludierte Toolbarstandardfunktionen
                                                     it_hyperlink                  = it_hyperlink[]                 " Hyperlinks
                                                     it_alv_graphics               = it_alv_graphics[]                 " Tabelle von der Struktur DTC_S_TC
                                                     it_except_qinfo               = it_except_qinfo[]                 " Tabelle für die Exception Quickinfo
                                                     ir_salv_adapter               = ir_salv_adapter                 " Interface ALV Adapter
                                          CHANGING   it_outtab                     = it_outtab[]                 " Ausgabetabelle
                                                     it_fieldcatalog               = it_fieldcatalog[]                 " Feldkatalog
                                                     it_sort                       = it_sort[]                 " Sortierkriterien
                                                     it_filter                     = it_filter[]                 " Filterkriterien
                                          EXCEPTIONS invalid_parameter_combination = 1                " Parameter falsch
                                                     program_error                 = 2                " Programmfehler
                                                     too_many_lines                = 3                " Zu viele Zeilen in eingabebereitem Grid.
                                                     OTHERS                        = 4 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_grid_alv( textid = zcx_grid_alv=>error_grid ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
