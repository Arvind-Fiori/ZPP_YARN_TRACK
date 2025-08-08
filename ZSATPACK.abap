*&----------------------------------------------------------------------------------------------*
*&  Program Name        :  ZPP_YARN_TRACK                                                      *
*&  Purpose             :  Yarn To Fiber Treacability Report...              *
*&  Author              :  Sushen Gore     - VCERP                                              *
*&  Business Analyst    :  Bhautik Umretiya
*&  TRN                 :  ATDK9A0JSM
*&  Start Date          :  11/05/2025                                                           *
*&  Category            :  PP                                                                   *
*&  Changed By          :  N.A                                                                  *
*&  Change Code         :  N.A                                                                  *
*&  Details             :  Yarn To Fiber Treacability Report...
*&  Devid               *  6710
*&-------------------------------------------------------------------------------------
REPORT ZSATPACK.

TABLES marc.

TYPES : BEGIN OF ty_final.

    INCLUDE STRUCTURE zpp_yarn_track.

TYPES : maktx    TYPE makt-maktx,
        vbeln_im TYPE mseg-vbeln_im,
        vbelp_im TYPE mseg-vbelp_im,
        matnr_50 TYPE c LENGTH 50,
        sel      TYPE c LENGTH 1,
        icon     TYPE c LENGTH 4,
        msg      TYPE c LENGTH 120,
        table    TYPE flag.
TYPES :     END OF ty_final.

TYPES: BEGIN OF ty_makt,
         mandt TYPE makt-mandt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
         maktg TYPE makt-maktg,
       END OF ty_makt.

TYPES: BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
       END OF ty_marc.

TYPES: BEGIN OF ty_ausp,
         objek TYPE ausp-objek,
         atinn TYPE ausp-atinn,
         atzhl TYPE ausp-atzhl,
         mafid TYPE ausp-mafid,
         klart TYPE ausp-klart,
         adzhl TYPE ausp-adzhl,
         atwrt TYPE ausp-atwrt,
         atflv TYPE ausp-atflv,
       END OF ty_ausp.

TYPES: BEGIN OF ty_mseg,
         mblnr      TYPE mseg-mblnr,
         mjahr      TYPE mseg-mjahr,
         zeile      TYPE mseg-zeile,
         matnr      TYPE mseg-matnr,
         werks      TYPE mseg-werks,
         charg      TYPE mseg-charg,
         menge      TYPE mseg-menge,
         bwtar      TYPE mseg-bwtar,
         dmbtr      TYPE mseg-dmbtr,
         sjahr      TYPE mseg-sjahr,
         smbln      TYPE mseg-smbln,
         smblp      TYPE mseg-smblp,
         aufnr      TYPE mseg-aufnr,
         ebeln      TYPE mseg-ebeln,
         ebelp      TYPE mseg-ebelp,
         lifnr      TYPE mseg-lifnr,
         vbeln_im   TYPE mseg-vbeln_im,
         vbelp_im   TYPE mseg-vbelp_im,
         budat_mkpf TYPE mseg-budat_mkpf,
       END OF ty_mseg.


TYPES: BEGIN OF ty_mseg_clb,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         menge TYPE mseg-menge,
         bwtar TYPE mseg-bwtar,
       END OF ty_mseg_clb.

TYPES: BEGIN OF ty_mcha,
         werks TYPE mcha-werks,
         charg TYPE mcha-charg,
         matnr TYPE mcha-matnr,
       END OF ty_mcha.



TYPES: BEGIN OF ty_aufm,
         mblnr TYPE aufm-mblnr,
         mjahr TYPE aufm-mjahr,
         zeile TYPE aufm-zeile,
         budat TYPE aufm-budat,
         bwart TYPE aufm-bwart,
         matnr TYPE aufm-matnr,
         werks TYPE aufm-werks,
         lgort TYPE aufm-lgort,
         charg TYPE aufm-charg,
         menge TYPE aufm-menge,
         aufnr TYPE aufm-aufnr,
         done  TYPE flag,
       END OF ty_aufm.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         matkl TYPE mara-matkl,
       END OF ty_mara.

TYPES : tt_final TYPE ty_final.

DATA: lt_celltab TYPE lvc_t_styl,
      ls_celltab TYPE lvc_s_styl.

DATA : gt_makt     TYPE STANDARD TABLE OF ty_makt,
       gs_makt     TYPE ty_makt,
       gt_mara     TYPE STANDARD TABLE OF ty_mara,
       gs_mara     TYPE ty_mara,
       gt_ausp     TYPE STANDARD TABLE OF ty_ausp,
       gs_ausp     TYPE ty_ausp,
       gt_aufm     TYPE STANDARD TABLE OF ty_aufm,
       gs_aufm     TYPE  ty_aufm,
       gt_aufm_ex  TYPE STANDARD TABLE OF ty_aufm,
       gs_aufm_ex  TYPE  ty_aufm,
       gt_aufm_clb TYPE STANDARD TABLE OF ty_aufm,
       gs_aufm_clb TYPE  ty_aufm,
       gt_mseg_clb TYPE STANDARD TABLE OF ty_mseg_clb,
       gs_mseg_clb TYPE ty_mseg_clb,
       gt_mseg     TYPE STANDARD TABLE OF ty_mseg,
       gs_mseg     TYPE ty_mseg,
       gt_mseg_rev TYPE STANDARD TABLE OF ty_mseg,
       gs_mseg_rev TYPE ty_mseg,
       gt_marc     TYPE STANDARD TABLE OF ty_marc,
       gs_marc     TYPE ty_marc,
       gt_final_01 TYPE STANDARD TABLE OF ty_final,
       gs_final_01 TYPE ty_final,
       gt_final    TYPE STANDARD TABLE OF ty_final,
       gs_final    TYPE ty_final,
       gs_final_lc TYPE ty_final,
       gt_final_lc TYPE STANDARD TABLE OF ty_final.

CONSTANTS: gc_msgid TYPE symsgid VALUE 'ZPP_YARN_TRACK',
           gc_msg_material_not_extended TYPE symsgno VALUE '001',
           gc_msg_yarn_not_found TYPE symsgno VALUE '002'.

TYPES : BEGIN OF ty_lead,
          werks         TYPE marc-werks,
          single_ymatnr TYPE marc-matnr,
          single_ymrn   TYPE sy-datum,
          last_7        TYPE sy-datum,
          last_15       TYPE sy-datum,
        END OF ty_lead.

DATA : gt_lead TYPE STANDARD TABLE OF ty_lead,
       gs_lead TYPE ty_lead.

TYPES : BEGIN OF ty_club,
          aufnr              TYPE aufm-aufnr,
          matnr              TYPE aufm-matnr,
          werks              TYPE aufm-werks,
          charg              TYPE aufm-charg,
          menge              TYPE aufm-menge,

          lead_single_ymatnr TYPE aufm-matnr,
          lead_done          TYPE flag,

        END OF ty_club.
DATA : gt_clb TYPE STANDARD TABLE OF ty_club,
       gs_clb TYPE ty_club.

DATA : gt_clb_lead TYPE STANDARD TABLE OF ty_club,
       gs_clb_lead TYPE ty_club.


TYPES: BEGIN OF ty_qamb,
         prueflos TYPE qamb-prueflos,
         zaehler  TYPE qamb-zaehler,
         typ      TYPE qamb-typ,
         mblnr    TYPE qamb-mblnr,
         mjahr    TYPE qamb-mjahr,
         zeile    TYPE qamb-zeile,
       END OF ty_qamb.

TYPES : BEGIN OF ys_qamv,
          prueflos   TYPE qamv-prueflos,
          vorglfnr   TYPE qamv-vorglfnr,
          merknr     TYPE qamv-merknr,
          verwmerkm  TYPE qamv-verwmerkm,
          qmtb_werks TYPE qamv-qmtb_werks,
          pmethode   TYPE qamv-pmethode,
          pmtversion TYPE qamv-pmtversion,
          kurztext   TYPE qamv-kurztext,
          masseinhsw TYPE qamv-masseinhsw,
          toleranzob TYPE qamv-toleranzob,
          toleranzun TYPE qamv-toleranzun,
          sollwert   TYPE qamv-sollwert,
        END OF ys_qamv,
        BEGIN OF ys_qamr,
          prueflos   TYPE qamr-prueflos,
          vorglfnr   TYPE qamr-vorglfnr,
          merknr     TYPE qamr-merknr,
          attribut   TYPE qattribut,
          mbewertg   TYPE qamr-mbewertg,
          pruefbemkt TYPE qamr-pruefbemkt,
          maschine   TYPE qamr-maschine,
          mittelwert TYPE qamr-mittelwert,
          katalgart1 TYPE qamr-katalgart1,
          gruppe1    TYPE qamr-gruppe1,
          code1      TYPE qamr-code1,
        END OF ys_qamr.

TYPES: BEGIN OF ty_qals,

         prueflos   TYPE qals-prueflos,
         werk       TYPE qals-werk,          "Plant
         selmatnr   TYPE qals-selmatnr,      "Material

         vorglfnr   TYPE qamr-vorglfnr,
         merknr     TYPE qamr-merknr,
         mittelwert TYPE qamr-mittelwert,

         verwmerkm  TYPE qamv-verwmerkm,     "Char No
         kurztext   TYPE qamv-kurztext,      "Char Desc

       END OF ty_qals.

TYPES : BEGIN OF ty_done_lead,
          werks         TYPE marc-werks,
          single_ymatnr TYPE marc-matnr,
          found         TYPE flag,
        END OF ty_done_lead.

DATA : gt_done_lead TYPE STANDARD TABLE OF ty_done_lead,
       gs_done_lead TYPE ty_done_lead.

DATA : gt_qals TYPE STANDARD TABLE OF ty_qals,
       gs_qals TYPE ty_qals.

DATA : gt_qals_lc TYPE STANDARD TABLE OF ty_qals,
       gs_qals_lc TYPE ty_qals.

DATA :gt_qamr TYPE TABLE OF ys_qamr,
      gs_qamr TYPE          ys_qamr,
      gt_qamv TYPE TABLE OF ys_qamv,
      gs_qamv TYPE          ys_qamv,
      gt_qamb TYPE STANDARD TABLE OF ty_qamb,
      gs_qamb TYPE ty_qamb.

DATA : lv_awart  TYPE c LENGTH 35,
       lv_atinn  TYPE ausp-atinn,
       lv_atflv  TYPE ausp-atflv,
       lv_cvalue TYPE c LENGTH 16.

DATA : s_budat  TYPE RANGE OF mkpf-budat,
       ls_budat LIKE LINE OF s_budat.

DATA: l_sel_dir TYPE string.
DATA : l_intern TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.
DATA : l_index TYPE i.
DATA : l_start_col TYPE i VALUE '1',
       l_start_row TYPE i VALUE '2',
       l_end_col   TYPE i VALUE '256',
       l_end_row   TYPE i VALUE '65536'.

DATA : gt_fieldcat TYPE lvc_t_fcat,
       gs_fieldcat TYPE lvc_s_fcat,
       ls_layout   TYPE lvc_s_layo.

FIELD-SYMBOLS: <outtab>    TYPE STANDARD TABLE,
               <l_line>    TYPE any,
               <l_line_lc> TYPE any,
               <ffield>    TYPE any.

DATA : gv_index  TYPE sy-tabix,
       new_table TYPE REF TO data,
       new_line  TYPE REF TO data.

FIELD-SYMBOLS: <outtab1>    TYPE STANDARD TABLE,
               <l_line1>    TYPE any,
               <l_line_lc1> TYPE any,
               <ffield1>    TYPE any,
               <cot_mic>    TYPE any.

DATA : gv_index1  TYPE sy-tabix,
       new_table1 TYPE REF TO data,
       new_line1  TYPE REF TO data.

DATA : gt_lead_time TYPE STANDARD TABLE OF zpp_ctn_lead_tim,
       gs_lead_time TYPE zpp_ctn_lead_tim.

DATA : p_exc TYPE c LENGTH 1,
       p_csv TYPE c LENGTH 1.
PARAMETERS:p_file TYPE rlgrap-filename MODIF ID m2.

DATA : gt_zpp_yarn_track TYPE STANDARD TABLE OF zpp_yarn_track,
       gs_zpp_yarn_track TYPE zpp_yarn_track.

DATA : gt_zpp_yarn_track_db TYPE STANDARD TABLE OF zpp_yarn_track,
       gs_zpp_yarn_track_db TYPE zpp_yarn_track.

SELECT-OPTIONS : s_werks FOR marc-werks MODIF ID m1 ,
                 s_yarn FOR marc-matnr MODIF ID m1.
PARAMETERS     : test_dt TYPE sy-datum MODIF ID m1.

SELECTION-SCREEN : ULINE.
PARAMETERS : mau RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND u2,
             yrn RADIOBUTTON GROUP g2.
SELECTION-SCREEN : ULINE.
PARAMETERS : dtl RADIOBUTTON GROUP g1 DEFAULT 'X',
             sum RADIOBUTTON GROUP g1.
SELECTION-SCREEN : ULINE.
PARAMETERS : dup TYPE flag DEFAULT 'X' NO-DISPLAY.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF mau = 'X' .
      IF screen-group1 = 'M1'.
        screen-active = 0.
      ENDIF.
    ENDIF.

    IF yrn = 'X'.
      IF screen-group1 = 'M2'.
        screen-active = 0.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.

START-OF-SELECTION.
  IF mau = 'X'.
    PERFORM upload_file.
  ELSEIF yrn = 'X'.
    PERFORM input.
  ENDIF.
  PERFORM get_data.

END-OF-SELECTION.
  PERFORM process.
  PERFORM cott_fiber1_mixing2.  "Level-1
  PERFORM cott_fiber2_mixing3.  "Level-2
  PERFORM cott_fiber3_mixing4.  "Level-3
  PERFORM cott_fiber4.          "Level-4

  PERFORM field_cat.

  IF sum = 'X'.
    PERFORM summary.
  ENDIF.

  PERFORM show_data.

FORM get_data.

  IF gt_final IS NOT INITIAL.

SELECT *
      FROM zpp_yarn_track
      INTO TABLE gt_zpp_yarn_track
      FOR ALL ENTRIES IN gt_final
      WHERE testing_dt = gt_final-testing_dt
        AND werks      = gt_final-werks
        AND matnr      = gt_final-matnr
        AND lot_no     = gt_final-lot_no.

    SORT gt_zpp_yarn_track ASCENDING BY  testing_dt werks matnr lot_no.


    LOOP AT gt_final INTO gs_final.
      READ TABLE gt_zpp_yarn_track INTO gs_zpp_yarn_track WITH KEY testing_dt =   gs_final-testing_dt
                                                                   werks      =   gs_final-werks
                                                                   matnr      =   gs_final-matnr
                                                                   lot_no     =   gs_final-lot_no BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE TABLE gt_final FROM gs_final.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF gt_final IS NOT INITIAL.

    lv_awart = 'Y_NPLY'.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = lv_awart
      IMPORTING
        output = lv_atinn.

    SELECT objek
           atinn
           atzhl
           mafid
           klart
           adzhl
           atwrt
           atflv
           FROM ausp
           INTO TABLE gt_ausp
           FOR ALL ENTRIES IN gt_final
           WHERE objek = gt_final-matnr_50
           AND   atinn = lv_atinn.

    SELECT mandt
           matnr
           spras
           maktx
           maktg
           FROM makt
           INTO TABLE gt_makt
           FOR ALL ENTRIES IN gt_final
           WHERE matnr = gt_final-matnr
           AND   spras = 'EN'.

    SELECT matnr
           werks
           FROM marc
           INTO TABLE gt_marc
           FOR ALL ENTRIES IN gt_final
           WHERE matnr = gt_final-matnr
           AND   werks = gt_final-werks.

    SELECT mblnr
           mjahr
           zeile
           matnr
           werks
           charg
           menge
           bwtar
           dmbtr
           sjahr
           smbln
           smblp
           aufnr
           ebeln
           ebelp
           lifnr
           vbeln_im
           vbelp_im
           budat_mkpf
           FROM mseg
           INTO TABLE gt_mseg
           FOR ALL ENTRIES IN gt_final
           WHERE matnr = gt_final-matnr
           AND   werks = gt_final-werks
           AND   bwart IN ( '101' ).
    IF sy-subrc = 0.

      SELECT mblnr
             mjahr
             zeile
             matnr
             werks
             charg
             menge
             bwtar
             dmbtr
             sjahr
             smbln
             smblp
             FROM mseg
             INTO TABLE gt_mseg_rev
             FOR ALL ENTRIES IN gt_mseg
             WHERE sjahr = gt_mseg-mjahr
             AND   smbln = gt_mseg-mblnr
             AND   smblp = gt_mseg-zeile.
      IF sy-subrc = 0.
        SORT gt_mseg      ASCENDING BY mjahr mblnr zeile.
        SORT gt_mseg_rev  ASCENDING BY sjahr smbln smblp.
        LOOP AT gt_mseg INTO gs_mseg.
          READ TABLE gt_mseg_rev INTO gs_mseg_rev WITH KEY sjahr = gs_mseg-mjahr
                                                           smbln = gs_mseg-mblnr
                                                           smblp = gs_mseg-zeile BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE TABLE gt_mseg FROM gs_mseg.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.


FORM process.

  DATA : lv_index       TYPE sy-tabix.
  DATA : lv_index_plus  TYPE sy-tabix.
  DATA : lv_qty         TYPE mseg-menge.
  DATA : lv_po_indi     TYPE flag.
  DATA : lv_latest_dt  TYPE sy-datum,
         lv_diff1      TYPE i,
         lv_diff2      TYPE i,
         lv_diff_flag1 TYPE c,
         lv_diff_flag2 TYPE c,
         lv_mqty       TYPE mseg-menge,
         lv_dycondi    TYPE string,
         lv_cmatnr     TYPE c LENGTH 20,
         lv_plant      TYPE c LENGTH 15,
         lv_cdate      TYPE c LENGTH 15.
  DATA: lv_werks       TYPE c LENGTH 4,
        lv_charg       TYPE c LENGTH 10,
        lv_objkey      TYPE string,
        lv_space       TYPE string,
        gv_objectkey   TYPE  bapi1003_key-object,
        gv_objecttable TYPE  bapi1003_key-objecttable,
        gv_classnum    TYPE  bapi1003_key-classnum,
        gv_classtype   TYPE  bapi1003_key-classtype.

  DATA: gt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num WITH HEADER LINE,
        gw_allocvaluesnum  TYPE  bapi1003_alloc_values_num,
        gt_allocvalueschar TYPE STANDARD TABLE OF  bapi1003_alloc_values_char WITH HEADER LINE,
        gw_allocvalueschar TYPE bapi1003_alloc_values_char,
        gt_allocvaluescurr TYPE STANDARD TABLE OF  bapi1003_alloc_values_curr WITH HEADER LINE,
        gw_allocvaluescurr TYPE  bapi1003_alloc_values_curr,
      lt_return          TYPE STANDARD TABLE OF bapiret2,
        lt_mcha            TYPE STANDARD TABLE OF ty_mcha,
        ls_mcha            TYPE ty_mcha,
        lt_makt_pref       TYPE STANDARD TABLE OF ty_makt,
        ls_makt_pref       TYPE ty_makt.

  IF gt_mseg IS NOT INITIAL.
    SELECT werks charg matnr
      FROM mcha
      INTO TABLE lt_mcha
      FOR ALL ENTRIES IN gt_mseg
      WHERE werks = gt_mseg-werks
        AND charg = gt_mseg-charg.
    SORT lt_mcha BY werks charg.
    DELETE ADJACENT DUPLICATES FROM lt_mcha COMPARING werks charg.

    SELECT mandt matnr spras maktx maktg
      FROM makt
      INTO TABLE lt_makt_pref
      FOR ALL ENTRIES IN lt_mcha
      WHERE matnr = lt_mcha-matnr
        AND spras = 'EN'.
    SORT lt_makt_pref BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_makt_pref COMPARING matnr.
  ENDIF.

  SORT gt_marc  ASCENDING BY matnr werks.
  SORT gt_makt  ASCENDING BY matnr.
  SORT gt_ausp  ASCENDING BY objek.
  DATA(gt_mseg01) = gt_mseg.
  DATA(gt_mseg02) = gt_mseg.

  SORT gt_mseg01  DESCENDING BY werks matnr budat_mkpf.
  SORT gt_mseg02  ASCENDING  BY werks matnr budat_mkpf.


  gt_final_01 = gt_final.
  CLEAR : gt_final[].

  LOOP AT gt_final_01 INTO gs_final.

    READ TABLE gt_marc INTO gs_marc WITH KEY matnr = gs_final-matnr
                                             werks = gs_final-werks BINARY SEARCH.
    IF sy-subrc NE 0.
      gs_final-icon = '@0A@'.
      MESSAGE ID gc_msgid TYPE 'S' NUMBER gc_msg_material_not_extended INTO gs_final-msg.
    ENDIF.

    READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_final-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      gs_final-maktx = gs_makt-maktx.
    ENDIF.

    IF gs_final-icon IS INITIAL.

      READ TABLE gt_ausp INTO gs_ausp WITH KEY objek = gs_final-matnr BINARY SEARCH.
      IF sy-subrc = 0.

        lv_atflv = gs_ausp-atflv.
        CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
          EXPORTING
            i_number_of_digits       = 0
            i_fltp_value             = lv_atflv
            i_value_not_initial_flag = 'X'
            i_screen_fieldlength     = 16
          IMPORTING
            e_char_field             = lv_cvalue.

        CONDENSE lv_cvalue NO-GAPS.

*_____________________________________________________________________________________________
*_Find the Closet Date
        CLEAR :  lv_diff1,lv_diff2,lv_latest_dt,lv_diff_flag1,lv_diff_flag2.

        LOOP AT gt_mseg01 INTO DATA(gs_mseg01) WHERE werks EQ gs_final-werks
                                               AND   matnr EQ gs_final-matnr
                                               AND   budat_mkpf LE gs_final-testing_dt.
          lv_diff_flag1 = 'X'.
          lv_diff1 = gs_final-testing_dt - gs_mseg01-budat_mkpf.EXIT.
        ENDLOOP.

        LOOP AT gt_mseg02 INTO DATA(gs_mseg02) WHERE werks EQ gs_final-werks
                                               AND   matnr EQ gs_final-matnr
                                               AND   budat_mkpf GT gs_final-testing_dt.
          lv_diff_flag2 = 'X'.
          lv_diff2 = gs_final-testing_dt - gs_mseg02-budat_mkpf.EXIT.
        ENDLOOP.

        IF lv_diff1 LT 0.
          lv_diff1 = lv_diff1 * -1.
        ENDIF.

        IF lv_diff2 LT 0.
          lv_diff2 = lv_diff2 * -1.
        ENDIF.

        IF lv_diff_flag1 = 'X' AND lv_diff1 = 0.
          lv_latest_dt = gs_mseg01-budat_mkpf.
        ELSEIF lv_diff_flag2 = 'X' AND lv_diff2 = 0.
          lv_latest_dt = gs_mseg02-budat_mkpf.
        ELSEIF  lv_diff_flag1 = '' AND lv_diff1 = 0.
          lv_latest_dt = gs_mseg02-budat_mkpf.
        ELSEIF  lv_diff_flag2 = '' AND lv_diff2 = 0.
          lv_latest_dt = gs_mseg01-budat_mkpf.
        ENDIF.

        IF lv_diff1 IS NOT INITIAL AND lv_diff2 IS NOT INITIAL AND lv_latest_dt IS INITIAL.
          IF lv_diff1 LE lv_diff2.
            lv_latest_dt = gs_mseg01-budat_mkpf.
          ELSEIF lv_diff1 GT lv_diff2.
            lv_latest_dt = gs_mseg02-budat_mkpf.
          ENDIF.
        ENDIF.

*_____________________________________________________________________________________________

*______
        IF lv_cvalue = '1'. "Single Yarn


          LOOP AT gt_mseg INTO gs_mseg WHERE matnr      = gs_final-matnr
                                       AND   werks      = gs_final-werks
                                       AND   budat_mkpf = lv_latest_dt.

            gs_final_lc = gs_final.

            gs_final_lc-single_ymatnr     = gs_final-matnr.
            gs_final_lc-single_ymatnr_des = gs_final-maktx.
            gs_final_lc-single_ybatch     = gs_mseg-charg.
            gs_final_lc-single_yqty       = gs_mseg-menge.
            gs_final_lc-single_yval       = gs_mseg-bwtar.
            gs_final_lc-vbeln_im          = gs_mseg-vbeln_im.
            gs_final_lc-vbelp_im          = gs_mseg-vbelp_im.
            gs_final_lc-single_ymrn       = gs_mseg-budat_mkpf.

            IF gs_mseg-aufnr IS NOT INITIAL.
              gs_final_lc-single_yprod    = gs_mseg-aufnr.
            ELSE.

              IF gs_final_lc-vbeln_im IS INITIAL AND gs_final_lc-vbelp_im IS INITIAL.
                gs_final_lc-single_ypo     = gs_mseg-ebeln.
                gs_final_lc-single_ypoline = gs_mseg-ebelp.
                gs_final_lc-single_yvendor = gs_mseg-lifnr.
                gs_final_lc-single_yplant  = gs_mseg-werks.
              ELSE.
                PERFORM find_po USING gs_final_lc-matnr
                                      gs_final_lc-single_ybatch
                                      gs_final_lc-vbeln_im
                                      gs_final_lc-vbelp_im
                      CHANGING  gs_final_lc-single_yprod
                                gs_final_lc-single_ypo
                                gs_final_lc-single_ypoline
                                gs_final_lc-single_yvendor
                                gs_final_lc-single_yplant.

              ENDIF.
            ENDIF.

*_Begin of Mixing material 1

            CLEAR : gv_objectkey,gt_allocvaluesnum,gt_allocvalueschar,gt_allocvaluescurr,lt_return.

            IF ( gs_final_lc-single_ymatnr IS NOT INITIAL AND gs_final_lc-werks IS NOT INITIAL AND gs_final_lc-single_ybatch IS NOT INITIAL )
                AND ( gs_final_lc-mix_matnr1 IS INITIAL ).

              gv_objectkey+0(18)  =  gs_final_lc-single_ymatnr.
              gv_objectkey+18(4)  =  gs_final_lc-werks.
              gv_objectkey+22(10) =  gs_final_lc-single_ybatch.

              CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
                EXPORTING
                  objectkey       = gv_objectkey
                  objecttable     = 'MCHA'
                  classnum        = 'B_GY'
                  classtype       = '022'
                  language        = sy-langu
                TABLES
                  allocvaluesnum  = gt_allocvaluesnum
                  allocvalueschar = gt_allocvalueschar
                  allocvaluescurr = gt_allocvaluescurr
                  return          = lt_return.

              SORT gt_allocvalueschar ASCENDING BY charact.
             READ TABLE  gt_allocvalueschar INTO gw_allocvalueschar WITH KEY charact = 'B_AMXD' BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_final_lc-mix_batch1   = gw_allocvalueschar-value_char.
                IF gs_final_lc-mix_batch1 IS NOT INITIAL.
                  READ TABLE lt_mcha INTO ls_mcha WITH KEY werks = gs_final_lc-werks charg = gs_final_lc-mix_batch1 BINARY SEARCH.
                  IF sy-subrc = 0.
                    gs_final_lc-mix_matnr1 = ls_mcha-matnr.
                  ELSE.
                    SELECT SINGLE matnr FROM mcha INTO gs_final_lc-mix_matnr1
                      WHERE werks = gs_final_lc-werks
                        AND charg = gs_final_lc-mix_batch1.
                  ENDIF.
                  IF gs_final_lc-mix_matnr1 IS NOT INITIAL.
                    READ TABLE lt_makt_pref INTO ls_makt_pref WITH KEY matnr = gs_final_lc-mix_matnr1 BINARY SEARCH.
                    IF sy-subrc = 0.
                      gs_final_lc-mix_matnr1_des = ls_makt_pref-maktx.
                    ELSE.
                      SELECT SINGLE maktx FROM makt INTO gs_final_lc-mix_matnr1_des
                        WHERE matnr = gs_final_lc-mix_matnr1
                          AND spras = 'EN'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF gs_final_lc-mix_matnr1 IS NOT INITIAL AND gs_final_lc-werks IS NOT INITIAL AND gs_final_lc-mix_batch1 IS NOT INITIAL.
                PERFORM goods_mvt_101 USING  gs_final_lc-mix_matnr1
                                             gs_final_lc-werks
                                             gs_final_lc-mix_batch1
                                             CHANGING  gs_final_lc-mix_prod1
                                                       gs_final_lc-mix_po1      "Not Req.
                                                       gs_final_lc-mix_poline1  "Not Req.
                                                       gs_final_lc-mix_vendor1   "Not Req.
                                                       gs_final_lc-mix_plant1   "Not Req.
                                                       gs_final_lc-mix_val1     "Not Req.
                                                       gs_final_lc-mix_qty1
                                                       gs_final_lc-mix_mrn1.
              ENDIF.

            ENDIF.

*_End of Mixing material 1

            APPEND gs_final_lc TO gt_final.
            CLEAR : gs_final_lc.

          ENDLOOP.
          IF sy-subrc NE 0.
            APPEND gs_final TO gt_final.
            CLEAR : gs_final.
          ENDIF.

        ELSE.   "_Double Yarn_______________________


          LOOP AT gt_mseg INTO gs_mseg WHERE matnr      = gs_final-matnr
                                       AND   werks      = gs_final-werks
                                       AND   budat_mkpf = lv_latest_dt.

            gs_final_lc = gs_final.

            gs_final_lc-doub_ymatnr      = gs_final-matnr.
            gs_final_lc-doub_ymatnr_des  = gs_final-maktx.
            gs_final_lc-doub_ybatch      = gs_mseg-charg.
            gs_final_lc-doub_yqty        = gs_mseg-menge.
            gs_final_lc-doub_yval        = gs_mseg-bwtar.
            gs_final_lc-vbeln_im         = gs_mseg-vbeln_im.
            gs_final_lc-vbelp_im         = gs_mseg-vbelp_im.
            gs_final_lc-doub_ymrn        = gs_mseg-budat_mkpf.

            IF gs_mseg-aufnr IS NOT INITIAL.
              gs_final_lc-doub_yprod    = gs_mseg-aufnr.
            ELSE.
              IF gs_final_lc-vbeln_im IS INITIAL AND gs_final_lc-vbelp_im IS INITIAL.
                gs_final_lc-doub_ypo     = gs_mseg-ebeln.
                gs_final_lc-doub_ypoline = gs_mseg-ebelp.
                gs_final_lc-doub_yvendor = gs_mseg-lifnr.
                gs_final_lc-doub_yplant  = gs_mseg-werks.
              ELSE.
                PERFORM find_po USING gs_final_lc-matnr
                                      gs_final_lc-doub_ybatch
                                      gs_final_lc-vbeln_im
                                      gs_final_lc-vbelp_im
                                      CHANGING  gs_final_lc-doub_yprod
                                                gs_final_lc-doub_ypo
                                                gs_final_lc-doub_ypoline
                                                gs_final_lc-doub_yvendor
                                                gs_final_lc-doub_yplant.
              ENDIF.

            ENDIF.
*
**_Begin of Single Yarn_______________________
            IF gs_final_lc-doub_ybatch IS NOT INITIAL.

              gv_objectkey+0(18)  =  gs_final_lc-matnr.
              gv_objectkey+18(4)  =  gs_final_lc-werks.
              gv_objectkey+22(10) =  gs_final_lc-doub_ybatch.

              CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
                EXPORTING
                  objectkey       = gv_objectkey
                  objecttable     = 'MCHA'
                  classnum        = 'B_GY'
                  classtype       = '022'
                  language        = sy-langu
                TABLES
                  allocvaluesnum  = gt_allocvaluesnum
                  allocvalueschar = gt_allocvalueschar
                  allocvaluescurr = gt_allocvaluescurr
                  return          = lt_return.

              SORT gt_allocvalueschar ASCENDING BY charact.
 READ TABLE  gt_allocvalueschar INTO gw_allocvalueschar WITH KEY charact = 'B_AMXD' BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_final_lc-single_ybatch   = gw_allocvalueschar-value_char.
                IF gs_final_lc-single_ybatch IS NOT INITIAL.
                  READ TABLE lt_mcha INTO ls_mcha WITH KEY werks = gs_final_lc-werks charg = gs_final_lc-single_ybatch BINARY SEARCH.
                  IF sy-subrc = 0.
                    gs_final_lc-single_ymatnr = ls_mcha-matnr.
                  ELSE.
                    SELECT SINGLE matnr FROM mcha INTO gs_final_lc-single_ymatnr
                      WHERE werks = gs_final_lc-werks
                        AND charg = gs_final_lc-single_ybatch.
                  ENDIF.
                  IF gs_final_lc-single_ymatnr IS NOT INITIAL.
                    READ TABLE lt_makt_pref INTO ls_makt_pref WITH KEY matnr = gs_final_lc-single_ymatnr BINARY SEARCH.
                    IF sy-subrc = 0.
                      gs_final_lc-single_ymatnr_des = ls_makt_pref-maktx.
                    ELSE.
                      SELECT SINGLE maktx FROM makt INTO gs_final_lc-single_ymatnr_des
                        WHERE matnr = gs_final_lc-single_ymatnr
                          AND spras = 'EN'.
                    ENDIF.
                    SELECT SINGLE matkl FROM mara INTO @DATA(lv_matkl) WHERE matnr = @gs_final_lc-single_ymatnr.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_matkl = 'MIX'.
                gs_final_lc-mix_matnr1        = gs_final_lc-single_ymatnr.
                gs_final_lc-mix_matnr1_des    = gs_final_lc-single_ymatnr_des.
                gs_final_lc-mix_batch1        = gs_final_lc-single_ybatch.

                PERFORM goods_mvt_101 USING gs_final_lc-mix_matnr1
                                            gs_final_lc-werks
                                            gs_final_lc-mix_batch1
                                           CHANGING  gs_final_lc-mix_prod1
                                                     gs_final_lc-mix_po1           "Not Req.
                                                     gs_final_lc-mix_poline1       "Not Req.
                                                     gs_final_lc-mix_vendor1       "Not Req.
                                                     gs_final_lc-mix_plant1        "Not Req.
                                                     gs_final_lc-mix_val1          "Not Req.
                                                     gs_final_lc-mix_qty1
                                                     gs_final_lc-mix_mrn1.

                CLEAR : gs_final_lc-single_ybatch ,gs_final_lc-single_ymatnr,gs_final_lc-single_ymatnr_des.
              ENDIF.

              IF gs_final_lc-single_ymatnr IS NOT INITIAL AND gs_final_lc-werks IS NOT INITIAL AND gs_final_lc-single_ybatch IS NOT INITIAL.

                PERFORM goods_mvt_101 USING gs_final_lc-single_ymatnr
                                            gs_final_lc-werks
                                            gs_final_lc-single_ybatch
                                            CHANGING  gs_final_lc-single_yprod
                                                      gs_final_lc-single_ypo
                                                      gs_final_lc-single_ypoline
                                                      gs_final_lc-single_yvendor
                                                      gs_final_lc-single_yplant
                                                      gs_final_lc-single_yval
                                                      gs_final_lc-single_yqty
                                                      gs_final_lc-single_ymrn.
              ENDIF.
*
            ENDIF.
**_End of Single Yarn_______________________

*_Begin of Mixing material 1

            CLEAR : gv_objectkey,gt_allocvaluesnum,gt_allocvalueschar,gt_allocvaluescurr,lt_return.

            IF gs_final_lc-single_ymatnr IS NOT INITIAL AND gs_final_lc-werks IS NOT INITIAL AND gs_final_lc-single_ybatch IS NOT INITIAL
               AND gs_final_lc-mix_matnr1 IS INITIAL.

              gv_objectkey+0(18)  =  gs_final_lc-single_ymatnr.
              gv_objectkey+18(4)  =  gs_final_lc-werks.
              gv_objectkey+22(10) =  gs_final_lc-single_ybatch.

              CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
                EXPORTING
                  objectkey       = gv_objectkey
                  objecttable     = 'MCHA'
                  classnum        = 'B_GY'
                  classtype       = '022'
                  language        = sy-langu
                TABLES
                  allocvaluesnum  = gt_allocvaluesnum
                  allocvalueschar = gt_allocvalueschar
                  allocvaluescurr = gt_allocvaluescurr
                  return          = lt_return.

SORT gt_allocvalueschar ASCENDING BY charact.
              READ TABLE  gt_allocvalueschar INTO gw_allocvalueschar WITH KEY charact = 'B_AMXD' BINARY SEARCH.
              IF sy-subrc EQ 0.
                gs_final_lc-mix_batch1   = gw_allocvalueschar-value_char.
                IF gs_final_lc-mix_batch1 IS NOT INITIAL.
                  READ TABLE lt_mcha INTO ls_mcha WITH KEY werks = gs_final_lc-werks charg = gs_final_lc-mix_batch1 BINARY SEARCH.
                  IF sy-subrc = 0.
                    gs_final_lc-mix_matnr1 = ls_mcha-matnr.
                  ELSE.
                    SELECT SINGLE matnr FROM mcha INTO gs_final_lc-mix_matnr1
                      WHERE werks = gs_final_lc-werks
                        AND charg = gs_final_lc-mix_batch1.
                  ENDIF.
                  IF gs_final_lc-mix_matnr1 IS NOT INITIAL.
                    READ TABLE lt_makt_pref INTO ls_makt_pref WITH KEY matnr = gs_final_lc-mix_matnr1 BINARY SEARCH.
                    IF sy-subrc = 0.
                      gs_final_lc-mix_matnr1_des = ls_makt_pref-maktx.
                    ELSE.
                      SELECT SINGLE maktx FROM makt INTO gs_final_lc-mix_matnr1_des
                        WHERE matnr = gs_final_lc-mix_matnr1
                          AND spras = 'EN'.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF gs_final_lc-mix_matnr1 IS NOT INITIAL AND gs_final_lc-werks IS NOT INITIAL AND gs_final_lc-mix_batch1 IS NOT INITIAL.
                PERFORM goods_mvt_101 USING  gs_final_lc-mix_matnr1
                                             gs_final_lc-werks
                                             gs_final_lc-mix_batch1
                                             CHANGING  gs_final_lc-mix_prod1
                                                       gs_final_lc-mix_po1      "Not Req.
                                                       gs_final_lc-mix_poline1  "Not Req.
                                                       gs_final_lc-mix_vendor1  "Not Req.
                                                       gs_final_lc-mix_plant1   "Not Req.
                                                       gs_final_lc-mix_val1     "Not Req.
                                                       gs_final_lc-mix_qty1
                                                       gs_final_lc-mix_mrn1.
              ENDIF.

            ENDIF.

*_End of Mixing material 1

            APPEND gs_final_lc TO gt_final.
            CLEAR : gs_final_lc ,lv_matkl.

          ENDLOOP.
          IF sy-subrc NE 0.
            APPEND gs_final TO gt_final.
            CLEAR : gs_final.
          ENDIF.

        ENDIF.

*______

      ELSE.
        gs_final-icon = '@0A@'.
        MESSAGE ID gc_msgid TYPE 'S' NUMBER gc_msg_yarn_not_found INTO gs_final-msg.
        MODIFY gt_final_01 FROM gs_final TRANSPORTING icon msg.
      ENDIF.

    ENDIF.

    CLEAR : lv_index,lv_index_plus ,gs_final_lc,gs_final,lv_po_indi,lv_cvalue.
  ENDLOOP.

  IF gt_final IS NOT INITIAL.
    SELECT zplant
           zyarn_code
           zcott_code
           zlead_time
      FROM zpp_ctn_lead_tim
      INTO CORRESPONDING FIELDS OF TABLE  gt_lead_time
      FOR ALL ENTRIES IN gt_final
      WHERE zyarn_code = gt_final-single_ymatnr
        AND zplant     = gt_final-werks.
  ENDIF.


ENDFORM.


FORM cott_fiber1_mixing2.

  DATA : last_7           TYPE sy-datum,
         last_15          TYPE sy-datum,
         lv_date          TYPE sy-datum,
         exit_flag        TYPE flag,
         lv_cnt1          TYPE i,
         lv_last          TYPE flag,
         lv_cnt2          TYPE i,
         int_div          TYPE p DECIMALS 2,
         int_num          TYPE i,
         lv_batch_cnt     TYPE i,
         batch_found_date TYPE sy-datum,
         lv_lead_logic    TYPE flag,
         lt_mcha          TYPE STANDARD TABLE OF ty_mcha,
         ls_mcha          TYPE ty_mcha,
         lt_makt_pref     TYPE STANDARD TABLE OF ty_makt,
         ls_makt_pref     TYPE ty_makt.

  IF gt_mseg IS NOT INITIAL.
    SELECT werks charg matnr
      FROM mcha
      INTO TABLE lt_mcha
      FOR ALL ENTRIES IN gt_mseg
      WHERE werks = gt_mseg-werks
        AND charg = gt_mseg-charg.
    SORT lt_mcha BY werks charg.
    DELETE ADJACENT DUPLICATES FROM lt_mcha COMPARING werks charg.

    SELECT mandt matnr spras maktx maktg
      FROM makt
      INTO TABLE lt_makt_pref
      FOR ALL ENTRIES IN lt_mcha
      WHERE matnr = lt_mcha-matnr
        AND spras = 'EN'.
    SORT lt_makt_pref BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_makt_pref COMPARING matnr.
  ENDIF.

  IF gt_final IS NOT INITIAL.

    SELECT  mblnr
            mjahr
            zeile
            budat
            bwart
            matnr
            werks
            lgort
            charg
            menge
            aufnr
            FROM aufm
            INTO TABLE gt_aufm
            FOR ALL ENTRIES IN gt_final
            WHERE aufnr = gt_final-mix_prod1
            AND   bwart = '261'.
    IF sy-subrc = 0.
      SELECT mblnr,
             mjahr,
             zeile,
             sjahr,
             smbln,
             smblp
             FROM mseg
             INTO TABLE @DATA(gt_rev)
             FOR ALL ENTRIES IN @gt_aufm
             WHERE sjahr = @gt_aufm-mjahr
             AND   smbln = @gt_aufm-mblnr
             AND   smblp = @gt_aufm-zeile.
    ENDIF.

    SORT gt_rev ASCENDING BY sjahr smbln smblp.
    LOOP AT gt_aufm INTO gs_aufm.
      READ TABLE gt_rev INTO DATA(gs_rev) WITH KEY sjahr = gs_aufm-mjahr
                                                   smbln = gs_aufm-mblnr
                                                   smblp = gs_aufm-zeile BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE TABLE gt_aufm FROM gs_aufm.
      ENDIF.
    ENDLOOP.

    DATA: lt_matnr TYPE SORTED TABLE OF mara-matnr WITH UNIQUE KEY table_line.

    IF gt_aufm IS NOT INITIAL.
      LOOP AT gt_aufm INTO DATA(ls_aufm).
        INSERT ls_aufm-matnr INTO TABLE lt_matnr.
      ENDLOOP.
    ENDIF.

    IF gt_lead_time IS NOT INITIAL.
      LOOP AT gt_lead_time INTO DATA(ls_lead_time).
        INSERT ls_lead_time-zcott_code INTO TABLE lt_matnr.
      ENDLOOP.
    ENDIF.

    IF lt_matnr IS NOT INITIAL.
      SELECT matnr
             mtart
             matkl
        FROM mara
        INTO TABLE gt_mara
        FOR ALL ENTRIES IN lt_matnr
        WHERE matnr = lt_matnr-table_line.

      SELECT mandt
             matnr
             spras
             maktx
             maktg
        FROM makt
        INTO TABLE gt_makt
        FOR ALL ENTRIES IN lt_matnr
        WHERE matnr = lt_matnr-table_line
          AND spras = 'EN'.
    ENDIF.

  ENDIF.

  CLEAR : gs_final_lc , gt_final_lc[],gt_clb.

  SORT gt_mara ASCENDING BY matnr.
  SORT gt_makt ASCENDING BY matnr.
  SORT gt_lead_time  ASCENDING  BY zplant zyarn_code zcott_code.
  SORT gt_clb_lead  ASCENDING  BY werks lead_single_ymatnr.

  LOOP AT gt_final INTO gs_final.

    READ TABLE gt_lead_time INTO DATA(gs_lead_time1) WITH KEY zplant     = gs_final-werks
                                                              zyarn_code = gs_final-single_ymatnr BINARY SEARCH.
    IF sy-subrc = 0.

      CLEAR : lv_cnt1, lv_cnt1,int_num,lv_batch_cnt ,s_budat,ls_budat,lv_lead_logic,lv_last,gt_aufm_ex,int_div.


      DATA(gt_lead_time_lc) = gt_lead_time.
      DELETE gt_lead_time_lc WHERE ( zplant NE gs_final-werks OR zyarn_code NE gs_final-single_ymatnr ).

      READ TABLE gt_clb_lead INTO gs_clb_lead WITH KEY werks              = gs_final-werks
                                                       lead_single_ymatnr = gs_final-single_ymatnr BINARY SEARCH.
      IF sy-subrc EQ 0.

        gs_final-table = 'X'.

        gt_clb = gt_clb_lead[].
        DELETE gt_clb WHERE ( werks NE gs_final-werks OR lead_single_ymatnr NE gs_final-single_ymatnr ).

        IF sum = 'X'.
          DELETE gt_clb WHERE lead_done = 'X'.
        ENDIF.

      ELSE.

        last_7  = gs_final-single_ymrn.
        last_15 = gs_final-single_ymrn - gs_lead_time1-zlead_time.

        ls_budat-sign   = 'I'.
        ls_budat-option = 'BT'.
        ls_budat-low    = last_15 .
        ls_budat-high   = last_7.
        APPEND ls_budat TO s_budat.


        SELECT  mblnr
                mjahr
                zeile
                budat
                bwart
                matnr
                werks
                lgort
                charg
                menge
                aufnr
                FROM aufm
                INTO TABLE gt_aufm_ex
                FOR ALL ENTRIES IN gt_lead_time_lc
                WHERE matnr = gt_lead_time_lc-zcott_code
                AND   werks = gt_lead_time_lc-zplant
                AND   bwart = '261'
                AND   budat IN s_budat.

        IF s_budat IS NOT INITIAL AND gt_lead_time_lc[] IS NOT INITIAL.

          LOOP AT gt_aufm_ex INTO gs_aufm.
            gs_clb-werks = gs_aufm-werks.
            gs_clb-matnr = gs_aufm-matnr.
            gs_clb-menge = gs_aufm-menge.
            gs_clb-charg = gs_aufm-charg.
            gs_clb-lead_single_ymatnr = gs_final-single_ymatnr.
            COLLECT gs_clb INTO gt_clb. CLEAR:gs_clb.
            CLEAR : gs_mara, gs_makt,gs_final_lc.
          ENDLOOP.

          gt_clb_lead = gt_clb[].

        ENDIF.

      ENDIF.

      IF sum = 'X'.

        DESCRIBE TABLE gt_clb_lead LINES lv_cnt1.

        DATA(gt_final_cnt) = gt_final.
        DELETE gt_final_cnt WHERE ( werks NE gs_final-werks OR single_ymatnr NE gs_final-single_ymatnr ).
        DESCRIBE TABLE gt_final_cnt LINES lv_cnt2.

        IF lv_cnt2 GE 1.
          int_div = ( lv_cnt1 / lv_cnt2 ).
          int_num = trunc( int_div ).

          IF int_num EQ 0.
            int_num = 1.
          ENDIF.

          READ TABLE gt_final_cnt INTO DATA(gs_final_cnt) INDEX lv_cnt2.
          IF gs_final_cnt-single_ybatch = gs_final-single_ybatch.
            lv_last = 'X'.
          ENDIF.
        ENDIF.

        IF lv_last EQ 'X'.
        ELSE.
          lv_batch_cnt = int_num + 1.
          DELETE gt_clb FROM lv_batch_cnt.
        ENDIF.

        LOOP AT gt_clb INTO gs_clb.
          READ TABLE gt_clb_lead INTO gs_clb_lead WITH KEY werks = gs_clb-werks
                                                           matnr = gs_clb-matnr
                                                           charg = gs_clb-charg
                                                           lead_single_ymatnr = gs_clb-lead_single_ymatnr.
          IF sy-subrc = 0.
            gs_clb_lead-lead_done = 'X'.
            MODIFY gt_clb_lead FROM gs_clb_lead INDEX sy-tabix TRANSPORTING lead_done.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ELSE.

      last_7  = gs_final-single_ymrn - 7.
      last_15 = gs_final-single_ymrn - 15.


      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod1
                                   AND ( budat LE last_7  AND budat GE last_15 ).
        SORT gt_aufm ASCENDING BY aufnr budat.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        last_7 = last_15 + 1.
        last_15 = '20180101'.
        exit_flag = 'X'.
        SORT gt_aufm DESCENDING BY aufnr budat mblnr.
      ENDIF.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod1 AND ( budat LE last_7  AND budat GE last_15 ).

        IF ( batch_found_date NE gs_aufm-budat ) AND batch_found_date IS NOT INITIAL.
          EXIT.
        ENDIF.

        gs_clb-aufnr = gs_aufm-aufnr.
        gs_clb-werks = gs_aufm-werks.
        gs_clb-matnr = gs_aufm-matnr.
        gs_clb-menge = gs_aufm-menge.
        gs_clb-charg = gs_aufm-charg.
        COLLECT gs_clb INTO gt_clb. CLEAR:gs_clb.

        IF exit_flag = 'X'.
          batch_found_date = gs_aufm-budat.
          CLEAR : exit_flag.
        ENDIF.

        CLEAR : gs_mara, gs_makt,gs_final_lc.
      ENDLOOP.

    ENDIF.

    IF gt_clb[] IS NOT INITIAL.

      LOOP AT gt_clb INTO gs_clb.

        MOVE-CORRESPONDING gs_final TO gs_final_lc.

        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_clb-matnr BINARY SEARCH.
        IF sy-subrc = 0.

          READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_clb-matnr BINARY SEARCH.

          IF gs_mara-matkl EQ 'MIX'.

            gs_final_lc-mix_matnr2     = gs_clb-matnr.
            gs_final_lc-mix_matnr2_des = gs_makt-maktx.
            gs_final_lc-mix_batch2     = gs_clb-charg.
            gs_final_lc-mix_plant2     = gs_clb-werks.

            PERFORM goods_mvt_101 USING  gs_final_lc-mix_matnr2
                                         gs_final_lc-werks
                                         gs_final_lc-mix_batch2
                                         CHANGING  gs_final_lc-mix_prod2
                                                   gs_final_lc-mix_po2      "Not Req.
                                                   gs_final_lc-mix_poline2  "Not Req.
                                                   gs_final_lc-mix_vendor2  "Not Req.
                                                   gs_final_lc-mix_plant2   "Not Req.
                                                   gs_final_lc-mix_val2     "Not Req.
                                                   gs_final_lc-mix_qty2
                                                   gs_final_lc-mix_mrn2.

            gs_final_lc-mix_qty2       = gs_clb-menge.

          ELSE.

            gs_final_lc-cott_matnr1     = gs_clb-matnr.
            gs_final_lc-cott_matnr1_des = gs_makt-maktx.
            gs_final_lc-cott_plant1     = gs_clb-werks.
            gs_final_lc-cott_batch1     = gs_clb-charg.

            PERFORM goods_mvt_cott USING  gs_final_lc-cott_matnr1
                                          gs_final_lc-cott_plant1
                                          gs_final_lc-cott_batch1
                                   CHANGING   gs_final_lc-cott_prod1
                                              gs_final_lc-cott_po1       "Not Req.
                                              gs_final_lc-cott_poline1   "Not Req.
                                              gs_final_lc-cott_vendor1
                                              gs_final_lc-cott_vname1
                                              gs_final_lc-cott_qty1
                                              gs_final_lc-cott_mrn1
                                              gs_final_lc-cott_mrn1_no
                                              gs_final_lc-cott_mrn1_year
                                              gs_final_lc-cott_mrn1_itm.

            gs_final_lc-cott_qty1       = gs_clb-menge.

          ENDIF.

        ENDIF.

        APPEND gs_final_lc TO gt_final_lc.
        CLEAR : gs_final_lc.
      ENDLOOP.

    ELSE.
      MOVE-CORRESPONDING gs_final TO gs_final_lc.
      APPEND gs_final_lc TO gt_final_lc.
      CLEAR : gs_final_lc.
    ENDIF.

    CLEAR : last_7,last_15,gs_final_lc,exit_flag,batch_found_date,gt_clb.
  ENDLOOP.


  IF gt_final_lc[] IS NOT INITIAL.
    gt_final[]  = gt_final_lc[].
  ENDIF.

ENDFORM.

FORM cott_fiber2_mixing3.

  CLEAR : gt_aufm,gt_mara,gt_makt ,gs_final_lc , gt_final_lc[],gs_final,gt_aufm_clb,gt_clb.

  IF gt_final IS NOT INITIAL.

    SELECT  mblnr
            mjahr
            zeile
            budat
            bwart
            matnr
            werks
            lgort
            charg
            menge
            aufnr
            FROM aufm
            INTO TABLE gt_aufm
            FOR ALL ENTRIES IN gt_final
            WHERE aufnr = gt_final-mix_prod2
            AND   bwart = '261'.

    IF sy-subrc = 0.
      SELECT mblnr,
             mjahr,
             zeile,
             sjahr,
             smbln,
             smblp
             FROM mseg
             INTO TABLE @DATA(gt_rev)
             FOR ALL ENTRIES IN @gt_aufm
             WHERE sjahr = @gt_aufm-mjahr
             AND   smbln = @gt_aufm-mblnr
             AND   smblp = @gt_aufm-zeile.
    ENDIF.

    SORT gt_rev ASCENDING BY sjahr smbln smblp.
    LOOP AT gt_aufm INTO gs_aufm.
      READ TABLE gt_rev INTO DATA(gs_rev) WITH KEY sjahr = gs_aufm-mjahr
                                                   smbln = gs_aufm-mblnr
                                                   smblp = gs_aufm-zeile BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE TABLE gt_aufm FROM gs_aufm.
      ENDIF.
    ENDLOOP.

    IF gt_aufm IS NOT INITIAL.

      SELECT matnr
             mtart
             matkl
             FROM mara
             APPENDING TABLE gt_mara
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr.

      SELECT mandt
             matnr
             spras
             maktx
             maktg
             FROM makt
             APPENDING TABLE gt_makt
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr
             AND   spras = 'EN'.

    ENDIF.

  ENDIF.


  DATA : last_7           TYPE sy-datum,
         last_15          TYPE sy-datum,
         lv_date          TYPE sy-datum,
         exit_flag        TYPE flag,
         batch_found_date TYPE sy-datum.


  SORT gt_mara ASCENDING BY matnr.
  SORT gt_makt ASCENDING BY matnr.
  SORT gt_lead_time  ASCENDING  BY zplant zyarn_code zcott_code.


  CLEAR : gs_final,s_budat,ls_budat,gt_aufm_ex.
  LOOP AT gt_final INTO gs_final.

    READ TABLE gt_lead_time INTO DATA(gs_lead_time1) WITH KEY zplant     = gs_final-werks
                                                              zyarn_code = gs_final-single_ymatnr BINARY SEARCH.
    IF sy-subrc = 0.

    ELSE.
      last_7  = gs_final-single_ymrn - 7.
      last_15 = gs_final-single_ymrn - 15.


      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod2
                                   AND ( budat LE last_7  AND budat GE last_15 ).
        SORT gt_aufm ASCENDING BY aufnr budat.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        last_7 = last_15 + 1.
        last_15 = '20180101'.
        exit_flag = 'X'.
        SORT gt_aufm DESCENDING BY aufnr budat mblnr.
      ENDIF.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod2 AND ( budat LE last_7  AND budat GE last_15 ).

        IF ( batch_found_date NE gs_aufm-budat ) AND batch_found_date IS NOT INITIAL.
          EXIT.
        ENDIF.

        gs_clb-aufnr = gs_aufm-aufnr.
        gs_clb-werks = gs_aufm-werks.
        gs_clb-matnr = gs_aufm-matnr.
        gs_clb-menge = gs_aufm-menge.
        gs_clb-charg = gs_aufm-charg.
        COLLECT gs_clb INTO gt_clb. CLEAR:gs_clb.

        IF exit_flag = 'X'.
          batch_found_date = gs_aufm-budat.
          CLEAR : exit_flag.
        ENDIF.

        CLEAR : gs_mara, gs_makt,gs_final_lc.
      ENDLOOP.

    ENDIF.


    IF gt_clb IS NOT INITIAL.

      LOOP AT gt_clb INTO gs_clb.

        MOVE-CORRESPONDING gs_final TO gs_final_lc.

        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_clb-matnr BINARY SEARCH.
        IF sy-subrc = 0.

          READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_clb-matnr BINARY SEARCH.

          IF gs_mara-matkl EQ 'MIX'.

            gs_final_lc-mix_matnr3     = gs_clb-matnr.
            gs_final_lc-mix_matnr3_des = gs_makt-maktx.
            gs_final_lc-mix_plant3     = gs_clb-werks.
            gs_final_lc-mix_batch3     = gs_clb-charg.

            PERFORM goods_mvt_101 USING  gs_final_lc-mix_matnr3
                                         gs_final_lc-werks
                                         gs_final_lc-mix_batch3
                                         CHANGING  gs_final_lc-mix_prod3
                                                   gs_final_lc-mix_po3      "Not Req.
                                                   gs_final_lc-mix_poline3  "Not Req.
                                                   gs_final_lc-mix_vendor3   "Not Req.
                                                   gs_final_lc-mix_plant3   "Not Req.
                                                   gs_final_lc-mix_val3     "Not Req.
                                                   gs_final_lc-mix_qty3
                                                   gs_final_lc-mix_mrn3.

            gs_final_lc-mix_qty3     = gs_clb-menge.

          ELSE.

            gs_final_lc-cott_matnr2     = gs_clb-matnr.
            gs_final_lc-cott_matnr2_des = gs_makt-maktx.
            gs_final_lc-cott_plant2     = gs_clb-werks.
            gs_final_lc-cott_batch2     = gs_clb-charg.

            PERFORM goods_mvt_cott USING  gs_final_lc-cott_matnr2
                                          gs_final_lc-cott_plant2
                                          gs_final_lc-cott_batch2
                                   CHANGING   gs_final_lc-cott_prod2
                                              gs_final_lc-cott_po2       "Not Req.
                                              gs_final_lc-cott_poline2   "Not Req.
                                              gs_final_lc-cott_vendor2
                                              gs_final_lc-cott_vname2
                                              gs_final_lc-cott_qty2
                                              gs_final_lc-cott_mrn2
                                              gs_final_lc-cott_mrn2_no
                                              gs_final_lc-cott_mrn2_year
                                              gs_final_lc-cott_mrn2_itm.

            gs_final_lc-cott_qty2     = gs_clb-menge.

          ENDIF.

        ENDIF.

        APPEND gs_final_lc TO gt_final_lc.
        CLEAR : gs_final_lc.
      ENDLOOP.
    ELSE.
      MOVE-CORRESPONDING gs_final TO gs_final_lc.
      APPEND gs_final_lc TO gt_final_lc.
      CLEAR : gs_final_lc.
    ENDIF.


    CLEAR : last_7,last_15,gs_final_lc,exit_flag,batch_found_date,gt_clb.
  ENDLOOP.

  IF gt_final_lc[] IS NOT INITIAL.
    gt_final[]  = gt_final_lc[].
  ENDIF.


ENDFORM.

FORM cott_fiber3_mixing4.

  CLEAR : gt_aufm,gt_mara,gt_makt ,gs_final_lc , gt_final_lc[] , gs_final,gt_aufm_clb,gt_clb,gt_aufm_ex.

  IF gt_final IS NOT INITIAL.

    SELECT  mblnr
            mjahr
            zeile
            budat
            bwart
            matnr
            werks
            lgort
            charg
            menge
            aufnr
            FROM aufm
            INTO TABLE gt_aufm
            FOR ALL ENTRIES IN gt_final
            WHERE aufnr = gt_final-mix_prod3
            AND   bwart = '261'.
    IF sy-subrc = 0.
      SELECT mblnr,
             mjahr,
             zeile,
             sjahr,
             smbln,
             smblp
             FROM mseg
             INTO TABLE @DATA(gt_rev)
             FOR ALL ENTRIES IN @gt_aufm
             WHERE sjahr = @gt_aufm-mjahr
             AND   smbln = @gt_aufm-mblnr
             AND   smblp = @gt_aufm-zeile.
    ENDIF.

    SORT gt_rev ASCENDING BY sjahr smbln smblp.
    LOOP AT gt_aufm INTO gs_aufm.
      READ TABLE gt_rev INTO DATA(gs_rev) WITH KEY sjahr = gs_aufm-mjahr
                                                   smbln = gs_aufm-mblnr
                                                   smblp = gs_aufm-zeile BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE TABLE gt_aufm FROM gs_aufm.
      ENDIF.
    ENDLOOP.


    IF gt_aufm IS NOT INITIAL.
      SELECT matnr
             mtart
             matkl
             FROM mara
             APPENDING TABLE gt_mara
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr.

      SELECT mandt
             matnr
             spras
             maktx
             maktg
             FROM makt
             APPENDING TABLE gt_makt
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr
             AND   spras = 'EN'.

    ENDIF.
  ENDIF.


  DATA : last_7           TYPE sy-datum,
         last_15          TYPE sy-datum,
         lv_date          TYPE sy-datum,
         exit_flag        TYPE flag,
         batch_found_date TYPE sy-datum.


  SORT gt_mara ASCENDING BY matnr.
  SORT gt_makt ASCENDING BY matnr.
  SORT gt_lead_time  ASCENDING  BY zplant zyarn_code zcott_code.

  LOOP AT gt_final INTO gs_final.

    READ TABLE gt_lead_time INTO DATA(gs_lead_time1) WITH KEY zplant     = gs_final-werks
                                                              zyarn_code = gs_final-single_ymatnr BINARY SEARCH.
    IF sy-subrc = 0.

    ELSE.
      last_7  = gs_final-single_ymrn - 7.
      last_15 = gs_final-single_ymrn - 15.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod3
                                   AND ( budat LE last_7  AND budat GE last_15 ).
        SORT gt_aufm ASCENDING BY aufnr budat.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        last_7 = last_15 + 1.
        last_15 = '20180101'.
        exit_flag = 'X'.
        SORT gt_aufm DESCENDING BY aufnr budat mblnr.
      ENDIF.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod3 AND ( budat LE last_7  AND budat GE last_15 ).

        IF ( batch_found_date NE gs_aufm-budat ) AND batch_found_date IS NOT INITIAL.
          EXIT.
        ENDIF.

        gs_clb-aufnr = gs_aufm-aufnr.
        gs_clb-werks = gs_aufm-werks.
        gs_clb-matnr = gs_aufm-matnr.
        gs_clb-menge = gs_aufm-menge.
        gs_clb-charg = gs_aufm-charg.
        COLLECT gs_clb INTO gt_clb. CLEAR:gs_clb.

        IF exit_flag = 'X'.
          batch_found_date = gs_aufm-budat.
          CLEAR : exit_flag.
        ENDIF.

        CLEAR : gs_mara, gs_makt,gs_final_lc.
      ENDLOOP.

    ENDIF.


    IF gt_clb[] IS NOT INITIAL.

      LOOP AT gt_clb INTO gs_clb.

        MOVE-CORRESPONDING gs_final TO gs_final_lc.


        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_clb-matnr.
        IF sy-subrc = 0.

          READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_clb-matnr BINARY SEARCH.

          IF gs_mara-matkl EQ 'MIX'.

            gs_final_lc-mix_matnr4     = gs_clb-matnr.
            gs_final_lc-mix_matnr4_des = gs_makt-maktx.
            gs_final_lc-mix_plant4     = gs_clb-werks.
            gs_final_lc-mix_batch4     = gs_clb-charg.

            PERFORM goods_mvt_101 USING  gs_final_lc-mix_matnr4
                                         gs_final_lc-werks
                                         gs_final_lc-mix_batch4
                                         CHANGING  gs_final_lc-mix_prod4
                                                   gs_final_lc-mix_po4      "Not Req.
                                                   gs_final_lc-mix_poline4  "Not Req.
                                                   gs_final_lc-mix_vendor4   "Not Req.
                                                   gs_final_lc-mix_plant4   "Not Req.
                                                   gs_final_lc-mix_val4     "Not Req.
                                                   gs_final_lc-mix_qty4
                                                   gs_final_lc-mix_mrn4.
            gs_final_lc-mix_qty4       = gs_clb-menge.
          ELSE.

            gs_final_lc-cott_matnr3     = gs_clb-matnr.
            gs_final_lc-cott_matnr3_des = gs_makt-maktx.
            gs_final_lc-cott_plant3     = gs_clb-werks.
            gs_final_lc-cott_batch3     = gs_clb-charg.

            PERFORM goods_mvt_cott USING  gs_final_lc-cott_matnr3
                                          gs_final_lc-cott_plant3
                                          gs_final_lc-cott_batch3
                                   CHANGING   gs_final_lc-cott_prod3
                                              gs_final_lc-cott_po3       "Not Req.
                                              gs_final_lc-cott_poline3   "Not Req.
                                              gs_final_lc-cott_vendor3
                                              gs_final_lc-cott_vname3
                                              gs_final_lc-cott_qty3
                                              gs_final_lc-cott_mrn3
                                              gs_final_lc-cott_mrn3_no
                                              gs_final_lc-cott_mrn3_year
                                              gs_final_lc-cott_mrn3_itm.

            gs_final_lc-cott_qty3       = gs_clb-menge.
          ENDIF.
        ENDIF.

        APPEND gs_final_lc TO gt_final_lc.

      ENDLOOP.

    ELSE.
      MOVE-CORRESPONDING gs_final TO gs_final_lc.
      APPEND gs_final_lc TO gt_final_lc.
    ENDIF.

    CLEAR : last_7,last_15,gs_final_lc,exit_flag,batch_found_date,gt_clb.
  ENDLOOP.

  IF gt_final_lc[] IS NOT INITIAL.
    gt_final[]  = gt_final_lc[].
  ENDIF.


ENDFORM.

FORM cott_fiber4.

  CLEAR : gt_aufm,gt_mara,gt_makt ,gs_final_lc , gt_final_lc[] , gs_final,gt_aufm_clb,gt_clb,gt_aufm_ex.

  IF gt_final IS NOT INITIAL.

    SELECT  mblnr
            mjahr
            zeile
            budat
            bwart
            matnr
            werks
            lgort
            charg
            menge
            aufnr
            FROM aufm
            INTO TABLE gt_aufm
            FOR ALL ENTRIES IN gt_final
            WHERE aufnr = gt_final-mix_prod4
            AND   bwart = '261'.
    IF sy-subrc = 0.
      SELECT mblnr,
             mjahr,
             zeile,
             sjahr,
             smbln,
             smblp
             FROM mseg
             INTO TABLE @DATA(gt_rev)
             FOR ALL ENTRIES IN @gt_aufm
             WHERE sjahr = @gt_aufm-mjahr
             AND   smbln = @gt_aufm-mblnr
             AND   smblp = @gt_aufm-zeile.
    ENDIF.

    SORT gt_rev ASCENDING BY sjahr smbln smblp.
    LOOP AT gt_aufm INTO gs_aufm.
      READ TABLE gt_rev INTO DATA(gs_rev) WITH KEY sjahr = gs_aufm-mjahr
                                                   smbln = gs_aufm-mblnr
                                                   smblp = gs_aufm-zeile BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE TABLE gt_aufm FROM gs_aufm.
      ENDIF.
    ENDLOOP.


    IF gt_aufm IS NOT INITIAL.

      SELECT matnr
             mtart
             matkl
             FROM mara
             APPENDING TABLE gt_mara
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr.

      SELECT mandt
             matnr
             spras
             maktx
             maktg
             FROM makt
             APPENDING TABLE gt_makt
             FOR ALL ENTRIES IN gt_aufm
             WHERE matnr = gt_aufm-matnr
             AND   spras = 'EN'.

    ENDIF.
  ENDIF.


  DATA : last_7           TYPE sy-datum,
         last_15          TYPE sy-datum,
         lv_date          TYPE sy-datum,
         exit_flag        TYPE flag,
         batch_found_date TYPE sy-datum.

  SORT gt_mara ASCENDING BY matnr.
  SORT gt_makt ASCENDING BY matnr.
  SORT gt_lead_time  ASCENDING  BY zplant zyarn_code zcott_code.

  LOOP AT gt_final INTO gs_final.

    READ TABLE gt_lead_time INTO DATA(gs_lead_time1) WITH KEY zplant     = gs_final-werks
                                                              zyarn_code = gs_final-single_ymatnr BINARY SEARCH.
    IF sy-subrc = 0.

    ELSE.
      last_7  = gs_final-single_ymrn - 7.
      last_15 = gs_final-single_ymrn - 15.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod4
                                   AND ( budat LE last_7  AND budat GE last_15 ).
        SORT gt_aufm ASCENDING BY aufnr budat.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        last_7 = last_15 + 1.
        last_15 = '20180101'.
        exit_flag = 'X'.
        SORT gt_aufm DESCENDING BY aufnr budat mblnr.
      ENDIF.

      LOOP AT gt_aufm INTO gs_aufm WHERE aufnr = gs_final-mix_prod4 AND ( budat LE last_7  AND budat GE last_15 ).

        IF ( batch_found_date NE gs_aufm-budat ) AND batch_found_date IS NOT INITIAL.
          EXIT.
        ENDIF.

        gs_clb-aufnr = gs_aufm-aufnr.
        gs_clb-werks = gs_aufm-werks.
        gs_clb-matnr = gs_aufm-matnr.
        gs_clb-menge = gs_aufm-menge.
        gs_clb-charg = gs_aufm-charg.
        COLLECT gs_clb INTO gt_clb. CLEAR:gs_clb.

        IF exit_flag = 'X'.
          batch_found_date = gs_aufm-budat.
          CLEAR : exit_flag.
        ENDIF.

        CLEAR : gs_mara, gs_makt,gs_final_lc.
      ENDLOOP.

    ENDIF.

    IF gt_clb[] IS NOT INITIAL.
      LOOP AT gt_clb INTO gs_clb.

        MOVE-CORRESPONDING gs_final TO gs_final_lc.

        READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_clb-matnr BINARY SEARCH.
        IF sy-subrc = 0.

          READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_clb-matnr BINARY SEARCH.

          IF gs_mara-matkl EQ 'MIX'.

          ELSE.

            gs_final_lc-cott_matnr4     = gs_clb-matnr.
            gs_final_lc-cott_matnr4_des = gs_makt-maktx.
            gs_final_lc-cott_plant4     = gs_clb-werks.
            gs_final_lc-cott_batch4     = gs_clb-charg.

            PERFORM goods_mvt_cott USING  gs_final_lc-cott_matnr4
                                          gs_final_lc-cott_plant4
                                          gs_final_lc-cott_batch4
                                   CHANGING   gs_final_lc-cott_prod4
                                              gs_final_lc-cott_po4       "Not Req.
                                              gs_final_lc-cott_poline4   "Not Req.
                                              gs_final_lc-cott_vendor4
                                              gs_final_lc-cott_vname4
                                              gs_final_lc-cott_qty4
                                              gs_final_lc-cott_mrn4
                                              gs_final_lc-cott_mrn4_no
                                              gs_final_lc-cott_mrn4_year
                                              gs_final_lc-cott_mrn4_itm.

            gs_final_lc-cott_qty4       = gs_clb-menge.
          ENDIF.

        ENDIF.

        APPEND gs_final_lc TO gt_final_lc.

      ENDLOOP.
    ELSE.
      MOVE-CORRESPONDING gs_final TO gs_final_lc.
      APPEND gs_final_lc TO gt_final_lc.
    ENDIF.


    CLEAR : last_7,last_15,gs_final_lc,exit_flag , batch_found_date,gt_final_lc,gt_clb.
  ENDLOOP.

  IF gt_final_lc[] IS NOT INITIAL.
    gt_final[]  = gt_final_lc[].
  ENDIF.

  gt_final_lc = gt_final.
  CLEAR : gt_final.
  LOOP AT gt_final_lc INTO gs_final_lc.

    MOVE-CORRESPONDING gs_final_lc TO gs_final.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-doub_ymatnr
      IMPORTING
        output = gs_final-doub_ymatnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-doub_yprod
      IMPORTING
        output = gs_final-doub_yprod.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-single_ymatnr
      IMPORTING
        output = gs_final-single_ymatnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-single_yprod
      IMPORTING
        output = gs_final-single_yprod.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_matnr1
      IMPORTING
        output = gs_final-mix_matnr1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_prod1
      IMPORTING
        output = gs_final-mix_prod1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-cott_matnr1
      IMPORTING
        output = gs_final-cott_matnr1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-cott_vendor1
      IMPORTING
        output = gs_final-cott_vendor1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_matnr2
      IMPORTING
        output = gs_final-mix_matnr2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_prod2
      IMPORTING
        output = gs_final-mix_prod2.



    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-cott_matnr2
      IMPORTING
        output = gs_final-cott_matnr2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_matnr3
      IMPORTING
        output = gs_final-mix_matnr3.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_prod3
      IMPORTING
        output = gs_final-mix_prod3.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-cott_matnr3
      IMPORTING
        output = gs_final-cott_matnr3.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_matnr4
      IMPORTING
        output = gs_final-mix_matnr4.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-mix_prod4
      IMPORTING
        output = gs_final-mix_prod4.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_final-cott_matnr4
      IMPORTING
        output = gs_final-cott_matnr4.

    COLLECT gs_final INTO gt_final.
    CLEAR : gs_final.
  ENDLOOP.

ENDFORM.

FORM summary.

  CLEAR : gt_final_lc[].


  LOOP AT gt_final INTO gs_final.

    gs_final_lc-testing_dt      = gs_final-testing_dt.
    gs_final_lc-werks           = gs_final-werks.
    gs_final_lc-matnr           = gs_final-matnr.
    gs_final_lc-lot_no          = gs_final-lot_no.
    gs_final_lc-maktx           = gs_final-maktx.


    gs_final_lc-doub_ymatnr     = gs_final-doub_ymatnr.
    gs_final_lc-doub_ymatnr_des = gs_final-doub_ymatnr_des.
    gs_final_lc-doub_ybatch     = gs_final-doub_ybatch.
    gs_final_lc-doub_ymrn       = gs_final-doub_ymrn.
    gs_final_lc-doub_yqty       = gs_final-doub_yqty.
    gs_final_lc-doub_yprod      = gs_final-doub_yprod.
    gs_final_lc-doub_yval       = gs_final-doub_yval.
    gs_final_lc-doub_ypo        = gs_final-doub_ypo.
    gs_final_lc-doub_ypoline    = gs_final-doub_ypoline.
    gs_final_lc-doub_yvendor    = gs_final-doub_yvendor.

    gs_final_lc-single_ymatnr     = gs_final-single_ymatnr.
    gs_final_lc-single_ymatnr_des = gs_final-single_ymatnr_des.
    gs_final_lc-single_ybatch     = gs_final-single_ybatch.
    gs_final_lc-single_ymrn       = gs_final-single_ymrn.
    gs_final_lc-single_yqty       = gs_final-single_yqty.
    gs_final_lc-single_yprod      = gs_final-single_yprod.
    gs_final_lc-single_yval       = gs_final-single_yval.
    gs_final_lc-single_ypo        = gs_final-single_ypo.
    gs_final_lc-single_ypoline    = gs_final-single_ypoline.
    gs_final_lc-single_yvendor    = gs_final-single_yvendor.

*_Uploaded__
    gs_final_lc-lot_no          = gs_final-lot_no.
    gs_final_lc-lab_no          = gs_final-lab_no.
    gs_final_lc-inv_no          = gs_final-inv_no.
    gs_final_lc-inv_qty         = gs_final-inv_qty.
    gs_final_lc-thin            = gs_final-thin.
    gs_final_lc-thik            = gs_final-thik.
    gs_final_lc-neps            = gs_final-neps.
    gs_final_lc-hairiness       = gs_final-hairiness.
    gs_final_lc-s3              = gs_final-s3.
    gs_final_lc-b_force         = gs_final-b_force.
    gs_final_lc-elong_per       = gs_final-elong_per.
    gs_final_lc-elong_per_p     = gs_final-elong_per_p.
    gs_final_lc-rkm             = gs_final-rkm.
    gs_final_lc-rkm_01          = gs_final-rkm_01.
    gs_final_lc-rkm_cv_per      = gs_final-rkm_cv_per.
    gs_final_lc-b_work          = gs_final-b_work.
    gs_final_lc-b_work_01       = gs_final-b_work_01.
    gs_final_lc-mean_count      = gs_final-mean_count.
    gs_final_lc-count_cv_per    = gs_final-count_cv_per.
    gs_final_lc-strength_cv_per = gs_final-strength_cv_per.
    gs_final_lc-csp             = gs_final-csp.
    gs_final_lc-tpi             = gs_final-tpi.
    gs_final_lc-tm              = gs_final-tm.
    gs_final_lc-a1a2b1          = gs_final-a1a2b1.
    gs_final_lc-a3tod2          = gs_final-a3tod2.
    gs_final_lc-obj_faults      = gs_final-obj_faults.
    gs_final_lc-efg             = gs_final-efg.
    gs_final_lc-h1              = gs_final-h1.
    gs_final_lc-h2              = gs_final-h2.
    gs_final_lc-total_classimat = gs_final-total_classimat.
    gs_final_lc-fd           = gs_final-fd.
    gs_final_lc-pp_lt_10     = gs_final-pp_lt_10.
    gs_final_lc-remarks      = gs_final-remarks.
    gs_final_lc-approval     = gs_final-approval.
    gs_final_lc-feedback     = gs_final-feedback.
    gs_final_lc-ucv          = gs_final-ucv.
    gs_final_lc-total_imp    = gs_final-total_imp.
    gs_final_lc-hcv          = gs_final-hcv.
    gs_final_lc-b_force1     = gs_final-b_force1.
    gs_final_lc-rkm_cv_per1  = gs_final-rkm_cv_per1.
    gs_final_lc-pp_gt_10     = gs_final-pp_gt_10.
    gs_final_lc-pp_lt_101    = gs_final-pp_lt_101.
    gs_final_lc-supplier     = gs_final-supplier.
    gs_final_lc-tpi_27       = gs_final-tpi_27.
    gs_final_lc-tm_4         = gs_final-tm_4.
    gs_final_lc-tpi_37       = gs_final-tpi_37.
    gs_final_lc-classimat    = gs_final-classimat.
*_Uploaded__



*_Level-1
    gs_final_lc-mix_matnr1       = gs_final-mix_matnr1.
    gs_final_lc-mix_matnr1_des   = gs_final-mix_matnr1_des.
    gs_final_lc-mix_batch1       = gs_final-mix_batch1.
    gs_final_lc-mix_mrn1         = gs_final-mix_mrn1.
    gs_final_lc-mix_prod1        = gs_final-mix_prod1.
    gs_final_lc-mix_qty1         = gs_final-mix_qty1.

    gs_final_lc-cott_plant1      = gs_final-cott_plant1.
    gs_final_lc-cott_matnr1      = gs_final-cott_matnr1.
    gs_final_lc-cott_matnr1_des  = gs_final-cott_matnr1_des.
    gs_final_lc-cott_batch1      = gs_final-cott_batch1.
    gs_final_lc-cott_mrn1        = gs_final-cott_mrn1.
    gs_final_lc-cott_mrn1_no     = gs_final-cott_mrn1_no.
    gs_final_lc-cott_mrn1_year   = gs_final-cott_mrn1_year.
    gs_final_lc-cott_mrn1_itm    = gs_final-cott_mrn1_itm.
    gs_final_lc-cott_vendor1     = gs_final-cott_vendor1.
    gs_final_lc-cott_vname1      = gs_final-cott_vname1.
    APPEND gs_final_lc TO gt_final_lc.

*_Level-2
    gs_final_lc-mix_matnr1       = gs_final-mix_matnr2.
    gs_final_lc-mix_matnr1_des   = gs_final-mix_matnr2_des.
    gs_final_lc-mix_batch1       = gs_final-mix_batch2.
    gs_final_lc-mix_mrn1         = gs_final-mix_mrn2.
    gs_final_lc-mix_prod1        = gs_final-mix_prod2.
    gs_final_lc-mix_qty1         = gs_final-mix_qty2.

    gs_final_lc-cott_plant1      = gs_final-cott_plant2.
    gs_final_lc-cott_matnr1      = gs_final-cott_matnr2.
    gs_final_lc-cott_matnr1_des  = gs_final-cott_matnr2_des.
    gs_final_lc-cott_batch1      = gs_final-cott_batch2.
    gs_final_lc-cott_mrn1        = gs_final-cott_mrn2.
    gs_final_lc-cott_mrn1_no     = gs_final-cott_mrn2_no.
    gs_final_lc-cott_mrn1_year   = gs_final-cott_mrn2_year.
    gs_final_lc-cott_mrn1_itm    = gs_final-cott_mrn2_itm.

    gs_final_lc-cott_vendor1     = gs_final-cott_vendor2.
    gs_final_lc-cott_vname1      = gs_final-cott_vname2.
    APPEND gs_final_lc TO gt_final_lc.

*_Level-3
    gs_final_lc-mix_matnr1       = gs_final-mix_matnr3.
    gs_final_lc-mix_matnr1_des   = gs_final-mix_matnr3_des.
    gs_final_lc-mix_batch1       = gs_final-mix_batch3.
    gs_final_lc-mix_mrn1         = gs_final-mix_mrn3.
    gs_final_lc-mix_prod1        = gs_final-mix_prod3.
    gs_final_lc-mix_qty1         = gs_final-mix_qty3.

    gs_final_lc-cott_plant1      = gs_final-cott_plant3.
    gs_final_lc-cott_matnr1      = gs_final-cott_matnr3.
    gs_final_lc-cott_matnr1_des  = gs_final-cott_matnr3_des.
    gs_final_lc-cott_batch1      = gs_final-cott_batch3.
    gs_final_lc-cott_mrn1        = gs_final-cott_mrn3.
    gs_final_lc-cott_mrn1_no     = gs_final-cott_mrn3_no.
    gs_final_lc-cott_mrn1_year   = gs_final-cott_mrn3_year.
    gs_final_lc-cott_mrn1_itm    = gs_final-cott_mrn3_itm.

    gs_final_lc-cott_vendor1     = gs_final-cott_vendor3.
    gs_final_lc-cott_vname1      = gs_final-cott_vname3.
    APPEND gs_final_lc TO gt_final_lc.

*_Level-4
    gs_final_lc-mix_matnr1       = gs_final-mix_matnr4.
    gs_final_lc-mix_matnr1_des   = gs_final-mix_matnr4_des.
    gs_final_lc-mix_batch1       = gs_final-mix_batch4.
    gs_final_lc-mix_mrn1         = gs_final-mix_mrn4.
    gs_final_lc-mix_prod1        = gs_final-mix_prod4.
    gs_final_lc-mix_qty1         = gs_final-mix_qty4.

    gs_final_lc-cott_plant1      = gs_final-cott_plant4.
    gs_final_lc-cott_matnr1      = gs_final-cott_matnr4.
    gs_final_lc-cott_matnr1_des  = gs_final-cott_matnr4_des.
    gs_final_lc-cott_batch1      = gs_final-cott_batch4.
    gs_final_lc-cott_mrn1        = gs_final-cott_mrn4.
    gs_final_lc-cott_mrn1_no     = gs_final-cott_mrn4_no.
    gs_final_lc-cott_mrn1_year   = gs_final-cott_mrn4_year.
    gs_final_lc-cott_mrn1_itm    = gs_final-cott_mrn4_itm.
    gs_final_lc-cott_vendor1     = gs_final-cott_vendor4.
    gs_final_lc-cott_vname1      = gs_final-cott_vname4.
    APPEND gs_final_lc TO gt_final_lc.
    CLEAR : gs_final_lc.

  ENDLOOP.

  CLEAR : gt_final.
  gt_final[] = gt_final_lc[].


ENDFORM.

FORM field_cat.

  DEFINE mac.

    gs_fieldcat-fieldname = &1.
    gs_fieldcat-scrtext_l = &2.
    gs_fieldcat-checkbox = &3.
    gs_fieldcat-icon = &4.
    gs_fieldcat-tabname = &5.
    gs_fieldcat-outputlen = &6.
    gs_fieldcat-edit = &7.
    gs_fieldcat-emphasize = &8.
    gs_fieldcat-datatype = &9.
    APPEND gs_fieldcat TO gt_fieldcat.

  END-OF-DEFINITION.

*_Layout
  ls_layout-zebra = 'X'.
  ls_layout-stylefname  = 'CELLTAB'.

*  mac 'SEL'              'Select'                 'X' '' 'GT_FINAL'   '05' 'X' ''.

  mac 'TESTING_DT'       'Testing Date'           '' '' 'GT_FINAL'   '10' '' 'C110' 'DATS'.
  mac 'WERKS'            'Plant'                  '' '' 'GT_FINAL'   '06' '' 'C110' ''.
  mac 'LOT_NO'           'Yarn Lot No'            '' '' 'GT_FINAL'   '10' '' 'C110' ''.
  mac 'TABLE'            'Lead Flag'              '' '' 'GT_FINAL'   '10' '' 'C110' ''.
*  mac 'MATNR'            'Yarn Code'              '' '' 'GT_FINAL'   '18' '' 'C110'.
*  mac 'MAKTX'            'Yarn Description'       '' '' 'GT_FINAL'   '40' '' 'C110'.

*_Double
  mac 'DOUB_YMATNR'            'Dob.Yarn Code'              '' '' 'GT_FINAL'   '18' '' 'C310' ''.
  mac 'DOUB_YMATNR_DES'        'Dob.Yarn Code Desc.'        '' '' 'GT_FINAL'   '40' '' 'C310' ''.
  mac 'DOUB_YBATCH'            'Dob.Yarn Batch'             '' '' 'GT_FINAL'   '10' '' 'C310' ''.
  mac 'DOUB_YMRN'              'Dob.Yarn MRN'               '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
  mac 'DOUB_YQTY'              'Dob.Yarn Qty.'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
  mac 'DOUB_YPROD'             'Dob.Yarn ProdOrd'           '' '' 'GT_FINAL'   '12' '' 'C310' ''.
  mac 'DOUB_YVAL'              'Dob.Yarn Val Type'          '' '' 'GT_FINAL'   '10' '' 'C310' ''.
  mac 'DOUB_YPO'               'Dob.Yarn PO'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.
  mac 'DOUB_YPOLINE'           'Dob.Yarn Item'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
  mac 'DOUB_YVENDOR'           'Dob.Yarn Vendor'            '' '' 'GT_FINAL'   '10' '' 'C310' ''.

*_Single
  mac 'SINGLE_YMATNR'          'Sin.Yarn Code'              '' '' 'GT_FINAL'   '18' '' 'C510' ''.
  mac 'SINGLE_YMATNR_DES'      'Sin.Yarn Code Desc.'        '' '' 'GT_FINAL'   '40' '' 'C510' ''.
  mac 'SINGLE_YBATCH'          'Sin.Yarn Batch'             '' '' 'GT_FINAL'   '10' '' 'C510' ''.
  mac 'SINGLE_YMRN'            'Sin.Yarn MRN'               '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
  mac 'SINGLE_YQTY'            'Sin.Yarn Qty.'              '' '' 'GT_FINAL'   '10' '' 'C510' ''.
  mac 'SINGLE_YPROD'           'Sin.Yarn ProdOrd'           '' '' 'GT_FINAL'   '12' '' 'C510' ''.
  mac 'SINGLE_YVAL'            'Sin.Yarn Val Type'          '' '' 'GT_FINAL'   '10' '' 'C510' ''.
  mac 'SINGLE_YPO'             'Sin.Yarn STO Plant'         '' '' 'GT_FINAL'   '10' '' 'C510' ''.
  mac 'SINGLE_YPOLINE'         'Sin.Yarn Item'              '' '' 'GT_FINAL'   '10' '' 'C510' ''.
  mac 'SINGLE_YVENDOR'         'Sin.Yarn Vendor'            '' '' 'GT_FINAL'   '10' '' 'C510' ''.




  IF dtl = 'X'.

*_Mixing material 1
    mac 'MIX_MATNR1'            'Mixing Code-1'               '' '' 'GT_FINAL'   '18' '' 'C310' ''.
    mac 'MIX_MATNR1_DES'        'Mixing Code Desc-1'          '' '' 'GT_FINAL'   '40' '' 'C310' ''.
    mac 'MIX_BATCH1'            'Mixing Batch-1'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
    mac 'MIX_MRN1'              'Mixing MRN-1'                '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
    mac 'MIX_PROD1'             'Mixing ProdOrd-1'            '' '' 'GT_FINAL'   '12' '' 'C310' ''.
    mac 'MIX_QTY1'              'Mixing Qty-1'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.

*_Cotton 1
    mac 'COTT_PLANT1'            'COTT Plant-1'                 '' '' 'GT_FINAL'   '15' '' 'C510' ''.
    mac 'COTT_MATNR1'            'COTT Code-1'                  '' '' 'GT_FINAL'   '18' '' 'C510' ''.
    mac 'COTT_MATNR1_DES'        'COTT Code Desc-1'             '' '' 'GT_FINAL'   '40' '' 'C510' ''.
    mac 'COTT_BATCH1'            'COTT Batch-1'                 '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN1'              'COTT MRN-1'                   '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
    mac 'COTT_VENDOR1'           'COTT Vendor-1'                '' '' 'GT_FINAL'   '12' '' 'C510' ''.
    mac 'COTT_VNAME1'            'COTT Vendor Nam-1'            '' '' 'GT_FINAL'   '40' '' 'C510' ''.


*_Mixing material 2
    mac 'MIX_PLANT2'            'Mixing Plant-2'              '' '' 'GT_FINAL'   '15' '' 'C310' ''.
    mac 'MIX_MATNR2'            'Mixing Code-2'               '' '' 'GT_FINAL'   '18' '' 'C310' ''.
    mac 'MIX_MATNR2_DES'        'Mixing Code Desc-2'          '' '' 'GT_FINAL'   '40' '' 'C310' ''.
    mac 'MIX_BATCH2'            'Mixing Batch-2'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
    mac 'MIX_MRN2'              'Mixing MRN-2'                '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
    mac 'MIX_PROD2'             'Mixing ProdOrd-2'            '' '' 'GT_FINAL'   '12' '' 'C310' ''.
    mac 'MIX_QTY2'              'Mixing Qty-2'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.


*_Cotton 2
    mac 'COTT_PLANT2'            'COTT Plant-2'                 '' '' 'GT_FINAL'   '15' '' 'C510' ''.
    mac 'COTT_MATNR2'            'COTT Code-2'                  '' '' 'GT_FINAL'   '18' '' 'C510' ''.
    mac 'COTT_MATNR2_DES'        'COTT Code Desc-2'             '' '' 'GT_FINAL'   '40' '' 'C510' ''.
    mac 'COTT_BATCH2'            'COTT Batch-2'                 '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN2'              'COTT MRN-2'                   '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
    mac 'COTT_VENDOR2'           'COTT Vendor-2'                '' '' 'GT_FINAL'   '12' '' 'C510' ''.
    mac 'COTT_VNAME2'            'COTT Vendor Nam-2'            '' '' 'GT_FINAL'   '40' '' 'C510' ''.


*_Mixing material 3
    mac 'MIX_PLANT3'            'Mixing Plant-3'              '' '' 'GT_FINAL'   '15' '' 'C310' ''.
    mac 'MIX_MATNR3'            'Mixing Code-3'               '' '' 'GT_FINAL'   '18' '' 'C310' ''.
    mac 'MIX_MATNR3_DES'        'Mixing Code Desc-3'          '' '' 'GT_FINAL'   '40' '' 'C310' ''.
    mac 'MIX_BATCH3'            'Mixing Batch-3'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
    mac 'MIX_MRN3'              'Mixing MRN-3'                '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
    mac 'MIX_PROD3'             'Mixing ProdOrd-3'            '' '' 'GT_FINAL'   '12' '' 'C310' ''.
    mac 'MIX_QTY3'              'Mixing Qty-3'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.

*_Cotton 3
    mac 'COTT_PLANT3'            'COTT Plant-3'                 '' '' 'GT_FINAL'   '15' '' 'C510' ''.
    mac 'COTT_MATNR3'            'COTT Code-3'                  '' '' 'GT_FINAL'   '18' '' 'C510' ''.
    mac 'COTT_MATNR3_DES'        'COTT Code Desc-3'             '' '' 'GT_FINAL'   '40' '' 'C510' ''.
    mac 'COTT_BATCH3'            'COTT Batch-3'                 '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN3'              'COTT MRN-3'                   '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
    mac 'COTT_VENDOR3'           'COTT Vendor-3'                '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_VNAME3'            'COTT Vendor Nam-3'            '' '' 'GT_FINAL'   '40' '' 'C510' ''.


*_Mixing material 4
    mac 'MIX_PLANT4'            'Mixing Plant-4'              '' '' 'GT_FINAL'   '15' '' 'C310' ''.
    mac 'MIX_MATNR4'            'Mixing Code-4'               '' '' 'GT_FINAL'   '18' '' 'C310' ''.
    mac 'MIX_MATNR4_DES'        'Mixing Code Desc-4'          '' '' 'GT_FINAL'   '40' '' 'C310' ''.
    mac 'MIX_BATCH4'            'Mixing Batch-4'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
    mac 'MIX_MRN4'              'Mixing MRN-4'                '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
    mac 'MIX_PROD4'             'Mixing ProdOrd-4'            '' '' 'GT_FINAL'   '12' '' 'C310' ''.
    mac 'MIX_QTY4'              'Mixing Qty-4'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.


*_Cotton 4
    mac 'COTT_PLANT4'            'COTT Plant-4'                 '' '' 'GT_FINAL'   '15' '' 'C510' ''.
    mac 'COTT_MATNR4'            'COTT Code-4'                  '' '' 'GT_FINAL'   '18' '' 'C510' ''.
    mac 'COTT_MATNR4_DES'        'COTT Code Desc-4'             '' '' 'GT_FINAL'   '40' '' 'C510' ''.
    mac 'COTT_BATCH4'            'COTT Batch-4'                 '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN4'              'COTT MRN-4'                   '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
    mac 'COTT_VENDOR4'           'COTT Vendor-4'                '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_VNAME4'            'COTT Vendor Nam-4'            '' '' 'GT_FINAL'   '40' '' 'C510' ''.


  ELSEIF sum = 'X'.

*_Mixing material 1
    mac 'MIX_MATNR1'            'Mixing Code'               '' '' 'GT_FINAL'   '18' '' 'C310' ''.
    mac 'MIX_MATNR1_DES'        'Mixing Code Desc'          '' '' 'GT_FINAL'   '40' '' 'C310' ''.
    mac 'MIX_BATCH1'            'Mixing Batch'              '' '' 'GT_FINAL'   '10' '' 'C310' ''.
    mac 'MIX_MRN1'              'Mixing MRN'                '' '' 'GT_FINAL'   '10' '' 'C310' 'DATS'.
    mac 'MIX_PROD1'             'Mixing ProdOrd'            '' '' 'GT_FINAL'   '12' '' 'C310' ''.
    mac 'MIX_QTY1'              'Mixing Qty'                '' '' 'GT_FINAL'   '10' '' 'C310' ''.

*_Cotton 1
    mac 'COTT_PLANT1'            'COTT Plant'                 '' '' 'GT_FINAL'   '15' '' 'C510' ''.
    mac 'COTT_MATNR1'            'COTT Code'                  '' '' 'GT_FINAL'   '18' '' 'C510' ''.
    mac 'COTT_MATNR1_DES'        'COTT Code Desc'             '' '' 'GT_FINAL'   '40' '' 'C510' ''.
    mac 'COTT_BATCH1'            'COTT Batch'                 '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN1_NO'           'COTT MRN No'                '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN1_YEAR'         'COTT MRN Year'              '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN1_ITM'          'COTT MRN Item'              '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_MRN1'              'COTT MRN Date'              '' '' 'GT_FINAL'   '10' '' 'C510' 'DATS'.
    mac 'COTT_VENDOR1'           'COTT Vendor'                '' '' 'GT_FINAL'   '10' '' 'C510' ''.
    mac 'COTT_VNAME1'            'COTT Vendor Nam'            '' '' 'GT_FINAL'   '40' '' 'C510' ''.

  ENDIF.

  MOVE-CORRESPONDING gt_final TO gt_zpp_yarn_track_db.
  IF gt_zpp_yarn_track_db IS NOT INITIAL.
    DATA : lv_cnt TYPE n LENGTH 5.
    SORT gt_zpp_yarn_track_db ASCENDING BY keycomb.

    LOOP AT gt_zpp_yarn_track_db INTO gs_zpp_yarn_track_db.
      lv_cnt = lv_cnt + 1.
      gs_zpp_yarn_track_db-counter = lv_cnt.
      MODIFY gt_zpp_yarn_track_db FROM gs_zpp_yarn_track_db TRANSPORTING counter.
      AT END OF keycomb.
        CLEAR:lv_cnt.
      ENDAT.
    ENDLOOP.

    MODIFY  zpp_yarn_track FROM TABLE gt_zpp_yarn_track_db.
    COMMIT WORK AND WAIT.
  ENDIF.


  CLEAR:gt_final_lc.
  MOVE-CORRESPONDING gt_zpp_yarn_track TO gt_final_lc.
  APPEND LINES OF gt_final_lc TO gt_final.

ENDFORM.

FORM show_data.

  DATA : lv_index TYPE sy-tabix.

*_display alv output
  IF dtl = 'X'.
    SORT gt_final ASCENDING BY testing_dt werks matnr lot_no.
  ELSE.

    DELETE gt_final WHERE mix_matnr1 = ''
                    AND   mix_batch1 = ''
                    AND   cott_matnr1 = ''
                    AND   cott_batch1 = ''.

    SORT gt_final ASCENDING BY  testing_dt    werks
                                matnr         lot_no

                                doub_ymatnr   doub_ybatch

                                single_ymatnr single_ybatch

                                mix_matnr1    mix_batch1

                                cott_matnr1   cott_batch1.


    DELETE ADJACENT DUPLICATES FROM gt_final COMPARING testing_dt     werks matnr lot_no
                                                       doub_ymatnr    doub_ybatch
                                                       single_ymatnr  single_ybatch
                                                       mix_matnr1     mix_batch1
                                                       cott_matnr1    cott_batch1.


    gt_final_lc = gt_final.

    SORT gt_final_lc ASCENDING BY testing_dt
                                  werks
                                  matnr
                                  doub_ymatnr
                                  doub_ybatch
                                  single_ymatnr
                                  single_ybatch.


    LOOP AT gt_final INTO gs_final.

      IF  gs_final-cott_plant1 = ''.

        READ TABLE gt_final_lc INTO DATA(ls_final_lc) WITH KEY testing_dt     = gs_final-testing_dt
                                                                werks          = gs_final-werks
                                                                matnr          = gs_final-matnr
                                                                doub_ymatnr    = gs_final-doub_ymatnr
                                                                doub_ybatch    = gs_final-doub_ybatch
                                                                single_ymatnr  = gs_final-single_ymatnr
                                                                single_ybatch  = gs_final-single_ybatch BINARY SEARCH.
*                                                               cott_plant1    = gs_final-cott_plant1 .
        IF sy-subrc = 0.

          lv_index = sy-tabix.

          LOOP AT gt_final_lc INTO DATA(ls_final) FROM lv_index.

            IF ( ls_final_lc-testing_dt    NE ls_final-testing_dt )    OR ( ls_final_lc-werks         NE ls_final-werks )         OR
               ( ls_final_lc-matnr         NE ls_final-matnr )         OR ( ls_final_lc-doub_ymatnr   NE ls_final-doub_ymatnr )   OR
               ( ls_final_lc-doub_ybatch   NE ls_final-doub_ybatch )   OR ( ls_final_lc-single_ymatnr NE ls_final-single_ymatnr ) OR
               ( ls_final_lc-single_ybatch NE ls_final-single_ybatch ).
              EXIT.
            ENDIF.

            IF ls_final-cott_plant1  NE gs_final-cott_plant1.
              DELETE TABLE gt_final FROM gs_final.
              EXIT.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

  IF sum = 'X' AND gt_final IS NOT INITIAL.

    SELECT prueflos
           zaehler
           typ
           mblnr
           mjahr
           zeile
           FROM qamb
           INTO TABLE gt_qamb
           FOR ALL ENTRIES IN gt_final
           WHERE mblnr = gt_final-cott_mrn1_no
           AND   mjahr = gt_final-cott_mrn1_year.
    IF sy-subrc = 0.

      SELECT prueflos,
             vorglfnr,
             merknr,
             attribut,
             mbewertg,
             pruefbemkt,
             maschine,
             mittelwert,
             katalgart1,
             gruppe1,
             code1
             FROM qamr
             INTO TABLE @gt_qamr
             FOR ALL ENTRIES IN @gt_qamb
             WHERE prueflos = @gt_qamb-prueflos.
      IF sy-subrc = 0.
        SELECT  prueflos,
                vorglfnr,
                merknr,
                kurztext,
                verwmerkm
                FROM qamv
                INTO TABLE @DATA(gt_qamv01)
                FOR ALL ENTRIES IN @gt_qamr
                WHERE prueflos = @gt_qamr-prueflos
                AND   vorglfnr = @gt_qamr-vorglfnr
                AND   merknr   = @gt_qamr-merknr.
      ENDIF.

      SORT gt_qamb ASCENDING BY mblnr mjahr zeile.
      SORT gt_qamr ASCENDING BY prueflos vorglfnr merknr.
      SORT gt_qamv01 ASCENDING BY prueflos verwmerkm.

      DATA(gt_qamv02) = gt_qamv01.
      SORT gt_qamv02 ASCENDING BY verwmerkm.
      DELETE ADJACENT DUPLICATES FROM   gt_qamv02 COMPARING verwmerkm.

      CLEAR : gs_fieldcat.
      gs_fieldcat-fieldname  = 'COTT_MIC'.
      gs_fieldcat-seltext    = 'Cotton Insp.Lot'.
      gs_fieldcat-scrtext_m  = 'Cotton Insp.Lot'.
      gs_fieldcat-datatype   = 'CHAR'.
      gs_fieldcat-outputlen  = '15'.
      gs_fieldcat-emphasize  = 'C410'.
      APPEND gs_fieldcat TO gt_fieldcat.
      CLEAR : gs_fieldcat.
      LOOP AT gt_qamv02 INTO DATA(gs_qamv).
        gs_fieldcat-fieldname  = gs_qamv-verwmerkm.
        gs_fieldcat-seltext    = gs_qamv-kurztext.
        gs_fieldcat-scrtext_m  = gs_qamv-kurztext.
        gs_fieldcat-datatype   = 'CHAR'.
        gs_fieldcat-outputlen  = '10'.
        gs_fieldcat-emphasize  = 'C410'.
        APPEND gs_fieldcat TO gt_fieldcat.
        CLEAR : gs_fieldcat.
      ENDLOOP.

    ENDIF.


    IF mau = 'X'.

      DEFINE mac.

        gs_fieldcat-fieldname = &1.
        gs_fieldcat-scrtext_l = &2.
        gs_fieldcat-checkbox = &3.
        gs_fieldcat-icon = &4.
        gs_fieldcat-tabname = &5.
        gs_fieldcat-outputlen = &6.
        gs_fieldcat-edit = &7.
        gs_fieldcat-emphasize = &8.
        gs_fieldcat-datatype = &9.
        APPEND gs_fieldcat TO gt_fieldcat.

      END-OF-DEFINITION.


      mac 'LAB_NO'           'Lab No'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'INV_NO'           'Inv No'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'INV_QTY'          'Inv. Qty'               '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'THIN'             'Thin'                   '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'THIK'             'Thick'                  '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'NEPS'             'Neps'                   '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'HAIRINESS'        'Hairiness'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'S3'               'S3'                     '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'B_FORCE'          'B.force'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ELONG_PER'        'Elong %'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ELONG_PER_P'      'Elong % 0.1 P'          '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'RKM'              'RKM'                    ''  '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'RKM_01'           '0.1 RKm'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'RKM_CV_PER'       'RKM CV%'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'B_WORK'           'B-Work'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'B_WORK_01'        '0.1 B-Work'             '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'MEAN_COUNT'       'Mean Count'             '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'COUNT_CV_PER'     'Count C.V%'             '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'STRENGTH_CV_PER'  'Strength C.V%'          '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'CSP'              'C.S.P'                  '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TPI'              'TPI'                    '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TM'               'TM'                     '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'A1A2B1'           'A1A2B1'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'A3TOD2'           'A3TOD2'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'OBJ_FAULTS'       'Obj. Faults'            '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'EFG'              'EFG'                    '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'H1'               'H1'                     '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'H2'               'H2'                     '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TOTAL_CLASSIMAT'  'Total Classimat'        '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'FD'               'F.D'                    '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'PP_LT_10'         'P.P <10mm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'REMARKS'          'Remarks'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'APPROVAL'         'Approval'               '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'FEEDBACK'         'Feedback'               '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'UCV'              'U.C.V%'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TOTAL_IMP'        'Total Imperfection'     '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'HCV'              'H CV'                   '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'B_FORCE1'         'B.Force'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'RKM_CV_PER1'      'Rkm cv%'                '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'PP_GT_10'         'PP>=10'                 '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'PP_LT_101'        'PP<10'                  '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'SUPPLIER'         'Supplier'               '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TPI_27'           'TPI(27.2)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TM_4'             'T.M(4.28)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'TPI_37'           'TPI(37.5)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'CLASSIMAT'        'Classimat'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD1'          '+140% Neps/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD2'          '+200% Neps/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD3'          '+280% Neps/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD4'          '+50% Thick/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD5'          '-50% Thin/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD6'          '-50% Thin/Km.1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD7'          'A/C No.'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD8'          'Actual Count (Ne)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD9'          'A~D'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD10'          'BWS CV%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD11'          'BWS%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD12'          'C0'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD13'          'CVm%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD14'          'Category'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD15'          'Cone Tip'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD16'          'Cotton/Thermolite/coolmax%/Tencel%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD17'          'Count slubs nec'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD18'          'D0'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD19'          'DENIER'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD20'          'E+G'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD21'          'Eco in cone label'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD22'          'Ecv%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD23'          'Enviya Elastane I 400'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD24'          'F.D-8% 2cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD25'          'F.D.1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD26'          'H2+I1+I2'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD27'          'HI'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD28'          'IH/PUR'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD29'          'L Outliers'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD30'          'LYCRA'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD31'          'Lea Count (CV%)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD32'          'Lea Strength (CV%)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD33'          'Lea Strength (lbs)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD34'          'Long Thin(H 1)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD35'          'M .TPI'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD36'          'MED S THICK'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD37'          'MED S THICK FAULT(B2+C1+D1)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD38'          'MIXING-M/C No'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD39'          'Mass Decr. left'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD40'          'Mass Increases (%)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD41'          'Mass decr right'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD42'          'Mass incr P1%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD43'          'Mass incr P2%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD44'          'Mass incr left'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD45'          'Mass incr right'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD46'          'Mass incr%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD47'          'Mean'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD48'          'Viscose/cotton'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD49'          'Mean Count (Ne)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD50'          'Mech TM'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD51'          'Mech tm:4.0'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD52'          'Mechanical TM'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD53'          'Min   RKM'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD54'          'Min CSP'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD55'          'Min E%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD56'          'Min Tanacity'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD57'          'Month'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD58'          'N Outliers'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD59'          'Year'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD60'          'Nip/Mtr.'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD61'          'No of Outlier /km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD62'          'No of Slub P1 /m'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD63'          'No of slub cv%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD64'          'Nylon%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD65'          'P.1 Rkm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD66'          'P.P .10mm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD67'          'P.P >10m.m'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD68'          'P.P.1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD69'          'P.P<10mm 65%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD70'          'P.P>10mm 65%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD71'          'PARTY'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD72'          'PD'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD73'          'PP'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD74'          'PP <10mm [65%]'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD75'          'PP >10mm [65%]'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD76'          'PP Total [65%]'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD77'          'count base nec'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD78'          'pkg. wt(gms'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD79'          'Pkg. st.'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD80'          'Polyester'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD81'          'Total IPI'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD82'          'Total IPI/Km'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD83'          'RKM (Statimat ME+)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD84'          'Ratio T/B'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD85'          'Received Date'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD86'          'Unit / Source'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD87'          'Report Status'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD88'          'Rkm speed'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD89'          'Time to Break(Sec)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD90'          'SAP Material Description'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD91'          'SH'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD92'          'SPANDEX%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD93'          'Short Thick(A ~D'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD94'          'Slub Distance (cm)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD95'          'Slub Length (cm)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD96'          'Slub Length Bottom (cm)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD97'          'Slub Length P1 cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD98'          'Slub Length P2 cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD99'          'Slub Length cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD100'          'Slub Length cv%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD101'          'Slub Type'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD102'          'Slub distance P1 cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD103'          'Slub distance P2 cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD104'          'Slub distance bottom cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD105'          'Slub distance cm'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD106'          'Spandec%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD107'          'Spandex%  (8.8)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD108'          'Spandex% (4.0)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD109'          'Spandex% (4.2)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD110'          'Spandex% (Std.:10.3)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD111'          'Spandex% (Std:7.0)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD112'          'Stn.'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD113'          'T 400%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD114'          'Mass (CVm%)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD115'          'No. of Slubs/m'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD116'          'S Outliers'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD117'          'T Outliers'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD118'          'TOTAL'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD119'          'TPM'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD120'          'TPM CV%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD121'          'Tenacity (CV%).1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD122'          'Tenacity (MIN.)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD123'          'Tenacity (MIN.).1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD124'          'Tenacity (cN/tex)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD125'          'Tenacity (cN/tex) Min'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD126'          'Tenacity (cN/tex).1'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD127'          'Tenacity CV%'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD128'          'Tenacity(Rkm)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD129'          'Tenacity(Rkm) CV'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD130'          'Tenacity(in g/den)'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.
      mac 'ZFIELD131'          'updated'              '' '' 'GT_FINAL'   '10' '' 'C610' ''.

    ENDIF.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = gt_fieldcat
      IMPORTING
        ep_table        = new_table1.

    ASSIGN new_table1->* TO <outtab1>.      "This is the required table
    CREATE DATA new_line1 LIKE LINE OF <outtab1>.
    ASSIGN new_line1->* TO <l_line1> .      "Work area forthe same
    ASSIGN new_line1->* TO <l_line_lc1> .   "Work area forthe same


    LOOP AT gt_final INTO gs_final.

      MOVE-CORRESPONDING gs_final TO <l_line_lc1>.

      READ TABLE gt_qamb INTO gs_qamb WITH KEY mblnr = gs_final-cott_mrn1_no
                                               mjahr = gs_final-cott_mrn1_year
                                               zeile = gs_final-cott_mrn1_itm BINARY SEARCH.
      IF sy-subrc = 0.

        LOOP AT gt_qamv02 INTO DATA(gs_qamv02).

          ASSIGN COMPONENT gs_qamv02-verwmerkm OF STRUCTURE <l_line_lc1> TO <ffield1>.
          IF <ffield1> IS ASSIGNED AND sy-subrc = 0.

            READ TABLE gt_qamv01 INTO DATA(gs_qamv01) WITH KEY prueflos  = gs_qamb-prueflos
                                                               verwmerkm = gs_qamv02-verwmerkm BINARY SEARCH.
            IF sy-subrc = 0.
              READ TABLE gt_qamr INTO gs_qamr WITH KEY prueflos  =  gs_qamv01-prueflos
                                                       vorglfnr  =  gs_qamv01-vorglfnr
                                                       merknr    =  gs_qamv01-merknr
                                                       BINARY SEARCH.
              IF sy-subrc = 0.

                ASSIGN COMPONENT 'COTT_MIC' OF STRUCTURE <l_line_lc1> TO <cot_mic>.
                IF sy-subrc = 0.
                  <cot_mic> = gs_qamv01-prueflos.
                ENDIF.
                UNASSIGN <cot_mic>.

                lv_atflv = gs_qamr-mittelwert.
                CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
                  EXPORTING
                    i_number_of_digits       = 2
                    i_fltp_value             = lv_atflv
                    i_value_not_initial_flag = 'X'
                    i_screen_fieldlength     = 16
                  IMPORTING
                    e_char_field             = lv_cvalue.
                CONDENSE lv_cvalue NO-GAPS.
                <ffield1> = lv_cvalue.
              ENDIF.

            ENDIF.

          ENDIF.

          CLEAR : lv_cvalue , lv_atflv.
        ENDLOOP.

      ENDIF.

      APPEND <l_line_lc1> TO <outtab1>.
      CLEAR : <l_line_lc1>.

    ENDLOOP.

  ENDIF.

  IF yrn = '' AND sum = 'X'.
    ASSIGN <outtab1> TO <outtab>.
  ENDIF.

  IF yrn = 'X' AND sum = 'X' AND gt_qals IS NOT INITIAL.

    gt_qals_lc = gt_qals.
    SORT gt_qals_lc ASCENDING BY verwmerkm.
    DELETE ADJACENT DUPLICATES FROM gt_qals_lc COMPARING verwmerkm.

    CLEAR : gs_fieldcat.
    gs_fieldcat-fieldname = 'YARN_MIC'.
    gs_fieldcat-seltext    = 'Yarn Insp.Lot'.
    gs_fieldcat-scrtext_m  = 'Yarn Insp.Lot'.
    gs_fieldcat-datatype   = 'CHAR'.
    gs_fieldcat-outputlen  = '15'.
    gs_fieldcat-emphasize  = 'C610'.
    APPEND gs_fieldcat TO gt_fieldcat.
    LOOP AT gt_qals_lc INTO gs_qals_lc.
      gs_fieldcat-fieldname  = gs_qals_lc-verwmerkm.
      gs_fieldcat-seltext    = gs_qals_lc-kurztext.
      gs_fieldcat-scrtext_m  = gs_qals_lc-kurztext.
      gs_fieldcat-datatype   = 'CHAR'.
      gs_fieldcat-outputlen  = '10'.
      gs_fieldcat-emphasize  = 'C610'.
      APPEND gs_fieldcat TO gt_fieldcat.
      CLEAR : gs_fieldcat.
    ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = gt_fieldcat
      IMPORTING
        ep_table        = new_table.

    ASSIGN new_table->* TO <outtab>.   "This is the required table
    CREATE DATA new_line LIKE LINE OF <outtab>.
    ASSIGN new_line->* TO <l_line> .   "Work area forthe same
    ASSIGN new_line->* TO <l_line_lc> .   "Work area forthe same

    SORT gt_qals  ASCENDING BY werk selmatnr verwmerkm.

    LOOP AT <outtab1> INTO <l_line_lc1>.

      MOVE-CORRESPONDING <l_line_lc1> TO <l_line_lc>.

      LOOP AT gt_qals_lc INTO gs_qals_lc.

        ASSIGN COMPONENT 'YARN_MIC' OF STRUCTURE <l_line_lc> TO <ffield1>.
        IF sy-subrc = 0.
          <ffield1> = gs_qals_lc-prueflos.
        ENDIF.
        UNASSIGN <ffield1>.

        ASSIGN COMPONENT gs_qals_lc-verwmerkm OF STRUCTURE <l_line_lc> TO <ffield>.
        IF <ffield> IS ASSIGNED.
          READ TABLE gt_qals INTO gs_qals WITH KEY werk      = gs_final-werks
                                                   selmatnr  = gs_final-matnr
                                                   verwmerkm = gs_qals_lc-verwmerkm BINARY SEARCH.
          IF sy-subrc = 0.
            lv_atflv = gs_qals-mittelwert.
            CALL FUNCTION 'QSS0_FLTP_TO_CHAR_CONVERSION'
              EXPORTING
                i_number_of_digits       = 2
                i_fltp_value             = lv_atflv
                i_value_not_initial_flag = 'X'
                i_screen_fieldlength     = 16
              IMPORTING
                e_char_field             = lv_cvalue.
            CONDENSE lv_cvalue NO-GAPS.
            <ffield> = lv_cvalue.
          ENDIF.
        ENDIF.
        CLEAR :  lv_cvalue ,lv_atflv.
      ENDLOOP.

      APPEND <l_line_lc> TO <outtab>.
      CLEAR : <l_line_lc>.
    ENDLOOP.


  ENDIF.


  IF sum = 'X' .

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        is_layout_lvc            = ls_layout
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_default                = 'X'
        i_save                   = 'X'
        it_fieldcat_lvc          = gt_fieldcat
      TABLES
        t_outtab                 = <outtab>
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

  ELSE.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program       = sy-repid
        is_layout_lvc            = ls_layout
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        i_default                = 'X'
        i_save                   = 'X'
        it_fieldcat_lvc          = gt_fieldcat
      TABLES
        t_outtab                 = gt_final
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDIF.


ENDFORM.

FORM user_command USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA : e_grid TYPE REF TO cl_gui_alv_grid.
  DATA : ls_chid  TYPE t604n,
         lv_msg   TYPE string,
         ucomm    TYPE sy-ucomm,
         go_ahead TYPE flag.

  ucomm = sy-ucomm.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = e_grid.

  CALL METHOD e_grid->check_changed_data.
  CASE ucomm.
    WHEN 'SEL'.

      e_grid->get_filtered_entries(
      IMPORTING
      et_filtered_entries = DATA(lit_index) ).
      SORT lit_index DESCENDING.

      " Remove excluded rows from buffer
      gs_final-sel = 'X'.
      MODIFY gt_final FROM gs_final TRANSPORTING sel WHERE sel = '' AND icon = ''.
      LOOP AT lit_index ASSIGNING FIELD-SYMBOL(<index>).
        READ TABLE gt_final ASSIGNING FIELD-SYMBOL(<fs001>) INDEX <index>.
        IF sy-subrc EQ 0.
          <fs001>-sel = ''.
        ENDIF.
      ENDLOOP.

      CALL METHOD e_grid->refresh_table_display.

    WHEN 'DSEL'.
      CLEAR : gs_final.
      gs_final-sel = ''.
      MODIFY  gt_final FROM gs_final TRANSPORTING sel WHERE sel = 'X'.
      CALL METHOD e_grid->refresh_table_display.

    WHEN 'POST'.

      LOOP AT gt_final INTO gs_final WHERE sel = 'X' AND icon NE '@0A@'.

*        PERFORM upload_bdc CHANGING gs_final.

        gs_final-sel  = ''.
*        ls_celltab-fieldname = 'SEL'.   " field1
*        ls_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
*        APPEND ls_celltab TO lt_celltab.
*        INSERT LINES OF lt_celltab INTO TABLE gs_final-celltab.
        CLEAR : lt_celltab , ls_celltab.

        MODIFY gt_final FROM gs_final TRANSPORTING sel icon msg.
        CLEAR : gs_final ,  lt_celltab , ls_celltab.
      ENDLOOP.

      CALL METHOD e_grid->refresh_table_display.
  ENDCASE.

ENDFORM.

FORM pf_status USING ut_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM.

FORM goods_mvt_101 USING p_matnr1   TYPE mara-matnr
                         p_werks1   TYPE mseg-werks
                         p_charg1   TYPE mseg-charg
  CHANGING  p_aufnr TYPE mseg-aufnr
            p_ebeln TYPE mseg-ebeln
            p_ebelp TYPE mseg-ebelp
            p_lifnr TYPE mseg-lifnr
            p_werks TYPE mseg-werks
            p_bwtar TYPE mseg-bwtar
            p_qty   TYPE mseg-menge
            p_budat   TYPE sy-datum.

  DATA : lv_delivery_no TYPE lips-vbeln,
         lv_delitem     TYPE lips-posnr.

  CLEAR : gs_mseg,gs_mseg_rev.

  DO .

    SELECT        mblnr,
                  mjahr,
                  zeile,
                  matnr,
                  werks,
                  charg,
                  menge,
                  bwtar,
                  dmbtr,
                  sjahr,
                  smbln,
                  smblp,
                  aufnr,
                  ebeln,
                  ebelp,
                  lifnr,
                  vbeln_im,
                  vbelp_im,
                  budat_mkpf
                  FROM mseg
                  INTO TABLE @DATA(gt_mseg_101)
                  WHERE matnr = @p_matnr1
                  AND   werks = @p_werks1
                  AND   charg = @p_charg1
                 AND   bwart IN ( '101' ).
    IF sy-subrc = 0.

      SELECT        mblnr,
                    mjahr,
                    zeile,
                    matnr,
                    werks,
                    charg,
                    menge,
                    bwtar,
                    dmbtr,
                    sjahr,
                    smbln,
                    smblp,
                    aufnr,
                    ebeln,
                    ebelp,
                    lifnr,
                    vbeln_im,
                    vbelp_im,
                    budat_mkpf
                    FROM mseg
                    INTO TABLE @DATA(gt_mseg_102)
                    FOR ALL ENTRIES IN @gt_mseg_101
                    WHERE smbln = @gt_mseg_101-mblnr
                    AND   sjahr = @gt_mseg_101-mjahr
                    AND   smblp = @gt_mseg_101-zeile.

      SORT gt_mseg_101 ASCENDING BY mblnr mjahr zeile.
      SORT gt_mseg_102 ASCENDING BY smbln sjahr smblp.

      LOOP AT gt_mseg_101 INTO DATA(ls_101).
        READ TABLE gt_mseg_102 INTO DATA(ls_102) WITH KEY smbln = ls_101-mblnr
                                                          sjahr = ls_101-mjahr
                                                          smblp = ls_101-zeile BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE TABLE gt_mseg_101 FROM ls_101.
        ENDIF.
      ENDLOOP.

      IF gt_mseg_101[] IS NOT INITIAL.

        CLEAR : gs_mseg.
        READ TABLE gt_mseg_101 INTO gs_mseg INDEX 1.

        p_aufnr = gs_mseg-aufnr.
        p_bwtar = gs_mseg-bwtar.
        p_qty   = gs_mseg-menge.
        p_budat = gs_mseg-budat_mkpf.

        IF p_aufnr IS NOT INITIAL.
          EXIT.
        ELSE.
          IF gs_mseg-vbeln_im IS NOT INITIAL AND gs_mseg-vbelp_im IS NOT INITIAL .

            lv_delivery_no  = gs_mseg-vbeln_im.
            lv_delitem      = gs_mseg-vbelp_im.

            PERFORM find_po USING p_matnr1
                                  p_charg1
                                  lv_delivery_no
                                  lv_delitem
                             CHANGING  p_aufnr
                                       p_ebeln
                                       p_ebelp
                                       p_lifnr
                                       p_werks.
            EXIT.
          ELSE.
            p_ebeln  = gs_mseg-ebeln.
            p_ebelp  = gs_mseg-ebelp.
            SELECT SINGLE lifnr FROM ekko INTO p_lifnr WHERE ebeln = p_ebeln.
            IF sy-subrc = 0.
              SELECT SINGLE werks FROM ekpo INTO p_werks WHERE ebeln = p_ebeln AND ebelp = p_ebelp.
            ENDIF.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.

FORM find_po USING p_matnr   TYPE mara-matnr
                   p_charg   TYPE mseg-charg
                   p_del_no  TYPE lips-vbeln
                   p_del_itm TYPE lips-posnr
  CHANGING  p_aufnr TYPE mseg-aufnr
            p_ebeln TYPE mseg-ebeln
            p_ebelp TYPE mseg-ebelp
            p_lifnr TYPE mseg-lifnr
            p_werks TYPE mseg-werks.

  TYPES: BEGIN OF ty_lips,
           vbeln TYPE lips-vbeln,
           posnr TYPE lips-posnr,
           matnr TYPE lips-matnr,
           werks TYPE lips-werks,
           lgort TYPE lips-lgort,
           charg TYPE lips-charg,
         END OF ty_lips.

  DATA : gt_lips TYPE STANDARD TABLE OF ty_lips,
         gs_lips TYPE ty_lips.

  CLEAR : gs_mseg, gt_lips,gs_lips ,gs_mseg_rev.


  DO .

    SELECT SINGLE  vbeln
                   posnr
                   matnr
                   werks
                   lgort
                   charg
                   FROM lips
                   INTO gs_lips
                   WHERE vbeln = p_del_no
                   AND   posnr = p_del_itm
                   AND   charg = p_charg.
    IF sy-subrc = 0.

      SELECT        mblnr,
                    mjahr,
                    zeile,
                    matnr,
                    werks,
                    charg,
                    menge,
                    bwtar,
                    dmbtr,
                    sjahr,
                    smbln,
                    smblp,
                    aufnr,
                    ebeln,
                    ebelp,
                    lifnr,
                    vbeln_im,
                    vbelp_im,
                    budat_mkpf
                    FROM mseg
                    INTO TABLE @DATA(gt_mseg_101)
                    WHERE matnr = @gs_lips-matnr
                    AND   werks = @gs_lips-werks
                    AND   charg = @gs_lips-charg
                    AND   bwart IN ( '101' ).
      IF sy-subrc = 0.

        SELECT        mblnr,
                      mjahr,
                      zeile,
                      matnr,
                      werks,
                      charg,
                      menge,
                      bwtar,
                      dmbtr,
                      sjahr,
                      smbln,
                      smblp,
                      aufnr,
                      ebeln,
                      ebelp,
                      lifnr,
                      vbeln_im,
                      vbelp_im,
                      budat_mkpf
                      FROM mseg
                      INTO TABLE @DATA(gt_mseg_102)
                      FOR ALL ENTRIES IN @gt_mseg_101
                      WHERE smbln = @gt_mseg_101-mblnr
                      AND   sjahr = @gt_mseg_101-mjahr
                      AND   smblp = @gt_mseg_101-zeile.

        SORT gt_mseg_101 ASCENDING BY mblnr mjahr zeile.
        SORT gt_mseg_102 ASCENDING BY smbln sjahr smblp.

        LOOP AT gt_mseg_101 INTO DATA(ls_101).
          READ TABLE gt_mseg_102 INTO DATA(ls_102) WITH KEY smbln = ls_101-mblnr
                                                            sjahr = ls_101-mjahr
                                                            smblp = ls_101-zeile BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE TABLE gt_mseg_101 FROM ls_101.
          ENDIF.
        ENDLOOP.

        IF gt_mseg_101 IS NOT INITIAL.

          CLEAR : gs_mseg.
          READ TABLE gt_mseg_101 INTO gs_mseg INDEX 1.

          p_aufnr = gs_mseg-aufnr.

          IF p_aufnr IS NOT INITIAL.
            EXIT.
          ELSE.
            IF gs_mseg-vbeln_im IS NOT INITIAL AND gs_mseg-vbelp_im IS NOT INITIAL .
              p_del_no  = gs_mseg-vbeln_im.
              p_del_itm = gs_mseg-vbelp_im.
            ELSE.
              p_ebeln  = gs_mseg-ebeln.
              p_ebelp  = gs_mseg-ebelp.
              SELECT SINGLE lifnr FROM ekko INTO p_lifnr WHERE ebeln = p_ebeln.
              IF sy-subrc = 0.
                SELECT SINGLE werks FROM ekpo INTO p_werks WHERE ebeln = p_ebeln AND ebelp = p_ebelp.
              ENDIF.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.

      ELSE.
        EXIT.
      ENDIF.

    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.


FORM goods_mvt_cott USING p_matnr1   TYPE mara-matnr
                          p_werks1   TYPE mseg-werks
                          p_charg1   TYPE mseg-charg
  CHANGING  p_aufnr TYPE mseg-aufnr
            p_ebeln TYPE mseg-ebeln
            p_ebelp TYPE mseg-ebelp
            p_lifnr TYPE mseg-lifnr
            p_name1 TYPE lfa1-name1
            p_qty   TYPE mseg-menge
            p_budat   TYPE sy-datum
            p_docno   TYPE mseg-mblnr
            p_docyear   TYPE mseg-mjahr
            p_docitm   TYPE mseg-zeile.

  DATA : lv_delivery_no TYPE lips-vbeln,
         lv_delitem     TYPE lips-posnr.

  CLEAR : gs_mseg,gs_mseg_rev.


  SELECT        mblnr,
                mjahr,
                zeile,
                matnr,
                werks,
                charg,
                menge,
                bwtar,
                dmbtr,
                sjahr,
                smbln,
                smblp,
                aufnr,
                ebeln,
                ebelp,
                lifnr,
                vbeln_im,
                vbelp_im,
                budat_mkpf
                FROM mseg
                INTO TABLE @DATA(gt_mseg_101)
                WHERE matnr = @p_matnr1
                AND   werks = @p_werks1
                AND   charg = @p_charg1
                AND   bwart = '101'.
  IF sy-subrc = 0.

    SELECT        mblnr,
                  mjahr,
                  zeile,
                  matnr,
                  werks,
                  charg,
                  menge,
                  bwtar,
                  dmbtr,
                  sjahr,
                  smbln,
                  smblp,
                  aufnr,
                  ebeln,
                  ebelp,
                  lifnr,
                  vbeln_im,
                  vbelp_im,
                  budat_mkpf
                  FROM mseg
                  INTO TABLE @DATA(gt_mseg_102)
                  FOR ALL ENTRIES IN @gt_mseg_101
                  WHERE smbln = @gt_mseg_101-mblnr
                  AND   sjahr = @gt_mseg_101-mjahr
                  AND   smblp = @gt_mseg_101-zeile.

    SORT gt_mseg_101 ASCENDING BY mblnr mjahr zeile.
    SORT gt_mseg_102 ASCENDING BY smbln sjahr smblp.

    LOOP AT gt_mseg_101 INTO DATA(ls_101).
      READ TABLE gt_mseg_102 INTO DATA(ls_102) WITH KEY smbln = ls_101-mblnr
                                                        sjahr = ls_101-mjahr
                                                        smblp = ls_101-zeile BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE TABLE gt_mseg_101 FROM ls_101.
      ENDIF.
    ENDLOOP.

    IF gt_mseg_101[] IS NOT INITIAL.

      CLEAR : gs_mseg.
      SORT gt_mseg_101 ASCENDING BY mblnr mjahr zeile vbeln_im.
      READ TABLE gt_mseg_101 INTO gs_mseg INDEX 1.
      IF sy-subrc = 0.

        IF gs_mseg-vbeln_im IS INITIAL.
          p_aufnr = gs_mseg-aufnr.
          p_qty   = gs_mseg-menge.
          p_budat = gs_mseg-budat_mkpf.
          p_docno   = gs_mseg-mblnr.
          p_docyear = gs_mseg-mjahr.
          p_docitm  = gs_mseg-zeile.

          p_lifnr = gs_mseg-lifnr.
          IF p_lifnr IS NOT INITIAL.
            p_ebeln = gs_mseg-ebeln.
            p_ebelp = gs_mseg-ebelp.
            SELECT SINGLE name1 FROM lfa1 INTO p_name1 WHERE lifnr = p_lifnr.
          ENDIF.
        ELSE.

          lv_delivery_no  = gs_mseg-vbeln_im.
          lv_delitem      = gs_mseg-vbelp_im.

          PERFORM find_vendor USING p_matnr1
                                    p_charg1
                                    lv_delivery_no
                                    lv_delitem
                           CHANGING  p_aufnr
                                     p_ebeln
                                     p_ebelp
                                     p_lifnr
                                     p_name1
                                     p_qty
                                     p_budat
                                     p_docno
                                     p_docyear
                                     p_docitm.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM find_vendor USING p_matnr   TYPE mara-matnr
                       p_charg   TYPE mseg-charg
                       p_del_no  TYPE lips-vbeln
                       p_del_itm TYPE lips-posnr
  CHANGING  p_aufnr TYPE mseg-aufnr
            p_ebeln TYPE mseg-ebeln
            p_ebelp TYPE mseg-ebelp
            p_lifnr TYPE mseg-lifnr
            p_name1 TYPE lfa1-name1
            p_qty   TYPE mseg-menge
            p_budat   TYPE sy-datum
            p_docno  TYPE mseg-mblnr
            p_docyear TYPE mseg-mjahr
            p_docitm TYPE mseg-zeile.

  TYPES: BEGIN OF ty_lips,
           vbeln TYPE lips-vbeln,
           posnr TYPE lips-posnr,
           matnr TYPE lips-matnr,
           werks TYPE lips-werks,
           lgort TYPE lips-lgort,
           charg TYPE lips-charg,
         END OF ty_lips.

  DATA : gt_lips TYPE STANDARD TABLE OF ty_lips,
         gs_lips TYPE ty_lips.

  CLEAR : gs_mseg, gt_lips,gs_lips ,gs_mseg_rev.


  DO .

    SELECT SINGLE  vbeln
                   posnr
                   matnr
                   werks
                   lgort
                   charg
                   FROM lips
                   INTO gs_lips
                   WHERE vbeln = p_del_no
                   AND   posnr = p_del_itm
                   AND   charg = p_charg.
    IF sy-subrc = 0.

      SELECT        mblnr,
                    mjahr,
                    zeile,
                    matnr,
                    werks,
                    charg,
                    menge,
                    bwtar,
                    dmbtr,
                    sjahr,
                    smbln,
                    smblp,
                    aufnr,
                    ebeln,
                    ebelp,
                    lifnr,
                    vbeln_im,
                    vbelp_im,
                    budat_mkpf
                    FROM mseg
                    INTO TABLE @DATA(gt_mseg_101)
                    WHERE matnr = @gs_lips-matnr
                    AND   werks = @gs_lips-werks
                    AND   charg = @gs_lips-charg
                    AND   bwart = '101'.
      IF sy-subrc = 0.

        SELECT        mblnr,
                      mjahr,
                      zeile,
                      matnr,
                      werks,
                      charg,
                      menge,
                      bwtar,
                      dmbtr,
                      sjahr,
                      smbln,
                      smblp,
                      aufnr,
                      ebeln,
                      ebelp,
                      lifnr,
                      vbeln_im,
                      vbelp_im,
                      budat_mkpf
                      FROM mseg
                      INTO TABLE @DATA(gt_mseg_102)
                      FOR ALL ENTRIES IN @gt_mseg_101
                      WHERE smbln = @gt_mseg_101-mblnr
                      AND   sjahr = @gt_mseg_101-mjahr
                      AND   smblp = @gt_mseg_101-zeile.

        SORT gt_mseg_101 ASCENDING BY mblnr mjahr zeile.
        SORT gt_mseg_102 ASCENDING BY smbln sjahr smblp.

        LOOP AT gt_mseg_101 INTO DATA(ls_101).
          READ TABLE gt_mseg_102 INTO DATA(ls_102) WITH KEY smbln = ls_101-mblnr
                                                            sjahr = ls_101-mjahr
                                                            smblp = ls_101-zeile BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE TABLE gt_mseg_101 FROM ls_101.
          ENDIF.
        ENDLOOP.

        IF gt_mseg_101 IS NOT INITIAL.

          CLEAR : gs_mseg.
          READ TABLE gt_mseg_101 INTO gs_mseg INDEX 1.

          IF gs_mseg-vbeln_im IS INITIAL.
            p_aufnr = gs_mseg-aufnr.
            p_qty   = gs_mseg-menge.
            p_budat = gs_mseg-budat_mkpf.
            p_docno = gs_mseg-mblnr.
            p_docyear = gs_mseg-mjahr.
            p_docitm  = gs_mseg-zeile.
            p_lifnr = gs_mseg-lifnr.
            p_ebeln = gs_mseg-ebeln.
            p_ebelp = gs_mseg-ebelp.
            IF p_lifnr IS NOT INITIAL.
              SELECT SINGLE name1 FROM lfa1 INTO p_name1 WHERE lifnr = p_lifnr.
            ENDIF.
            EXIT.
          ELSE.

            p_del_no  = gs_mseg-vbeln_im.
            p_del_itm = gs_mseg-vbelp_im.

          ENDIF.
        ELSE.
          EXIT.
        ENDIF.

      ELSE.
        EXIT.
      ENDIF.

    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.


FORM upload_file.
  p_exc = 'X'.
  IF p_exc = 'X'.

    IF p_file IS NOT INITIAL.

      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
        EXPORTING
          filename                = p_file
          i_begin_col             = l_start_col
          i_begin_row             = l_start_row
          i_end_col               = l_end_col
          i_end_row               = l_end_row
        TABLES
          intern                  = l_intern
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.

      REPLACE ALL OCCURRENCES OF ',' IN l_intern-value WITH space.
      CONDENSE l_intern-value NO-GAPS.

      LOOP AT l_intern.

        CASE l_intern-col.
          WHEN 1.
            IF l_intern-value IS NOT INITIAL.
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                EXPORTING
                  date_external            = l_intern-value
                IMPORTING
                  date_internal            = gs_final-testing_dt
                EXCEPTIONS
                  date_external_is_invalid = 1
                  OTHERS                   = 2.
            ENDIF.
          WHEN 2.
            gs_final-werks          = l_intern-value.
          WHEN 3.
            gs_final-matnr          = l_intern-value.
            gs_final-matnr_50       = gs_final-matnr.
            CONDENSE gs_final-matnr NO-GAPS.
            CONDENSE gs_final-matnr_50 NO-GAPS.
          WHEN 4.
            gs_final-lot_no         = l_intern-value.
          WHEN 5.
            gs_final-lab_no         = l_intern-value.
          WHEN 6.
            gs_final-inv_no         = l_intern-value.
          WHEN 7.
            gs_final-inv_qty        = l_intern-value.
          WHEN 8.
            gs_final-thin           = l_intern-value.
          WHEN 9.
            gs_final-thik           = l_intern-value.
          WHEN 10.
            gs_final-neps           = l_intern-value.
          WHEN 11.
            gs_final-hairiness      = l_intern-value.
          WHEN 12.
            gs_final-s3             = l_intern-value.
          WHEN 13.
            gs_final-b_force        = l_intern-value.
          WHEN 14.
            gs_final-elong_per      = l_intern-value.
          WHEN 15.
            gs_final-elong_per_p    = l_intern-value.
          WHEN 16.
            gs_final-rkm            = l_intern-value.
          WHEN 17.
            gs_final-rkm_01         = l_intern-value.
          WHEN 18.
            gs_final-rkm_cv_per     = l_intern-value.
          WHEN 19.
            gs_final-b_work         = l_intern-value.
          WHEN 20.
            gs_final-b_work_01      = l_intern-value.
          WHEN 21.
            gs_final-mean_count     = l_intern-value.
          WHEN 22.
            gs_final-count_cv_per   = l_intern-value.
          WHEN 23.
            gs_final-strength_cv_per = l_intern-value.
          WHEN 24.
            gs_final-csp            = l_intern-value.
          WHEN 25.
            gs_final-tpi            = l_intern-value.
          WHEN 26.
            gs_final-tm             = l_intern-value.
          WHEN 27.
            gs_final-a1a2b1         = l_intern-value.
          WHEN 28.
            gs_final-a3tod2         = l_intern-value.
          WHEN 29.
            gs_final-obj_faults     = l_intern-value.
          WHEN 30.
            gs_final-efg            = l_intern-value.
          WHEN 31.
            gs_final-h1             = l_intern-value.
          WHEN 32.
            gs_final-h2             = l_intern-value.
          WHEN 33.
            gs_final-total_classimat = l_intern-value.
          WHEN 34.
            gs_final-fd               = l_intern-value.
          WHEN 35.
            gs_final-pp_lt_10         = l_intern-value.
          WHEN 36.
            gs_final-remarks          = l_intern-value.
          WHEN 37.
            gs_final-approval         = l_intern-value.
          WHEN 38.
            gs_final-feedback         = l_intern-value.
          WHEN 39.
            gs_final-ucv              = l_intern-value.
          WHEN 40.
            gs_final-total_imp        = l_intern-value.
          WHEN 41.
            gs_final-hcv              = l_intern-value.
          WHEN 42.
            gs_final-b_force1         = l_intern-value.
          WHEN 43.
            gs_final-rkm_cv_per1      = l_intern-value.
          WHEN 44.
            gs_final-pp_gt_10         = l_intern-value.
          WHEN 45.
            gs_final-pp_lt_101        = l_intern-value.
          WHEN 46.
            gs_final-supplier         = l_intern-value.
          WHEN 47.
            gs_final-tpi_27           = l_intern-value.
          WHEN 48.
            gs_final-tm_4             = l_intern-value.
          WHEN 49.
            gs_final-tpi_37           = l_intern-value.
          WHEN 50.
            gs_final-classimat        = l_intern-value.

          WHEN 51.  gs_final-zfield1         = l_intern-value. CONDENSE gs_final-zfield1 NO-GAPS.
          WHEN 52.  gs_final-zfield2         = l_intern-value. CONDENSE gs_final-zfield2 NO-GAPS.
          WHEN 53.  gs_final-zfield3         = l_intern-value. CONDENSE gs_final-zfield3 NO-GAPS.
          WHEN 54.  gs_final-zfield4         = l_intern-value. CONDENSE gs_final-zfield4 NO-GAPS.
          WHEN 55.  gs_final-zfield5         = l_intern-value. CONDENSE gs_final-zfield5 NO-GAPS.
          WHEN 56.  gs_final-zfield6         = l_intern-value. CONDENSE gs_final-zfield6 NO-GAPS.
          WHEN 57.  gs_final-zfield7         = l_intern-value. CONDENSE gs_final-zfield7 NO-GAPS.
          WHEN 58.  gs_final-zfield8         = l_intern-value. CONDENSE gs_final-zfield8 NO-GAPS.
          WHEN 59.  gs_final-zfield9         = l_intern-value. CONDENSE gs_final-zfield9 NO-GAPS.
          WHEN 60.  gs_final-zfield10        = l_intern-value. CONDENSE gs_final-zfield10 NO-GAPS.
          WHEN 61.  gs_final-zfield11        = l_intern-value. CONDENSE gs_final-zfield11 NO-GAPS.
          WHEN 62.  gs_final-zfield12         = l_intern-value. CONDENSE gs_final-zfield12 NO-GAPS.
          WHEN 63.  gs_final-zfield13         = l_intern-value. CONDENSE gs_final-zfield13 NO-GAPS.
          WHEN 64.  gs_final-zfield14         = l_intern-value. CONDENSE gs_final-zfield14 NO-GAPS.
          WHEN 65.  gs_final-zfield15         = l_intern-value. CONDENSE gs_final-zfield15 NO-GAPS.
          WHEN 66.  gs_final-zfield16         = l_intern-value. CONDENSE gs_final-zfield16 NO-GAPS.
          WHEN 67.  gs_final-zfield17         = l_intern-value. CONDENSE gs_final-zfield17 NO-GAPS.
          WHEN 68.  gs_final-zfield18         = l_intern-value. CONDENSE gs_final-zfield18 NO-GAPS.
          WHEN 69.  gs_final-zfield19         = l_intern-value. CONDENSE gs_final-zfield19 NO-GAPS.
          WHEN 70.  gs_final-zfield20         = l_intern-value. CONDENSE gs_final-zfield20 NO-GAPS.
          WHEN 71.  gs_final-zfield21         = l_intern-value. CONDENSE gs_final-zfield21 NO-GAPS.
          WHEN 72.  gs_final-zfield22         = l_intern-value. CONDENSE gs_final-zfield22 NO-GAPS.
          WHEN 73.  gs_final-zfield23         = l_intern-value. CONDENSE gs_final-zfield23 NO-GAPS.
          WHEN 74.  gs_final-zfield24         = l_intern-value. CONDENSE gs_final-zfield24 NO-GAPS.
          WHEN 75.  gs_final-zfield25         = l_intern-value. CONDENSE gs_final-zfield25 NO-GAPS.
          WHEN 76.  gs_final-zfield26         = l_intern-value. CONDENSE gs_final-zfield26 NO-GAPS.
          WHEN 77.  gs_final-zfield27         = l_intern-value. CONDENSE gs_final-zfield27 NO-GAPS.
          WHEN 78.  gs_final-zfield28         = l_intern-value. CONDENSE gs_final-zfield28 NO-GAPS.
          WHEN 79.  gs_final-zfield29         = l_intern-value. CONDENSE gs_final-zfield29 NO-GAPS.
          WHEN 80.  gs_final-zfield30         = l_intern-value. CONDENSE gs_final-zfield30 NO-GAPS.
          WHEN 81.  gs_final-zfield31        = l_intern-value. CONDENSE gs_final-zfield31 NO-GAPS.
          WHEN 82.  gs_final-zfield32         = l_intern-value. CONDENSE gs_final-zfield32 NO-GAPS.
          WHEN 83.  gs_final-zfield33        = l_intern-value. CONDENSE gs_final-zfield33 NO-GAPS.
          WHEN 84.  gs_final-zfield34         = l_intern-value. CONDENSE gs_final-zfield34 NO-GAPS.
          WHEN 85.  gs_final-zfield35         = l_intern-value. CONDENSE gs_final-zfield35 NO-GAPS.
          WHEN 86.  gs_final-zfield36         = l_intern-value. CONDENSE gs_final-zfield36 NO-GAPS.
          WHEN 87.  gs_final-zfield37         = l_intern-value. CONDENSE gs_final-zfield37 NO-GAPS.
          WHEN 88.  gs_final-zfield38         = l_intern-value. CONDENSE gs_final-zfield38 NO-GAPS.
          WHEN 89.  gs_final-zfield39         = l_intern-value. CONDENSE gs_final-zfield39 NO-GAPS.
          WHEN 90.  gs_final-zfield40         = l_intern-value. CONDENSE gs_final-zfield40 NO-GAPS.
          WHEN 91.  gs_final-zfield41         = l_intern-value. CONDENSE gs_final-zfield41 NO-GAPS.
          WHEN 92.  gs_final-zfield42         = l_intern-value. CONDENSE gs_final-zfield42 NO-GAPS.
          WHEN 93.  gs_final-zfield43         = l_intern-value. CONDENSE gs_final-zfield43 NO-GAPS.
          WHEN 94.  gs_final-zfield44         = l_intern-value. CONDENSE gs_final-zfield44 NO-GAPS.
          WHEN 95.  gs_final-zfield45         = l_intern-value. CONDENSE gs_final-zfield45 NO-GAPS.
          WHEN 96.  gs_final-zfield46         = l_intern-value. CONDENSE gs_final-zfield46 NO-GAPS.
          WHEN 97.  gs_final-zfield47         = l_intern-value. CONDENSE gs_final-zfield47 NO-GAPS.
          WHEN 98.  gs_final-zfield48         = l_intern-value. CONDENSE gs_final-zfield48 NO-GAPS.
          WHEN 99.  gs_final-zfield49         = l_intern-value. CONDENSE gs_final-zfield49 NO-GAPS.
          WHEN 100.  gs_final-zfield50         = l_intern-value. CONDENSE gs_final-zfield50 NO-GAPS.
          WHEN 101.  gs_final-zfield51         = l_intern-value. CONDENSE gs_final-zfield51 NO-GAPS.
          WHEN 102.  gs_final-zfield52         = l_intern-value. CONDENSE gs_final-zfield52 NO-GAPS.
          WHEN 103.  gs_final-zfield53         = l_intern-value. CONDENSE gs_final-zfield53 NO-GAPS.
          WHEN 104.  gs_final-zfield54         = l_intern-value. CONDENSE gs_final-zfield54 NO-GAPS.
          WHEN 105.  gs_final-zfield55         = l_intern-value. CONDENSE gs_final-zfield55 NO-GAPS.
          WHEN 106.  gs_final-zfield56         = l_intern-value. CONDENSE gs_final-zfield56 NO-GAPS.
          WHEN 107.  gs_final-zfield57         = l_intern-value. CONDENSE gs_final-zfield57 NO-GAPS.
          WHEN 108.  gs_final-zfield58         = l_intern-value. CONDENSE gs_final-zfield58 NO-GAPS.
          WHEN 109.  gs_final-zfield59         = l_intern-value. CONDENSE gs_final-zfield59 NO-GAPS.
          WHEN 110.  gs_final-zfield60         = l_intern-value. CONDENSE gs_final-zfield60 NO-GAPS.
          WHEN 111.  gs_final-zfield61         = l_intern-value. CONDENSE gs_final-zfield61 NO-GAPS.
          WHEN 112.  gs_final-zfield62         = l_intern-value. CONDENSE gs_final-zfield62 NO-GAPS.
          WHEN 113.  gs_final-zfield63         = l_intern-value. CONDENSE gs_final-zfield63 NO-GAPS.
          WHEN 114.  gs_final-zfield64         = l_intern-value. CONDENSE gs_final-zfield64 NO-GAPS.
          WHEN 115.  gs_final-zfield65         = l_intern-value. CONDENSE gs_final-zfield65 NO-GAPS.
          WHEN 116.  gs_final-zfield66         = l_intern-value. CONDENSE gs_final-zfield66 NO-GAPS.
          WHEN 117.  gs_final-zfield67         = l_intern-value. CONDENSE gs_final-zfield67 NO-GAPS.
          WHEN 118.  gs_final-zfield68         = l_intern-value. CONDENSE gs_final-zfield68 NO-GAPS.
          WHEN 119.  gs_final-zfield69         = l_intern-value. CONDENSE gs_final-zfield69 NO-GAPS.
          WHEN 120.  gs_final-zfield70         = l_intern-value. CONDENSE gs_final-zfield70 NO-GAPS.
          WHEN 121.  gs_final-zfield71         = l_intern-value. CONDENSE gs_final-zfield71 NO-GAPS.
          WHEN 122.  gs_final-zfield72         = l_intern-value. CONDENSE gs_final-zfield72 NO-GAPS.
          WHEN 123.  gs_final-zfield73         = l_intern-value. CONDENSE gs_final-zfield73 NO-GAPS.
          WHEN 124.  gs_final-zfield74         = l_intern-value. CONDENSE gs_final-zfield74 NO-GAPS.
          WHEN 125.  gs_final-zfield75         = l_intern-value. CONDENSE gs_final-zfield75 NO-GAPS.
          WHEN 126.  gs_final-zfield76         = l_intern-value. CONDENSE gs_final-zfield76 NO-GAPS.
          WHEN 127.  gs_final-zfield77         = l_intern-value. CONDENSE gs_final-zfield77 NO-GAPS.
          WHEN 128.  gs_final-zfield78         = l_intern-value. CONDENSE gs_final-zfield78 NO-GAPS.
          WHEN 129.  gs_final-zfield79         = l_intern-value. CONDENSE gs_final-zfield79 NO-GAPS.
          WHEN 130.  gs_final-zfield80         = l_intern-value. CONDENSE gs_final-zfield80 NO-GAPS.
          WHEN 131.  gs_final-zfield81         = l_intern-value. CONDENSE gs_final-zfield81 NO-GAPS.
          WHEN 132.  gs_final-zfield82         = l_intern-value. CONDENSE gs_final-zfield82 NO-GAPS.
          WHEN 133.  gs_final-zfield83         = l_intern-value. CONDENSE gs_final-zfield83 NO-GAPS.
          WHEN 134.  gs_final-zfield84         = l_intern-value. CONDENSE gs_final-zfield84 NO-GAPS.
          WHEN 135.  gs_final-zfield85         = l_intern-value. CONDENSE gs_final-zfield85 NO-GAPS.
          WHEN 136.  gs_final-zfield86         = l_intern-value. CONDENSE gs_final-zfield86 NO-GAPS.
          WHEN 137.  gs_final-zfield87         = l_intern-value. CONDENSE gs_final-zfield87 NO-GAPS.
          WHEN 138.  gs_final-zfield88         = l_intern-value. CONDENSE gs_final-zfield88 NO-GAPS.
          WHEN 139.  gs_final-zfield89         = l_intern-value. CONDENSE gs_final-zfield89 NO-GAPS.
          WHEN 140.  gs_final-zfield90         = l_intern-value. CONDENSE gs_final-zfield90 NO-GAPS.
          WHEN 141.  gs_final-zfield91         = l_intern-value. CONDENSE gs_final-zfield91 NO-GAPS.
          WHEN 142.  gs_final-zfield92         = l_intern-value. CONDENSE gs_final-zfield92 NO-GAPS.
          WHEN 143.  gs_final-zfield93         = l_intern-value. CONDENSE gs_final-zfield93 NO-GAPS.
          WHEN 144.  gs_final-zfield94         = l_intern-value. CONDENSE gs_final-zfield94 NO-GAPS.
          WHEN 145.  gs_final-zfield95         = l_intern-value. CONDENSE gs_final-zfield95 NO-GAPS.
          WHEN 146.  gs_final-zfield96         = l_intern-value. CONDENSE gs_final-zfield96 NO-GAPS.
          WHEN 147.  gs_final-zfield97         = l_intern-value. CONDENSE gs_final-zfield97 NO-GAPS.
          WHEN 148.  gs_final-zfield98         = l_intern-value. CONDENSE gs_final-zfield98 NO-GAPS.
          WHEN 149.  gs_final-zfield99         = l_intern-value. CONDENSE gs_final-zfield99 NO-GAPS.
          WHEN 150.  gs_final-zfield100         = l_intern-value. CONDENSE gs_final-zfield100 NO-GAPS.
          WHEN 151.  gs_final-zfield101         = l_intern-value. CONDENSE gs_final-zfield101 NO-GAPS.
          WHEN 152.  gs_final-zfield102         = l_intern-value. CONDENSE gs_final-zfield102 NO-GAPS.
          WHEN 153.  gs_final-zfield103         = l_intern-value. CONDENSE gs_final-zfield103 NO-GAPS.
          WHEN 154.  gs_final-zfield104         = l_intern-value. CONDENSE gs_final-zfield104 NO-GAPS.
          WHEN 155.  gs_final-zfield105         = l_intern-value. CONDENSE gs_final-zfield105 NO-GAPS.
          WHEN 156.  gs_final-zfield106         = l_intern-value. CONDENSE gs_final-zfield106 NO-GAPS.
          WHEN 157.  gs_final-zfield107         = l_intern-value. CONDENSE gs_final-zfield107 NO-GAPS.
          WHEN 158.  gs_final-zfield108         = l_intern-value. CONDENSE gs_final-zfield108 NO-GAPS.
          WHEN 159.  gs_final-zfield109         = l_intern-value. CONDENSE gs_final-zfield109 NO-GAPS.
          WHEN 160.  gs_final-zfield110         = l_intern-value. CONDENSE gs_final-zfield110 NO-GAPS.
          WHEN 161.  gs_final-zfield111         = l_intern-value. CONDENSE gs_final-zfield111 NO-GAPS.
          WHEN 162.  gs_final-zfield112         = l_intern-value. CONDENSE gs_final-zfield112 NO-GAPS.
          WHEN 163.  gs_final-zfield113         = l_intern-value. CONDENSE gs_final-zfield113 NO-GAPS.
          WHEN 164.  gs_final-zfield114         = l_intern-value. CONDENSE gs_final-zfield114 NO-GAPS.
          WHEN 165.  gs_final-zfield115         = l_intern-value. CONDENSE gs_final-zfield115 NO-GAPS.
          WHEN 166.  gs_final-zfield116         = l_intern-value. CONDENSE gs_final-zfield116 NO-GAPS.
          WHEN 167.  gs_final-zfield117         = l_intern-value. CONDENSE gs_final-zfield117 NO-GAPS.
          WHEN 168.  gs_final-zfield118         = l_intern-value. CONDENSE gs_final-zfield118 NO-GAPS.
          WHEN 169.  gs_final-zfield119         = l_intern-value. CONDENSE gs_final-zfield119 NO-GAPS.
          WHEN 170.  gs_final-zfield120         = l_intern-value. CONDENSE gs_final-zfield120 NO-GAPS.
          WHEN 171.  gs_final-zfield121         = l_intern-value. CONDENSE gs_final-zfield121 NO-GAPS.
          WHEN 172.  gs_final-zfield122         = l_intern-value. CONDENSE gs_final-zfield122 NO-GAPS.
          WHEN 173.  gs_final-zfield123         = l_intern-value. CONDENSE gs_final-zfield123 NO-GAPS.
          WHEN 174.  gs_final-zfield124         = l_intern-value. CONDENSE gs_final-zfield124 NO-GAPS.
          WHEN 175.  gs_final-zfield125         = l_intern-value. CONDENSE gs_final-zfield125 NO-GAPS.
          WHEN 176.  gs_final-zfield126         = l_intern-value. CONDENSE gs_final-zfield126 NO-GAPS.
          WHEN 177.  gs_final-zfield127         = l_intern-value. CONDENSE gs_final-zfield127 NO-GAPS.
          WHEN 178.  gs_final-zfield128         = l_intern-value. CONDENSE gs_final-zfield128 NO-GAPS.
          WHEN 179.  gs_final-zfield129         = l_intern-value. CONDENSE gs_final-zfield129 NO-GAPS.
          WHEN 180.  gs_final-zfield130         = l_intern-value. CONDENSE gs_final-zfield130 NO-GAPS.
          WHEN 181.  gs_final-zfield131         = l_intern-value. CONDENSE gs_final-zfield131 NO-GAPS.
        ENDCASE.

        AT END OF row.

          CONCATENATE gs_final-werks gs_final-matnr gs_final-lot_no gs_final-testing_dt INTO gs_final-keycomb.
          CONDENSE gs_final-keycomb NO-GAPS.
          APPEND gs_final TO gt_final.
          CLEAR: gs_final.
        ENDAT.

      ENDLOOP.

    ENDIF.

  ELSEIF p_csv = 'X'.

    DATA: p_sep TYPE c VALUE ',',
          p_hed TYPE kcd_ex_row_n VALUE '1',
          p_col TYPE kcd_ex_col_n VALUE '1'.

    IF p_file IS NOT INITIAL.
      CALL METHOD zcl_csv_upload=>extract_data_tab
        EXPORTING
          i_filename     = p_file
          i_separator    = p_sep
          i_header_del   = p_hed
          i_start_column = p_col
          i_method       = '1'
          i_other        = ''
        CHANGING
          it_data        = gt_final.

    ENDIF.

  ENDIF.

ENDFORM.

FORM input.

  SELECT qals~prueflos,
         qals~werk,
         qals~selmatnr,
         qamr~vorglfnr,
         qamr~merknr,
         qamr~mittelwert,
         qamv~verwmerkm,
         qamv~kurztext
    FROM qals
    INNER JOIN qamr ON qals~prueflos = qamr~prueflos
    INNER JOIN qamv ON qamr~prueflos = qamv~prueflos
                   AND qamr~vorglfnr = qamv~vorglfnr
                   AND qamr~merknr   = qamv~merknr
    INNER JOIN mara ON qals~selmatnr = mara~matnr
    INTO TABLE @DATA(gt_qals)
    WHERE qals~werk     IN @s_werks
      AND qals~selmatnr IN @s_yarn
      AND qals~budat    =  @test_dt
      AND mara~matkl    =  'YGR'.
*  IF sy-subrc = 0.
*    SELECT matnr
*           mtart
*           matkl
*           FROM mara
*           INTO TABLE gt_mara
*           FOR ALL ENTRIES IN gt_qals
*           WHERE matnr = gt_qals-selmatnr
*           AND   matkl IN ( 'YGR' ).
*    SORT gt_mara ASCENDING BY matnr.
*    LOOP AT gt_qals INTO gs_qals.
*      READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_qals-selmatnr BINARY SEARCH.
*      IF sy-subrc NE 0.
*        DELETE TABLE gt_qals FROM gs_qals.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF gt_qals IS NOT INITIAL.
    DATA(gt_qals_temp) = gt_qals.
    SORT gt_qals_temp BY werk selmatnr.
    DELETE ADJACENT DUPLICATES FROM gt_qals_temp COMPARING werk selmatnr.

    gt_final = REDUCE tt_final(
                  INIT tab = gt_final
                  FOR ls_qals IN gt_qals_temp
                  LET lv_testing_dt = test_dt - 2220
                      lv_keycomb    = replace( val   = |{ ls_qals-werk }{ ls_qals-selmatnr }{ gs_final-lot_no }{ lv_testing_dt }|
                                               regex = '\s+'
                                               with  = '' )
                  NEXT tab = VALUE #( BASE tab
                                      ( werks      = ls_qals-werk
                                        matnr      = ls_qals-selmatnr
                                        matnr_50   = ls_qals-selmatnr
                                        testing_dt = lv_testing_dt
                                        keycomb    = lv_keycomb ) ) ).
  ENDIF.

  SORT gt_final BY werks matnr testing_dt.
  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING werks matnr testing_dt.

ENDFORM.
