*&---------------------------------------------------------------------*
*& Report ZRPM_INTERF_HPLC_NIR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrpm_interf_hplc_nir_log MESSAGE-ID zadftax.

CONSTANTS : cc_endda TYPE endda VALUE '99991231',
            cc_check TYPE c LENGTH 1 VALUE 'X',
  cc_complete_enqueue TYPE char1                    VALUE 'C',
  cc_on(1)            VALUE 'X',
  cc_topofpage        TYPE slis_formname VALUE 'TOP_OF_PAGE'.

*--------------------------------------------------------------------*
* Tipos
*--------------------------------------------------------------------*
  TABLES: ZQMPP002, ZQMPP003.
*  area delacariva
types:  BEGIN OF tp_lista,
         sel(1) TYPE c,
         TXT type T003P-TXT,
         data(10),
         hora(08),
         qtde(13),
         unid(05),
         aufnr(12),
         zlog(30).
        INCLUDE STRUCTURE ZQMPP002.
TYPES END OF tp_lista.

types:  BEGIN OF tp_AUFK,
         aufnr type aufk-aufnr,
         objnr type aufk-objnr,
         auart type aufk-auart,
         ERDAT type aufk-erdat,
       END OF tp_aufk,

       BEGIN OF tp_caract,
         WERKS   type ZQMPP002-werks,
         caract(10), "  type ZQMPP002-TPROC,"(20),
         MERKNR(04) type n,
         END OF tp_caract,

       BEGIN OF tp_mara,
         matnr   type mara-matnr,
         MEINS   type MARA-MEINS,
         END OF tp_mara.

DATA: vg_butxt TYPE t001-butxt.

*  dados do alv.
DATA: ti_fieldcat    TYPE slis_t_fieldcat_alv,
      e_layout       TYPE slis_layout_alv,
      ti_top_of_page TYPE slis_t_listheader,
      ti_print       TYPE slis_print_alv,
      ti_sort        TYPE slis_t_sortinfo_alv,
      ti_excluding   TYPE slis_t_extab.

DATA: t_arquivo  TYPE TABLE OF ZQMPP002  WITH HEADER LINE,
      t_caract   type TABLE of tp_caract WITH HEADER LINE,
      t_T003P    TYPE TABLE OF T003P     WITH HEADER LINE,
      t_ZQMT_INTERFACE  type TABLE of ZQMT_INTERFACE  WITH HEADER LINE,
      T_mara            TYPE TABLE OF tp_mara         WITH HEADER LINE,
      t_ZQMPP003        TYPE TABLE OF ZQMPP003        WITH HEADER LINE,
      t_return   TYPE TABLE OF bapiret2               WITH HEADER LINE.

DATA: wc_repid LIKE sy-repid,
      wc_save.
DATA:  e_variante    LIKE disvariant.

DATA: lt_lista     TYPE STANDARD TABLE OF  tp_lista WITH HEADER LINE,
      lt_lista_aux TYPE STANDARD TABLE OF  tp_lista WITH HEADER LINE,
      ls_lista     TYPE  tp_lista,
      lt_comp      TYPE string, "yst_lista_comp_rend,
      ls_comp      TYPE string, "yse_lista_comp_rend,
      t_aufk       TYPE STANDARD TABLE OF tp_aufk WITH HEADER LINE,
      ti_aufk       TYPE STANDARD TABLE OF aufk WITH HEADER LINE,
      ti_afvc       TYPE STANDARD TABLE OF afvc WITH HEADER LINE,
      ti_afkO       TYPE STANDARD TABLE OF afkO WITH HEADER LINE,
      ls_return    TYPE bapireturn.

DATA: vg_template LIKE disvariant,
      vg_f4_exit  TYPE c,
      ZData type sy-datum,
      wc_name1    type t001w-name1,
      wc_prueflos type afko-prueflos.

DATA: lc_msg TYPE c LENGTH 220,
      wc_mod.
*  PARAMETROS DE ENTRADA

*  dados das baps.
 data:  t_ORDERDATA type table of BAPI_PP_ORDER_CREATE WITH HEADER LINE,
        wc_ORDER_NUMBER type BAPI_ORDER_KEY-ORDER_NUMBER,
        wc_ORDER_TYPE   type BAPI_ORDER_COPY-ORDER_TYPE,
        ti_return        type BAPIRET2.
*  dados bapi DU
 DATA: WC_NUMBER    TYPE BAPI2045UD-INSPLOT,
       WC_UD_DATA   TYPE TABLE OF BAPI2045UD WITH HEADER LINE,
       WC_LANGUAGE  TYPE TABLE OF LANGUAGE   WITH HEADER LINE,
       WC_UD_MODE   TYPE BAPI2045D_IL4-UD_MODE,
       " EXPORT
       WC_UD_RETURN_DATA TYPE TABLE OF BAPI2045UD_RETURN WITH HEADER LINE,
       WC_STOCK_DATA     TYPE TABLE OF BAPI2045D_IL2 WITH HEADER LINE,
       WC_RETURN1        TYPE TABLE OF BAPIRETURN1   WITH HEADER LINE,
       WC_STOCK_DATA1     TYPE TABLE OF /CWM/BAPI2045D_IL2 WITH HEADER LINE.
*  >> dados ordem
 "WC_TIMETICKET
*  dados BDCDATA.
 DATA: BEGIN OF T_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF T_BDC.

DATA: MESSTAB  LIKE BDCMSGCOLL  OCCURS 0 WITH HEADER LINE.
*  DADOS SELECAO
SELECTION-SCREEN BEGIN OF BLOCK bloq WITH FRAME TITLE TEXT-180.
  PARAMETERS: P_werks TYPE t001w-werks.
  SELECT-OPTIONS: sc_CUART for zqmpp002-CAUART,
                  sc_TPROC for ZQMPP002-TPROC.
*                  sc_data  for ZQMPP002-ERDAT no INTERVALS.
SELECTION-SCREEN END OF BLOCK bloq.

AT SELECTION-SCREEN.

  IF NOT P_werks IS INITIAL.
    SELECT SINGLE name1 INTO wc_name1
      FROM t001w
      WHERE werks = p_werks.
    IF sy-subrc NE 0.
      MESSAGE e000(zadftax) WITH 'Centro inexistente'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
     ZData = sy-datum.
*  verifica tipo de selecao para arquivo.
    PERFORM ys_upload.
*&---------------------------------------------------------------------*
*& Form ys_upload
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ys_upload .
    data: ano(04),
          mes(02),
          dia(02),
          date type sy-datum.

    clear t_arquivo[].
    PERFORM ys_visivel.
if not t_arquivo[] is initial.
  clear LT_LISTA[].
  loop at t_arquivo.
    clear: t_T003P, T_AUFK, LT_LISTA,
           t_ZQMT_INTERFACE,
           t_mara.
    MOVE-CORRESPONDING t_arquivo to LT_LISTA.
    WRITE ZDATA TO LT_LISTA-DATA.
    WRITE SY-UZEIT TO LT_LISTA-HORA.
    READ TABLE t_T003P WITH KEY AUART = t_arquivo-CAUART.
    LT_LISTA-txt = t_T003P-txt.

* recupera dados quanbtidade e unidade
    t_caract-caract = t_arquivo-TPROC.
    date = t_arquivo-ERDAT.
    sort t_ZQMT_INTERFACE by ID_PROCESSO data hora DESCENDING.
    READ TABLE t_ZQMT_INTERFACE WITH KEY werks = t_arquivo-werks
                                          ID_PROCESSO = t_caract-caract.
*                                          data        =  date.
       if sy-subrc eq 0.
         LT_LISTA-qtde  =  t_ZQMT_INTERFACE-RESULTADO.
       endif.

    READ TABLE t_mara WITH KEY matnr = t_arquivo-PLNBEZ.

       if sy-subrc eq 0.
         LT_LISTA-unid =  t_mara-meins.
       endif.

*     loop at t_AUFK where AUART = t_arquivo-CAUART
*                    and   ERDAT = ZData.
*
*      clear wc_prueflos.
*      SELECT SINGLE prueflos FROM AFKO INTO wc_prueflos
*          WHERE AUFNR = T_AUFK-AUFNR
*            and plnnr = t_arquivo-tproc.
*
*       if wc_prueflos is not initial.
*         LT_LISTA-aufnr = T_AUFK-AUFNR.
*         CONCATENATE 'Processo com lote' wc_prueflos 'já atualizado'
*         into  LT_LISTA-ZLog SEPARATED BY space.
*         exit.
*         endif.
*      endloop.

        READ TABLE t_ZQMPP003 WITH KEY PROCID = t_arquivo-procid
                                       WERKS  = t_arquivo-werks
                                      CAUART  = t_arquivo-cauart
                                      TPROC   = t_arquivo-tproc
                                      DATA    =  ZData.
          IF SY-SUBRC EQ 0.
         LT_LISTA-aufnr = t_ZQMPP003-AUFNR.
         LT_LISTA-qtde  = t_ZQMPP003-qtde.
         CONCATENATE 'Processo com lote' t_ZQMPP003-prueflos 'já atualizado'
         into  LT_LISTA-ZLog SEPARATED BY space.
            ENDIF.
    append LT_LISTA.
    endloop.

  perform ys_imprime.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ys_visivel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ys_visivel.

  SELECT * FROM  ZQMPP002 INTO TABLE T_ARQUIVO
           WHERE werks = P_WERKS
           AND  CAUART IN sc_CUART
           and  TPROC in sc_TPROC
           AND LTEXT  <> 'X'.

  IF t_arquivo[] IS INITIAL.
    MESSAGE i368(00) WITH 'Centro inexistente na ZQMPP002'.
    STOP.
  ENDIF.

select * from T003P into table t_T003P
         FOR ALL ENTRIES IN t_arquivo
         where spras = 'PT'
         AND   AUART = t_arquivo-CAUART.

*Selecione a tabela AUFK- Dados mestre da ordem o campo AUFK-AUFNR, AUFK-OBJNR Where AUFK-AUART = ZQMPP002-AURT and AUFK-ERDAT = ZDataExecução

*select  aufnr objnr auart ERDAT from aufk into table t_AUFK
*         FOR ALL ENTRIES IN t_arquivo
*         where AUART = t_arquivo-CAUART
*         and   ERDAT = ZData.

* recupera dados quanbtidade e unidade
clear: t_caract[].
       loop at t_arquivo.
         t_caract-werks  = t_arquivo-WERKS.
         t_caract-caract = t_arquivo-TPROC.
         append t_caract.
         endloop.

select * from ZQMT_INTERFACE into table t_ZQMT_INTERFACE
         FOR ALL ENTRIES IN t_caract
         where WERKS = t_caract-WERKS
         and   ID_PROCESSO = t_caract-caract
         and   DATA        = zdata
         and   status  <> 'P'.


        select matnr meins from mara into table t_mara
                FOR ALL ENTRIES IN t_arquivo
                where matnr = t_arquivo-PLNBEZ.

        select * from ZQMPP003 into table t_ZQMPP003
                FOR ALL ENTRIES IN t_arquivo
                where PROCID = t_arquivo-procid
                and   WERKS  = t_arquivo-werks
                and  CAUART  = t_arquivo-cauart
                and  TPROC   = t_arquivo-tproc
                and  DATA    =  ZData.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ys_imprime
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ys_imprime .
  PERFORM f_prepara_alv.
*  PERFORM f_modify_ti_fieldcat.
  PERFORM f_display_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepara_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_prepara_alv .
  CLEAR: ti_fieldcat[],
         ti_sort[],
         ti_excluding[],
         e_layout.

* preparação para alv

  PERFORM f_preenche_fieldcat.
  PERFORM f_preenche_layout USING e_layout.
  PERFORM f_monta_cabecalho USING text-007
                                  text-008
                                  ti_top_of_page[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_modify_ti_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_modify_ti_fieldcat .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_display_list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_list .
  DATA: is_layout            TYPE slis_layout_alv,
        l_i_save(1)          TYPE c,
        l_i_callback_program LIKE sy-repid,
        l_i_grid_title       TYPE lvc_title.

  CHECK NOT ti_fieldcat[] IS INITIAL.

  l_i_callback_program = sy-repid.
  l_i_save = 'A'.

  is_layout-zebra = 'X'.
  is_layout-numc_sum = 'X'.
  is_layout-subtotals_text = 'X'.
  is_layout-totals_before_items = 'X'.
  is_layout-colwidth_optimize = 'X'.
  l_i_grid_title = sy-title.
*  is_layout-box_fieldname = 'SEL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title             = l_i_grid_title
      i_buffer_active          = ''
      i_callback_program       = l_i_callback_program
      i_callback_pf_status_set = 'F_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_save                   = l_i_save
      is_layout                = is_layout
      is_variant               = vg_template
      it_fieldcat              = ti_fieldcat[]
    TABLES
      t_outtab                 = lt_lista
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  CHECK sy-subrc EQ 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_preenche_fieldcat .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'SEL'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 2.
  ls_fieldcat-checkbox = cc_check.
  ls_fieldcat-edit = cc_check.
  ls_fieldcat-seltext_l = 'Mark'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'PROCID'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-seltext_l = 'Sequencia'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'WERKS'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-seltext_l = 'Processo PP'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'TXT'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 20.
  ls_fieldcat-seltext_l = 'Descrição'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'TPROC'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 24.
  ls_fieldcat-seltext_l = 'Processo QM'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'DATA'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 18.
  ls_fieldcat-seltext_l = 'Data execução'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'Hora'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-seltext_l = 'Hora execucao'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'qtde'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-edit = cc_check.
  ls_fieldcat-seltext_l = 'Qtde'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'unid'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-seltext_l = 'Unidmed'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'AUFNR'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-seltext_l = 'Ordem'.
  APPEND ls_fieldcat TO ti_fieldcat.

  CLEAR ls_fieldcat.

  ls_fieldcat-fieldname = 'zlog'.
  ls_fieldcat-tabname = 'LT_LISTA'.
  ls_fieldcat-outputlen = 30.
  ls_fieldcat-seltext_l = 'Log ação'.
  APPEND ls_fieldcat TO ti_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_LAYOUT
*&---------------------------------------------------------------------*
FORM f_preenche_layout USING    p_layout TYPE slis_layout_alv.
  p_layout-zebra               = cc_on.
  p_layout-colwidth_optimize   = cc_on.
  p_layout-totals_only         = cc_on.
  p_layout-numc_sum = 'X'.
  p_layout-subtotals_text = 'X'.
  p_layout-totals_before_items = 'X'.
  p_layout-colwidth_optimize = 'X'.
  p_layout-box_fieldname = 'SEL'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_cabecalho
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_007
*&      --> TEXT_008
*&      --> TI_TOP_OF_PAGE[]
*&---------------------------------------------------------------------*
FORM f_monta_cabecalho USING    p_report
                                p_texto
                                p_top_of_page TYPE slis_t_listheader.

  DATA: ls_line          TYPE slis_listheader,
        lc_tit(50)       TYPE c,
        lc_registros(30),
        lc_lin(10),
        li_linhas TYPE i.

* Título do Report
*  CONCATENATE p_report p_exerc INTO lc_tit SEPARATED BY space.

  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = lc_tit.
  APPEND ls_line TO p_top_of_page.

* DESCRIBE TABLE ti_reg_saida LINES li_linhas.

  CLEAR: lc_registros, lc_lin.

  lc_lin = li_linhas.

  SHIFT lc_lin LEFT DELETING LEADING space.

  CONCATENATE p_texto lc_lin INTO lc_registros
                                     SEPARATED BY space.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = lc_registros.
  APPEND ls_line TO p_top_of_page.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PF_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pf_status USING ft_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD03'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM user_command USING   r_ucomm TYPE sy-ucomm
                          r_selfield TYPE slis_selfield.

DATA: submitid(30).

  DATA:
    w_tabix1    LIKE sy-tabix,
    w_tabix2    LIKE sy-tabix,
    ref_grid    TYPE REF TO cl_gui_alv_grid,
    lt_process  TYPE STANDARD TABLE OF tp_lista,
    ls_process  TYPE tp_lista,
    ls_return   TYPE bapiret1,
    ls_xstring  TYPE xstring,
    lc_lines    TYPE p,
    lv_arq_name TYPE string,
    lv_dest     TYPE rlgrap-filename,
    LC_FLAG,
    lv_dest1    TYPE rlgrap-filename,
    lc_PRCTR    type marc-prctr.

  data: lc_MKAL type MKAL,
        lc_data(08),
        lc_hora(08),
        lc_qtde(13),
        lc_dia(02), lc_mes(02), lc_ano(04).
*
  CASE r_ucomm.
    WHEN 'PROCESSAR'.
      REFRESH lt_process.
      IF ref_grid IS INITIAL.
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = ref_grid.
      ENDIF.
      IF NOT ref_grid IS INITIAL.
        CALL METHOD ref_grid->check_changed_data.
      ENDIF.
*      SORT lt_lista BY sel DESCENDING.

      LOOP AT lt_lista INTO ls_lista WHERE sel = 'X'.
      IF ls_lista-zlog IS not INITIAL.
        CONCATENATE ' ' 'registro já processado ! '   INTO lc_msg.
        MESSAGE lc_msg TYPE 'W'.
        CONTINUE.
      ENDIF.
        MOVE-CORRESPONDING ls_lista TO ls_process.
        APPEND ls_process TO lt_process.
      ENDLOOP.

      r_selfield-refresh = cc_check.
      CLEAR lc_lines.
      IF lt_process[] IS INITIAL.
        CONCATENATE ' ' 'Selecionar pelo menos um registro ! '   INTO lc_msg.
        MESSAGE lc_msg TYPE 'E'.
      ENDIF.

*  atividades para criacao da ordem inicio
     loop at lt_process into ls_process.
       clear: wc_ORDER_NUMBER.
       t_ORDERDATA-MATERIAL = ls_process-PLNBEZ.
       t_ORDERDATA-PLANT    = ls_process-werks.
       t_ORDERDATA-PLANNING_PLANT = ls_process-werks.
       t_ORDERDATA-ORDER_TYPE = ls_process-CAUART.
       lc_dia = ls_process-data(2).
       lc_mes = ls_process-data+3(2).
       lc_ano = ls_process-data+6(4).
       concatenate lc_ano lc_mes lc_dia into lc_data.
       CONCATENATE ls_process-hora(2) ls_process-hora+3(2)  ls_process-hora+6(2) into lc_hora.
       lc_qtde = ls_process-qtde.
       CONDENSE lc_qtde.
       t_ORDERDATA-BASIC_START_DATE = lc_data.
       t_ORDERDATA-BASIC_START_TIME = lc_hora.
       t_ORDERDATA-BASIC_END_DATE   = lc_data.
       t_ORDERDATA-BASIC_END_TIME   =  lc_hora.
       t_ORDERDATA-QUANTITY         =  lc_qtde.
       t_ORDERDATA-QUANTITY_UOM     =  ls_process-unid.
       t_ORDERDATA-MRP_CONTROLLER = 'MRP'.  "! Fixo não temos regra para esse campo
       t_ORDERDATA-BUSINESS_AREA = ls_process-werks.

*Selecione a tabela MARC - Dados de centro para material o campo MARC-PRCTR Where MARC-MATNR = ZPPQM001-CodMaterial
* anda MARC- WERKS = ZPPQM001-GSBER
       select SINGLE prctr into lc_prctr from marc
              where  matnr = ls_process-PLNBEZ
              and    werks = ls_process-werks.
         if sy-subrc eq 0.
           t_ORDERDATA-PROFIT_CENTER = lc_PRCTR.
         endif.

*  >> Selecione a tabela MKAL = Versões de produção por material todos os campos Where MKAL-MATNR = ZPPQM001-PLNBEZ
*     and MKAL-WERKS = ZPPQM001-GSBER and MKAL-MKSP <> “X”! Fixo
       select SINGLE * into lc_mkal from mkal
              where  matnr = ls_process-PLNBEZ
              and    werks = ls_process-werks
              and    MKSP <> 'X'.
         if sy-subrc eq 0.
           t_ORDERDATA-PROD_VERSION = LC_MKAL-VERID.
           t_ORDERDATA-ROUTING_GROUP = LC_MKAL-PLNNR.
           t_ORDERDATA-ROUTING_COUNTER = LC_MKAL-ALNAL.
           t_ORDERDATA-ROUTING_TYPE = lc_MKAL-PLNTY.
         endif.

*  CHAMAR BAP CPM PARAMETRO INFORMADOS
         CLEAR:  wc_ORDER_NUMBER, TI_RETURN.

         CALL FUNCTION 'BAPI_PRODORD_CREATE'
           EXPORTING
             ORDERDATA                = t_ORDERDATA
*            FSH_PRODORD_SEASON       =
          IMPORTING
            RETURN                   =   ti_return
            ORDER_NUMBER             = wc_ORDER_NUMBER
            ORDER_TYPE               = wc_ORDER_TYPE.
   IF  wc_ORDER_NUMBER IS NOT INITIAL.
     call FUNCTION 'BAPI_TRANSACTION_COMMIT'.
     WAIT UP TO 5 SECONDS.
*  >>  Selecione a tabela AUFK - Dados mestre da ordem o campo AUFK-WERKS, AUFK-OBJNR Where AUFK-AUFNR = ZNrORdemCriada.
     clear ti_aufk.
     SELECT SINGLE * FROM AUFK INTO Ti_AUFK
              WHERE AUFNR = WC_ORDER_NUMBER.

       IF SY-SUBRC EQ 0.

 CLEAR: T_BDC[], MESSTAB[],
        T_BDC, MESSTAB.

  PERFORM F_BDC USING:
    'X'     'SAPLCOKO1'          '0110',
    '  '    'BDC_CURSOR'         'CAUFVD-AUFNR',
    '  '    'BDC_OKCODE'         '=ENTK',
    '  '    'CAUFVD-AUFNR'       WC_ORDER_NUMBER,
    '  '    'R62CLORD-FLG_OVIEW' 'X'.

  PERFORM F_BDC USING:
    'X'     'SAPLCOKO1'         '0115',
*    '  '    'BDC_CURSOR'         'CAUFVD-AUFNR',
    '  '    'BDC_OKCODE'         '=FREI'.

  PERFORM F_BDC USING:
    'X'     'SAPLCOKO1'         '0115',
*    '  '    'BDC_CURSOR'         'CAUFVD-AUFNR',
    '  '    'BDC_OKCODE'         '=BU'.

*   Efetua o Call Transaction
  wc_Mod = 'N'.
  CALL TRANSACTION 'CO02'
     USING T_BDC
     MODE  WC_MOD
     UPDATE 'S'
     MESSAGES INTO MESSTAB.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC NE 0.
   commit WORK AND WAIT.
*  >> Selecionando os dados para QM – Entrada de resultados
     CLEAR Ti_AFKO[].
     SELECT SINGLE * FROM AFKO INTO Ti_AFKO
              WHERE AUFNR = WC_ORDER_NUMBER.

    if sy-subrc eq 0.
* >> Em seguida selecione a tabela AFVC - Operação da ordem os campos
*      AFVC-VORNR, AFVC-WERKS where AFVC-AUFPL = AFKO-AUFPL and AFKO-STEUS = “QM01” ! Chave para Controle de características
           SELECT * FROM AFVC INTO TABLE Ti_AFVC
              WHERE AUFPL = TI_AFKO-AUFPL.
*              AND   STEUS = 'QM01'.
    IF SY-SUBRC EQ 0.
*  atualiza lista com resultado
      LOOP AT lt_lista WHERE sel = 'X'
                         and TXT = ls_process-TXT
                         and DATA =  ls_process-DATA
                         and HORA =  ls_process-hora
                         and QTDE =  ls_process-qtde
                         and UNID =  ls_process-UNID
                         and PROCID = ls_process-PROCID
                         and TPROC  = ls_process-TPROC
                         and PLNBEZ = ls_process-PLNBEZ.
        lt_lista-aufnr =  WC_ORDER_NUMBER.

      clear wc_prueflos.
*      SELECT SINGLE prueflos FROM AFKO INTO wc_prueflos
*          WHERE AUFNR =  WC_ORDER_NUMBER.

      SELECT SINGLE prueflos FROM QALS INTO wc_prueflos
          WHERE AUFNR =  WC_ORDER_NUMBER.

       if wc_prueflos is initial.
       LT_LISTA-ZLog = 'Processo já atualizado na Data Informada'.
       else.
         CONCATENATE 'Processo com lote' wc_prueflos 'já atualizado'
         into  LT_LISTA-ZLog SEPARATED BY space.
         endif.
        lt_lista-sel = space.
        modify lt_lista index sy-tabix.

        clear: t_ZQMPP003, ZQMPP003.
                 t_ZQMPP003-PROCID = lt_lista-procid.
                 t_ZQMPP003-WERKS  = p_werks.
                 t_ZQMPP003-CAUART = ls_process-cauart.
                 t_ZQMPP003-TPROC  = ls_process-tproc.
                 t_ZQMPP003-DATA   =  ZData.
                 t_ZQMPP003-AUFNR =  WC_ORDER_NUMBER.
                 t_ZQMPP003-prueflos = wc_prueflos.
                 t_ZQMPP003-ERNAM  = sy-uname.
                 t_ZQMPP003-qtde  = lt_lista-qtde.

                 delete FROM ZQMPP003 where PROCID = t_ZQMPP003-PROCID
                                       and  WERKS  = t_ZQMPP003-WERKS
                                       and  CAUART = t_ZQMPP003-CAUART
                                       and  TPROC  = t_ZQMPP003-TPROC
                                       and  DATA   = t_ZQMPP003-DATA.
                 ZQMPP003 = t_ZQMPP003.
                 insert ZQMPP003.
                 if sy-subrc eq 0.
                   commit WORK AND WAIT.
                   endif.
       exit.
       endloop.
      ENDIF.
      endif.
    ENDIF.
  ENDIF.
 ELSE.

 ENDIF.

 endloop.
*  atividades para criacao da ordem fim
    WHEN OTHERS.
  ENDCASE.

  r_selfield-refresh = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM F_BDC USING DYNBEGIN PROGRAM DYNPRO.

  CLEAR T_BDC.
  IF DYNBEGIN EQ 'X'.
    T_BDC-PROGRAM  = PROGRAM.
    T_BDC-DYNPRO   = DYNPRO.
    T_BDC-DYNBEGIN = 'X'.
  ELSE.
    T_BDC-FNAM = PROGRAM.
    T_BDC-FVAL = DYNPRO.
  ENDIF.
  APPEND T_BDC.

ENDFORM.
