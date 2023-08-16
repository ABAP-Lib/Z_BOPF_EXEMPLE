FUNCTION ZQM_BUSCA_RESULTADOS_PP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_QALS) LIKE  QALS STRUCTURE  QALS
*"     VALUE(I_QAVE) LIKE  QAVE STRUCTURE  QAVE
*"  EXPORTING
*"     VALUE(E_SUBRC) LIKE  SY-SUBRC
*"  TABLES
*"      E_PROTOCOL STRUCTURE  RQEVP
*"----------------------------------------------------------------------
 data: tI_QALS type QALS,
       ti_qave type qave,
       ti_qapo type qapo,
       ti_qapp type qapp,
       ti_qalt type qalt,
       ti_QAMKR type TABLE OF QAMKR WITH HEADER LINE.
* This FUNCTION is only a COPY_REFERENCE_FORM and it doesn't give
* any Output
* please us another FUNCTION out of this GROUP of FUNCTIONS.
* When you copy this FUNCTION into another one, you should use
* another GROUP OF FUNCTIONS (see rules for modification!)

*  >> dados bapi BAPI_PRODORDCONF_GET_TT_PROP
data: ti_TIMETICKET TYPE TABLE OF BAPI_PP_TIMETICKET WITH HEADER LINE,
      ti_GOODSMOVEMENTS TYPE TABLE OF BAPI2017_GM_ITEM_CREATE WITH HEADER LINE,
      TI_CORU_RETURN    TYPE TABLE OF  BAPI_CORU_RETURN WITH HEADER LINE,
      ls_bapi_propose       TYPE BAPI_PP_CONF_PROP,
      wc_return type TABLE OF BAPIRET1 WITH HEADER LINE,
      ti_afvc       TYPE STANDARD TABLE OF afvc WITH HEADER LINE,
      ti_afkO       TYPE STANDARD TABLE OF afkO WITH HEADER LINE,
      ti_MAST       type STANDARD TABLE OF mast WITH HEADER LINE,
      ti_MARD       TYPE STANDARD TABLE OF mard WITH HEADER LINE,
      ti_aufk       TYPE STANDARD TABLE OF aufk WITH HEADER LINE,
      ti_STPO       type STANDARD TABLE OF STPO WITH HEADER LINE,
      ti_PLMK       TYPE STANDARD TABLE OF PLMK WITH HEADER LINE,
      ti_QAMR       TYPE STANDARD TABLE OF QAMR WITH HEADER LINE,
      wc_STLNR      type MAST-STLNR,
      wc_lgort      type mard-lgort,
      WC_ANO(04),
      wc_LOSMENGEx(15),
      WC_VAL(13),
      WC_VALF(13),
      wc_LOSMENGE type BAPI_PP_TIMETICKET-YIELD,
      WC_METT TYPE QAMR-MITTELWERT.

*  >> Selecionando os dados para QM – Entrada de resultados
     MOVE-CORRESPONDING I_QALS to tI_QALS.
     MOVE-CORRESPONDING i_qave to ti_qave.

     CLEAR Ti_AFKO[].

     SELECT SINGLE * FROM AFKO INTO Ti_AFKO
              WHERE AUFNR = tI_QALS-aufnr.

    if sy-subrc eq 0.
* >> Em seguida selecione a tabela AFVC - Operação da ordem os campos
*      AFVC-VORNR, AFVC-WERKS where AFVC-AUFPL = AFKO-AUFPL and AFKO-STEUS = “QM01” ! Chave para Controle de características
           SELECT single * FROM AFVC INTO Ti_AFVC
              WHERE AUFPL = TI_AFKO-AUFPL
              AND   STEUS <> 'QM01'.

    IF SY-SUBRC EQ 0.

           SELECT single * FROM PLMK INTO Ti_PLMK
              WHERE PLNTY = tI_QALS-PLNTY
              AND   PLNNR = tI_QALS-PLNNR
              AND   DUMMY20 = 'Valor real'.

           if sy-subrc eq 0.

           SELECT single * FROM QAMR INTO Ti_QAMR
              WHERE PRUEFLOS = TI_QALS-PRUEFLOS
*              AND   VORGLFNR = Ti_PLMK-PLNKN
              AND   MERKNR   =  Ti_PLMK-MERKNR
              AND   SATZSTATUS = '5'.

          IF SY-SUBRC EQ 0.

*  conversao valor
*WC_METT = Ti_QAMR-MITTELWERT.
*CALL FUNCTION 'FLTP_CHAR_CONVERSION'
*EXPORTING
*DECIM = Ti_PLMK-STELLEN
*INPUT = WC_METT
*IMPORTING
*FLSTR = wc_LOSMENGEX.
*SHIFT wc_LOSMENGeX LEFT DELETING LEADING SPACE .
*REPLACE ',' WITH '.' INTO wc_LOSMENGeX.
*MOVE wc_LOSMENGEX TO wc_LOSMENGE.

*  aplicar shdb inicio.
*  shdb fim
WC_VAL = wc_LOSMENGE.
CONDENSE WC_VAL.
REPLACE '.' WITH ',' INTO wc_val.

WC_VAL = TI_QALS-LOSMENGE.
CONDENSE WC_VAL.

IF TI_QALS-MENGENEINH <> 'UN'.
REPLACE '.' WITH ',' INTO wc_val.
ELSE.
  SPLIT WC_VAL AT '.' INTO WC_VAL WC_VALF.
  CONDENSE WC_VAL.
  ENDIF.

 CLEAR: T_BDC[], MESSTAB[],
        T_BDC, MESSTAB.

  PERFORM F_BDC USING:
    'X'     'SAPLCORU_S'         '0100',
    '  '    'BDC_OKCODE'         '/00',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0010SUB01',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0110SLOT_HDR',
    '  '    'AFRUD-AUFNR'         ti_qals-aufnr,
    '  '    'AFRUD-VORNR'         ti_AFVC-VORNR,
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0105SLOT_CONF_TYPE',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0200SLOT_DET1',
    '  '    'BDC_CURSOR'         'AFRUD-LMNGA',
    '  '    'AFRUD-LMNGA'          wc_VAL,
    '  '    'BDC_SUBSCR'         'SAPLCORU_S                              0300SLOT_DET2',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S                              0400SLOT_DET3',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S                              0500SLOT_DET4'.

  PERFORM F_BDC USING:
    'X'     'SAPLCORU_S'         '0100',
    '  '    'BDC_OKCODE'         '=MB03',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0010SUB01',
    '  '    'BDC_SUBSCR'         'SAPLCORU_S      0110SLOT_HDR'.

  PERFORM F_BDC USING:
    'X'     'SAPLCOWB'           '0130',
    '  '    'BDC_OKCODE'         '=WEIT',
    '  '    'BDC_SUBSCR'         'SAPLCOWB                                0802HEADER',
    '  '    'BDC_SUBSCR'         'SAPLCOWB                                0500TABLE',
    '  '    'BDC_CURSOR'         'COWB_COMP-ERFMG(01)'.

    wc_mode = 'N'.
    CALL TRANSACTION 'CO11N'
     USING T_BDC
     MODE  WC_MODE
     UPDATE 'S'
     MESSAGES INTO MESSTAB.

  READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC NE 0.
   commit WORK AND WAIT.
   endif.
      endif.
*      endif.
      endif.
      endif.
      endif.


ENDFUNCTION.
