*----------------------------------------------------------------------*
***INCLUDE LZQMPPI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  YS_VALIDACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE YS_VALIDACAO INPUT.

 IF ZQMPP002-ERNAM IS INITIAL.
    ZQMPP002-ERNAM = SY-UNAME.
    ZQMPP002-ERDAT = SY-DATUM.
    ELSE.
    IF FUNCTION = 'NEWL'.
      WC_UCOMM = FUNCTION.
    ENDIF.
    IF WC_UCOMM <> 'NEWL'.
    ZQMPP002-AENAM = SY-UNAME .
    ZQMPP002-AEDAT = SY-DATUM .
    ENDIF.
    ENDIF.
ENDMODULE.
