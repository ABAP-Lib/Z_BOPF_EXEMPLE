FUNCTION-POOL ZQMPP                      MESSAGE-ID SV.

* INCLUDE LZQMPPD...                         " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZQMPPT00                               . "view rel. data dcl.

  data: WC_UCOMM(10),
        wc_mode(01).

   DATA: BEGIN OF T_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF T_BDC.

DATA: MESSTAB  LIKE BDCMSGCOLL  OCCURS 0 WITH HEADER LINE.
