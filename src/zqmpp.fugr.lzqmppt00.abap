*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZQMPP002........................................*
DATA:  BEGIN OF STATUS_ZQMPP002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZQMPP002                      .
CONTROLS: TCTRL_ZQMPP002
            TYPE TABLEVIEW USING SCREEN '0900'.
*.........table declarations:.................................*
TABLES: *ZQMPP002                      .
TABLES: ZQMPP002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
