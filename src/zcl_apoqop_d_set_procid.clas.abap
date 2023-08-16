class ZCL_APOQOP_D_SET_PROCID definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APOQOP_D_SET_PROCID IMPLEMENTATION.


  method /BOBF/IF_FRW_DETERMINATION~EXECUTE.

    DATA(LT_ZTIZQMPP001) = VALUE ZTIZQMPP001( ).

    io_read->retrieve(
      EXPORTING
        iv_node         = is_ctx-node_key
        it_key          = it_key
      IMPORTING
        et_data         = LT_ZTIZQMPP001
    ).

    LOOP AT LT_ZTIZQMPP001 reference into data(lv_ref_ZTIZQMPP001).

        call function 'NUMBER_GET_NEXT'
          EXPORTING
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZAPOQOP'
*            QUANTITY                = '1'
*            SUBOBJECT               = SPACE
*            TOYEAR                  = '0000'
*            IGNORE_BUFFER           = SPACE
           IMPORTING
             NUMBER                  = lv_ref_ZTIZQMPP001->procid
*            QUANTITY                =
*            RETURNCODE              =
           EXCEPTIONS
             INTERVAL_NOT_FOUND      = 1
             NUMBER_RANGE_NOT_INTERN = 2
             OBJECT_NOT_FOUND        = 3
             QUANTITY_IS_0           = 4
             QUANTITY_IS_NOT_1       = 5
             INTERVAL_OVERFLOW       = 6
             BUFFER_OVERFLOW         = 7
             OTHERS                  = 8
          .

        IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        " Update the changed data (billig_status) of the node instance
        io_modify->update(
          EXPORTING
            iv_node               = is_ctx-node_key
            iv_key                = lv_ref_ZTIZQMPP001->key
            iv_root_key           = lv_ref_ZTIZQMPP001->root_key
            is_data               = lv_ref_ZTIZQMPP001
            it_changed_fields     = VALUE #(
              ( ZIF_I_ZQMPP001_C=>sc_node_attribute-zi_zqmpp001-procid )
            )
          ).

    endloop.


  endmethod.
ENDCLASS.
