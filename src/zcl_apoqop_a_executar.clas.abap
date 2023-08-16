class ZCL_APOQOP_A_EXECUTAR definition
  public
  inheriting from /BOBF/CL_LIB_A_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_ACTION~EXECUTE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APOQOP_A_EXECUTAR IMPLEMENTATION.


  method /BOBF/IF_FRW_ACTION~EXECUTE.

    DATA(LT_ZTIZQMPP001) = VALUE ZTIZQMPP001( ).

    io_read->retrieve(
      EXPORTING
        iv_node         = is_ctx-node_key
        it_key          = it_key
      IMPORTING
        et_data         = LT_ZTIZQMPP001
    ).

    LOOP AT LT_ZTIZQMPP001 ASSIGNING FIELD-SYMBOL(<LS_ZTIZQMPP001>).

        check <LS_ZTIZQMPP001>-timestamp_execucao is initial.

        GET TIME STAMP FIELD <LS_ZTIZQMPP001>-timestamp_execucao.

        " Update the changed data (billig_status) of the node instance
        io_modify->update(
          EXPORTING
            iv_node               = is_ctx-node_key
            iv_key                = <LS_ZTIZQMPP001>-key
            iv_root_key           = <LS_ZTIZQMPP001>-root_key
            is_data               = REF #( <LS_ZTIZQMPP001>-node_data )
            it_changed_fields     = VALUE #(
              ( ZIF_I_ZQMPP001_C=>sc_node_attribute-zi_zqmpp001-timestamp_execucao )
            )
          ).

    endloop.

  endmethod.
ENDCLASS.
