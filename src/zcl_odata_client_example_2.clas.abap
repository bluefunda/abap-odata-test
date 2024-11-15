CLASS zcl_odata_client_example_2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES t_bp TYPE TABLE OF bu_partner WITH DEFAULT KEY.
    METHODS test_data
      RETURNING
        VALUE(result) TYPE t_bp.
ENDCLASS.



CLASS zcl_odata_client_example_2 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    TRY.

        DATA(bp_tab) = test_data( ). "prepare test data = 20 BP numbers

        LOOP AT bp_tab ASSIGNING FIELD-SYMBOL(<bp>).

          "prepare URL with variables
          "v1 = variable value - name can be any as long as it is enclosed with {}
          DATA(url) = `/sap/opu/odata/IWBEP/GWSAMPLE_BASIC/BusinessPartnerSet({v1})?$format=json`.

          "construct key-value table with variables and their respective values
          DATA vars TYPE zif_odata_test=>t_vars.
          vars = VALUE #( ( key = `v1` value = <bp> ) ).

          "call GET with URL & VARS
          DATA(odata_test_client) = zcl_odata_test=>new( ).
          DATA(result) =
                        odata_test_client->get(
                                                url  = url
                                                vars = vars  ).
          "check response body (one can check response time, response status, header etc)
          out->write( result->response->body ).

        ENDLOOP.

      CATCH zcx_odata_client INTO DATA(error).
        out->write( error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD test_data.

    DATA bp TYPE bu_partner VALUE '0100000000'.
    DATA(bp_number) = bp.
    DO 19 TIMES.
      bp_number = bp_number + 1.
      bp_number = |{ bp_number ALPHA = IN }|.
      INSERT bp_number INTO TABLE result.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
