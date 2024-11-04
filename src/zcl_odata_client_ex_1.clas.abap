CLASS zcl_odata_client_ex_1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_odata_client_ex_1 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    TRY.
        "DATA(url) = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/ServiceCollection`.
        DATA(odata_client) = zcl_odata_client=>construct( ).
        "odata_client->set_json( ).
        "DATA(response) = odata_client->get( url = url ).
        DATA(csrf_token) = odata_client->get_csrf_token( ).
        out->write( csrf_token ).

      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        out->write( exception->get_text( )  ).
    ENDTRY.



  ENDMETHOD.
ENDCLASS.
