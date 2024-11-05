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
        DATA(url) = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/?$format=json`.
        DATA(odata_test_client) = zcl_odata_test=>new( ).
        out->write( odata_test_client->get_csrf_token( ) ).
        odata_test_client->do_get(
                                    url        = url
                                    csrf_fetch = abap_true
                                 )->assert_http_200(
                                 )->assert_json_content_type(
                                 )->assert_csrf_token_not_initial(
                                 )->assert_json_not_empty(
                                 ).
        out->write( |INFO: | & |"GET | & |{ url }| & |"| & | passed | ).

      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        out->write( exception->get_text( )  ).
      CATCH cx_aunit_sbx_quit_test_method INTO DATA(uncaught_exception).
        out->write( |ERROR: | & |"GET | & |{ url }| & |"| & | failed| ).
    ENDTRY.



  ENDMETHOD.
ENDCLASS.
