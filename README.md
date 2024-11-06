# ABAP OData Test Library
## Where to use
You can use this in writing unit-tests or in ADT for console run or in standalone programs.
## Example:
ADT console run
```
TRY.
        DATA(url) = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/ServiceCollection?$format=json`.
        DATA(odata_test_client) = zcl_odata_test=>new( ).
        out->write( odata_test_client->get_csrf_token( ) ).
        odata_test_client->get(
                                    url        = url
                                    csrf_fetch = abap_true
                                 )->assert_http_200(                                "HTTP 200 assertion
                                 )->assert_json_content_type(                       "'application/json; charset=utf-8'
                                 )->assert_csrf_token_not_initial(                  "'x-csrf-token' fetch
                                 )->assert_json_not_empty(                          "Empty JSON check
                                 )->assert_json_path_exists( '/d/results/1/Title'   "follows AJSON project
                              ).

        out->write( |INFO: | & |"GET | & |{ url }| & |"| & | passed | ).

        out->write( odata_test_client->get(
                            url        = url
                            csrf_fetch = abap_true
                          )->get_json_path_value( '/d/results/5/Title'
                         ) ).

      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        out->write( exception->get_text( )  ).
      CATCH cx_aunit_sbx_quit_test_method INTO DATA(uncaught_exception).
        out->write( |ERROR: | & |"GET | & |{ url }| & |"| & | failed| ).
    ENDTRY.
```
