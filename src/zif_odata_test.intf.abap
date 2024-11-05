INTERFACE zif_odata_test PUBLIC .

  METHODS do_get
    IMPORTING url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
              csrf_fetch    TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS get_csrf_token
    RETURNING VALUE(result) TYPE string.
  METHODS assert_csrf_token_not_initial
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_status
    IMPORTING status        TYPE i
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_http_200
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_http_201
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_content_type
    IMPORTING content_type  TYPE string
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_json_content_type
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_json_not_empty
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.

ENDINTERFACE.
