INTERFACE zif_odata_client PUBLIC .
  METHODS get
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS post
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS put
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS delete
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS get_csrf_token
    RETURNING VALUE(result) TYPE string.
  METHODS set_json.
  METHODS set_xml.
  METHODS set_stats_true.
ENDINTERFACE.
