INTERFACE zif_rest_client PUBLIC .
  TYPES: BEGIN OF ty_response,
           code            TYPE i,
           reason          TYPE string,
           header          TYPE tihttpnvp,
           body            TYPE string,
           response_object TYPE REF TO if_http_response,
         END OF ty_response.
  METHODS get
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS post
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS put
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS delete
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS get_csrf_token
    RETURNING VALUE(result) TYPE string.
ENDINTERFACE.
