*MIT License
*
*Copyright (c) 2024 BlueFunda
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
class ZCL_REST_CLIENT_ABS definition
  public
  abstract
  create public .

public section.
  PROTECTED SECTION.
    METHODS get
      IMPORTING
                url           TYPE string
                header        TYPE tihttpnvp
                timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
      RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
      RAISING   RESUMABLE(zcx_rest_client).
    METHODS post
      IMPORTING
                url           TYPE string
                header        TYPE tihttpnvp
                body          TYPE string
                timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
      RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
      RAISING   RESUMABLE(zcx_rest_client).
    METHODS put
      IMPORTING
                url           TYPE string
                header        TYPE tihttpnvp
                body          TYPE string
                timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
      RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
      RAISING   RESUMABLE(zcx_rest_client).
    METHODS delete
      IMPORTING
                url           TYPE string
                header        TYPE tihttpnvp
                timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
      RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
      RAISING   RESUMABLE(zcx_rest_client).
    METHODS add_stats_to_url
      IMPORTING url           TYPE string
      RETURNING VALUE(result) TYPE string.
    METHODS add_json_to_url
      IMPORTING url           TYPE string
      RETURNING VALUE(result) TYPE string.
    METHODS add_xml_to_url
      IMPORTING url           TYPE string
      RETURNING VALUE(result) TYPE string.
  PRIVATE SECTION.
    METHODS http_call
      IMPORTING
                method        TYPE string
                url           TYPE string
                header        TYPE tihttpnvp
                body          TYPE string OPTIONAL
                timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
      RETURNING VALUE(result) TYPE REF TO zif_rest_client=>ty_response
      RAISING   zcx_rest_client.
ENDCLASS.



CLASS ZCL_REST_CLIENT_ABS IMPLEMENTATION.


  METHOD add_json_to_url.

    "if url has ?, then don't concatenate with ? -- AND include &
    IF url CA `?`.
      result = |{ url }| & |&$format=json|.
      RETURN.
    ENDIF.
    "if url has no ?, then concatenate with ?    -- AND exclude &
    result = |{ url }| & |?$format=json|.

  ENDMETHOD.


  METHOD add_stats_to_url.
    "if url has ?, then don't concatenate with ? -- AND include &
    IF url CA `?`.
      result = |{ url }| & |&sap-statistics=true|.
      RETURN.
    ENDIF.
    "if url has no ?, then concatenate with ?    -- AND exclude &
    result = |{ url }| & |?sap-statistics=true|.
  ENDMETHOD.


  METHOD add_xml_to_url.
    "if url has ?, then don't concatenate with ? -- AND include &
    IF url CA `?`.
      result = |{ url }| & |&$format=xml|.
      RETURN.
    ENDIF.
    "if url has no ?, then concatenate with ?    -- AND exclude &
    result = |{ url }| & |?$format=xml|.
  ENDMETHOD.


  METHOD delete.
    result = http_call(
                                method = `DELETE`
                                url = url
                                header = header
                                timeout = timeout ).
  ENDMETHOD.


  METHOD get.
    result = http_call(
                                method = `GET`
                                url = url
                                header = header
                                timeout = timeout ).

  ENDMETHOD.


  METHOD http_call.
    DATA client TYPE REF TO if_http_client.
    cl_http_client=>create_internal(
  IMPORTING
    client            = client
  EXCEPTIONS
    plugin_not_active = 1
    internal_error    = 2
    OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_rest_client( textid = VALUE scx_t100key(
                                                           msgid = sy-msgid
                                                           msgno = sy-msgno
                                                           attr1 = sy-msgv1
                                                           attr2 = sy-msgv2
                                                           attr3 = sy-msgv3
                                                           attr4 = sy-msgv4 ) ).
    ENDIF.
    client->request->set_method( method ).
    DATA(relative_url) = client->create_rel_url( path = url ).
    cl_http_utility=>set_request_uri(
      request    = client->request
      uri        = relative_url ).
    client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
    IF header IS NOT INITIAL.
      client->request->set_header_fields( header ).
    ENDIF.
    IF body IS NOT INITIAL.
      client->request->set_cdata(
        data   = body ).
    ENDIF.
    client->send(
      EXPORTING
        timeout                    = timeout
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_rest_client( textid = VALUE scx_t100key(
                                                           msgid = sy-msgid
                                                           msgno = sy-msgno
                                                           attr1 = sy-msgv1
                                                           attr2 = sy-msgv2
                                                           attr3 = sy-msgv3
                                                           attr4 = sy-msgv4 ) ).
    ENDIF.
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).
    IF sy-subrc <> 0.
      client->get_last_error(
        IMPORTING
          code           = DATA(code)
          message        = DATA(message)
          message_class  = DATA(message_class)
          message_number = DATA(message_number)
      ).
      RAISE EXCEPTION NEW zcx_rest_client( textid = VALUE scx_t100key(
                                                           msgid = sy-msgid
                                                           msgno = sy-msgno
                                                           attr1 = sy-msgv1
                                                           attr2 = sy-msgv2
                                                           attr3 = sy-msgv3
                                                           attr4 = sy-msgv4 ) ).
    ENDIF.

    " capture response (body, status, reason, header fields)
    result = NEW #( ).
    result->body = client->response->get_cdata( ).
    client->response->get_status(
    IMPORTING
    code = result->code
    reason = result->reason ).
    client->response->get_header_fields( CHANGING fields = result->header ).
    result->response_object ?= client->response.
    " close client connection
    client->close(
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(client_close_error).
    ENDIF.

  ENDMETHOD.


  METHOD post.
    result = http_call(
                                method = `POST`
                                url = url
                                header = header
                                body = body
                                timeout = timeout ).

  ENDMETHOD.


  METHOD put.
    result = http_call(
                                method = `PUT`
                                url = url
                                header = header
                                body = body
                                timeout = timeout ).

  ENDMETHOD.
ENDCLASS.
