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
CLASS zcl_odata_client DEFINITION
  PUBLIC
  INHERITING FROM zcl_rest_client_abs
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_odata_client .

    CLASS-METHODS construct
      RETURNING
        VALUE(result) TYPE REF TO zif_odata_client .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA json TYPE abap_bool.
    DATA xml TYPE abap_bool.
    DATA stats TYPE abap_bool.
ENDCLASS.



CLASS zcl_odata_client IMPLEMENTATION.


  METHOD construct.
    result ?= NEW zcl_odata_client( ).
  ENDMETHOD.


  METHOD zif_odata_client~delete.

    DATA(lv_url) = url.
    IF json = abap_true.
      lv_url = add_json_to_url( url ).
    ENDIF.
    IF xml = abap_true.
      json = abap_false.
      lv_url = add_xml_to_url( url ).
    ENDIF.
    IF stats = abap_true.
      lv_url = add_stats_to_url( url ).
    ENDIF.
    IF csrf_token IS NOT INITIAL.
      DATA(csrf) = csrf_token.
    ELSE.
      csrf = zif_odata_client~get_csrf_token( ).
    ENDIF.
    DATA(header_modified) = header.
    INSERT VALUE #( name = zif_odata_client=>x_csrf_token value = csrf ) INTO TABLE header_modified.
    TRY.
        result ?= delete(
                    url     = lv_url
                    header  = header_modified
                    timeout = timeout ).
      CATCH zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_client~get.

    DATA(lv_url) = url.
    IF json = abap_true.
      lv_url = add_json_to_url( url ).
    ENDIF.
    IF xml = abap_true.
      json = abap_false.
      lv_url = add_xml_to_url( url ).
    ENDIF.
    IF stats = abap_true.
      lv_url = add_stats_to_url( url ).
    ENDIF.

    TRY.
        result ?= get(
                        url     = lv_url
                        header  = header
                        timeout = timeout ).
      CATCH zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_client~get_csrf_token.

    TRY.
        IF url IS NOT INITIAL.
          DATA(input_url) = url.
        ELSE.
          input_url = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/ServiceCollection`.
        ENDIF.

        DATA(response) = zif_odata_client~get(
                                    url    = input_url
                                    header = VALUE #( ( name = zif_odata_client=>x_csrf_token value = 'fetch' ) ) ).
        READ TABLE response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = zif_odata_client=>x_csrf_token.
        IF sy-subrc = 0.
          result = <header>-value.
        ENDIF.
      CATCH zcx_rest_client INTO DATA(exception).
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_client~post.

    DATA(lv_url) = url.
    IF json = abap_true.
      lv_url = add_json_to_url( url ).
    ENDIF.
    IF xml = abap_true.
      json = abap_false.
      lv_url = add_xml_to_url( url ).
    ENDIF.
    IF stats = abap_true.
      lv_url = add_stats_to_url( url ).
    ENDIF.

    IF csrf_token IS NOT INITIAL.
      DATA(csrf) = csrf_token.
    ELSE.
      csrf = zif_odata_client~get_csrf_token( ).
    ENDIF.
    DATA(header_modified) = header.
    INSERT VALUE #( name = zif_odata_client=>x_csrf_token value = csrf ) INTO TABLE header_modified.

    TRY.
        result ?= post(
                        url     = lv_url
                        header  = header_modified
                        body    = body
                        timeout = timeout ).
      CATCH zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_client~put.

    DATA(lv_url) = url.
    IF json = abap_true.
      lv_url = add_json_to_url( url ).
    ENDIF.
    IF xml = abap_true.
      json = abap_false.
      lv_url = add_xml_to_url( url ).
    ENDIF.
    IF stats = abap_true.
      lv_url = add_stats_to_url( url ).
    ENDIF.

    IF csrf_token IS NOT INITIAL.
      DATA(csrf) = csrf_token.
    ELSE.
      csrf = zif_odata_client~get_csrf_token( ).
    ENDIF.
    DATA(header_modified) = header.
    INSERT VALUE #( name = zif_odata_client=>x_csrf_token value = csrf ) INTO TABLE header_modified.

    TRY.
        result ?= put(
                        url     = lv_url
                        header  = header_modified
                        body    = body
                        timeout = timeout ).
      CATCH zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_client~set_json.
    me->json = abap_true.
  ENDMETHOD.


  METHOD zif_odata_client~set_stats_true.
    me->stats = abap_true.
  ENDMETHOD.


  METHOD zif_odata_client~set_xml.
    me->xml = abap_true.
  ENDMETHOD.
ENDCLASS.
