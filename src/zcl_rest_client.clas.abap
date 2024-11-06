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
class ZCL_REST_CLIENT definition
  public
  inheriting from ZCL_REST_CLIENT_ABS
  final
  create private .

public section.

  interfaces ZIF_REST_CLIENT .

  class-methods CONSTRUCT
    importing
      !INTERNAL_PROXY type ABAP_BOOL default ABAP_TRUE
    returning
      value(RESULT) type ref to ZIF_REST_CLIENT .
  PROTECTED SECTION.
    METHODS constructor IMPORTING internal_proxy TYPE abap_bool.
  PRIVATE SECTION.
    DATA internal_proxy TYPE abap_bool.

ENDCLASS.



CLASS ZCL_REST_CLIENT IMPLEMENTATION.


  METHOD construct.
    result ?= NEW zcl_rest_client( internal_proxy ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    me->internal_proxy = internal_proxy.
  ENDMETHOD.


  METHOD zif_rest_client~delete.
    result = delete(
            url = url
            header = header
            timeout = timeout ).
  ENDMETHOD.


  METHOD zif_rest_client~get.
    result = get(
                        url = url
                        header = header
                        timeout = timeout ).
  ENDMETHOD.


  METHOD zif_rest_client~get_csrf_token.

    TRY.
        DATA(url) = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/ServiceCollection`.

        DATA(response) = zif_rest_client~get(
                                    url    = url
                                    header = VALUE #( ( name = 'X-Csrf-Token' value = 'fetch' ) ) ).
        READ TABLE response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'x-csrf-token'.
        IF sy-subrc = 0.
          result = <header>-value.
        ENDIF.
      CATCH zcx_rest_client INTO DATA(exception).
        "handle exception
    ENDTRY.

  ENDMETHOD.


  METHOD zif_rest_client~post.
    result = post(
                    url = url
                    header = header
                    body = body
                    timeout = timeout ).
  ENDMETHOD.


  METHOD zif_rest_client~put.
    result = put(
                   url = url
                   header = header
                   body = body
                   timeout = timeout ).
  ENDMETHOD.
ENDCLASS.
