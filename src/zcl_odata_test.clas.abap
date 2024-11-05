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
class ZCL_ODATA_TEST definition
  public
  final
  create private .

public section.

  interfaces ZIF_ODATA_TEST .

  class-methods NEW
    returning
      value(RESULT) type ref to ZIF_ODATA_TEST .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA test_client TYPE REF TO zif_odata_test.
    DATA odata_client TYPE REF TO zif_odata_client.
    DATA response TYPE REF TO zif_rest_client=>ty_response.
ENDCLASS.



CLASS ZCL_ODATA_TEST IMPLEMENTATION.


  METHOD new.
    result ?= NEW zcl_odata_test( ).
  ENDMETHOD.


  METHOD zif_odata_test~assert_content_type.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    " check content type
    READ TABLE me->response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'content-type'.
    IF <header> IS ASSIGNED.
      DATA(ct) = <header>-value.
    ENDIF.

    cl_aunit_assert=>assert_equals(
        exp                  = content_type
        act                  = ct ).

  ENDMETHOD.


  METHOD zif_odata_test~assert_csrf_token_not_initial.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    READ TABLE me->response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = zif_odata_client=>x_csrf_token.
    IF <header> IS ASSIGNED.
      DATA(csrf_token) = <header>-value.
    ENDIF.
    cl_aunit_assert=>assert_not_initial(
      act = csrf_token
    ).

  ENDMETHOD.


  METHOD zif_odata_test~assert_http_200.

    result ?= zif_odata_test~assert_status( status = `200` ).

  ENDMETHOD.


  METHOD zif_odata_test~assert_http_201.

    result ?= zif_odata_test~assert_status( status = `201` ).

  ENDMETHOD.


  METHOD zif_odata_test~assert_json_content_type.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    DATA: content_type_tt TYPE RANGE OF string.

    content_type_tt = VALUE #( sign = 'I' option = 'EQ'
   ( low = 'application/json; charset=utf-8' )
   ( low = 'application/json' ) ).

    " check content type
    READ TABLE me->response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'content-type'.
    IF <header> IS ASSIGNED.
      DATA(ct) = <header>-value.
    ENDIF.

    IF ct NOT IN content_type_tt.
      cl_aunit_assert=>fail( ).
    ENDIF.


  ENDMETHOD.


  METHOD zif_odata_test~assert_json_not_empty.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    TRY.
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->response->body ).
        IF json_body->is_empty( ) = abap_true.
          cl_aunit_assert=>fail( ).
        ENDIF.

      CATCH zcx_ajson_error INTO DATA(exception).
        cl_aunit_assert=>fail( ).
    ENDTRY.


  ENDMETHOD.


  METHOD zif_odata_test~assert_json_path_exists.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    TRY.
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->response->body ).
        IF json_body->exists( json_path  ) = abap_false.
          cl_aunit_assert=>fail( ).
        ENDIF.

      CATCH zcx_ajson_error INTO DATA(exception).
        cl_aunit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_test~assert_status.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result ?= me->test_client.

    cl_aunit_assert=>assert_equals(
      exp = status
      act = me->response->code ).

  ENDMETHOD.


  METHOD zif_odata_test~get.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result = me->test_client.

    TRY.
        DATA(header_local) = header.
        IF csrf_fetch = abap_true.
          INSERT VALUE #( name = zif_odata_client=>x_csrf_token value = 'fetch' )  INTO TABLE header_local.
        ENDIF.
        response = odata_client->get(
                            url     = url
                            header  = header_local
                            timeout = timeout ).
      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_odata_test~get_csrf_token.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.
    result = me->odata_client->get_csrf_token( ).

  ENDMETHOD.


  METHOD zif_odata_test~get_json_path_value.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.

    TRY.
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->response->body ).
        result = json_body->get( json_path  ).

      CATCH zcx_ajson_error INTO DATA(exception).
        CLEAR result.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
