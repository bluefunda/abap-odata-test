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
CLASS zcl_odata_test DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_odata_test .

    CLASS-METHODS new
      RETURNING
        VALUE(result) TYPE REF TO zif_odata_test .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA test_client TYPE REF TO zif_odata_test.
    DATA odata_client TYPE REF TO zif_odata_client.
    METHODS modify_url_with_vars
      IMPORTING url           TYPE string
                vars          TYPE zif_odata_test=>t_vars
      RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_odata_test IMPLEMENTATION.


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
    READ TABLE me->zif_odata_test~response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'content-type'.
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

    READ TABLE me->zif_odata_test~response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = zif_odata_client=>x_csrf_token.
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
    READ TABLE me->zif_odata_test~response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'content-type'.
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
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->zif_odata_test~response->body ).
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
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->zif_odata_test~response->body ).
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
      act = me->zif_odata_test~response->code ).

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
        IF stats = abap_true.
          odata_client->set_stats_true( ).
        ENDIF.
        IF vars IS NOT INITIAL AND vars IS SUPPLIED.
          DATA(url_modified) = modify_url_with_vars(
                                 url  = url
                                 vars = vars
                               ).
        ELSE.
          url_modified = url.
        ENDIF.

        zif_odata_test~response = odata_client->get(
                            url     = url_modified
                            header  = header_local
                            timeout = timeout ).

      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_odata_test~delete.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result = me->test_client.

    TRY.
        IF csrf_token IS NOT INITIAL.
          DATA(csrf) = csrf_token.
        ELSE.
          csrf = zif_odata_test~get_csrf_token( ).
        ENDIF.
        IF stats = abap_true.
          odata_client->set_stats_true( ).
        ENDIF.
        zif_odata_test~response = odata_client->delete(
                            url     = url
                            header  = header
                            csrf_token = csrf
                            timeout = timeout ).
      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_odata_test~post.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result = me->test_client.

    TRY.
        IF csrf_token IS NOT INITIAL.
          DATA(csrf) = csrf_token.
        ELSE.
          csrf = zif_odata_test~get_csrf_token( ).
        ENDIF.
        IF stats = abap_true.
          odata_client->set_stats_true( ).
        ENDIF.
        zif_odata_test~response = odata_client->post(
                            url     = url
                            header  = header
                            body    = body
                            csrf_token = csrf
                            timeout = timeout ).
      CATCH BEFORE UNWIND zcx_rest_client INTO DATA(exception).
        RAISE EXCEPTION NEW zcx_odata_client( previous = exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_odata_test~put.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result = me->test_client.

    TRY.
        IF csrf_token IS NOT INITIAL.
          DATA(csrf) = csrf_token.
        ELSE.
          csrf = zif_odata_test~get_csrf_token( ).
        ENDIF.
        IF stats = abap_true.
          odata_client->set_stats_true( ).
        ENDIF.
        zif_odata_test~response = odata_client->put(
                            url     = url
                            header  = header
                            body    = body
                            csrf_token = csrf
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
        DATA(json_body) = zcl_ajson=>parse( iv_json = me->zif_odata_test~response->body ).
        result = json_body->get( json_path  ).

      CATCH zcx_ajson_error INTO DATA(exception).
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD zif_odata_test~perf_stats.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.

    READ TABLE me->zif_odata_test~response->header ASSIGNING FIELD-SYMBOL(<header>) WITH KEY name = 'sap-statistics'.
    IF <header> IS ASSIGNED.
      DATA perf_stats TYPE TABLE OF string.
      SPLIT <header>-value AT ',' INTO TABLE perf_stats.
      LOOP AT perf_stats ASSIGNING FIELD-SYMBOL(<st>).
        SPLIT <st> AT '=' INTO DATA(name) DATA(value).
        CASE name.
          WHEN 'total'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'total'
                                    desc = 'Total processing time'
                                    value = value ) INTO TABLE result.
          WHEN 'fw'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'fw'
                                    desc = 'Framework'
                                    value = value ) INTO TABLE result.
          WHEN 'app'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'app'
                                    desc = 'Application'
                                    value = value ) INTO TABLE result.
          WHEN 'gwtotal'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'gwtotal'
                                    desc = 'Total processing time of the OData request'
                                    value = value ) INTO TABLE result.
          WHEN 'gwhub'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'gwhub'
                                    desc = 'Processing time in SAP Gateway hub system'
                                    value = value ) INTO TABLE result.
          WHEN 'gwrfcoh'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'gwrfcoh'
                                    desc = 'RFC and network overhead for communication between the hub and backend system' value = value ) INTO TABLE result.
          WHEN 'gwbe'.
            INSERT VALUE zif_odata_test~ty_stats(
                                     name = 'gwbe'
                                     desc = 'Processing time in SAP Gateway framework in backend system (without application time)Processing time in SAP Gateway framework in backend system (without application time)'
                                     value = value ) INTO TABLE result.
          WHEN 'gwapp'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'gwapp'
                                    desc = 'Processing time in application (data provider)'
                                    value = value ) INTO TABLE result.
          WHEN 'gwnongw'.
            INSERT VALUE zif_odata_test~ty_stats(
                                    name = 'gwnongw'
                                    desc = 'Processing time of applications called (referred to as non SAP Gateway since this processing time is not related to the SAP Gateway framework)'
                                    value = value ) INTO TABLE result.
          WHEN OTHERS.
        ENDCASE.
        CLEAR: name, value.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD zif_odata_test~assert_total_time.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.

    test_client ?= me.
    result = me->test_client.

    READ TABLE zif_odata_test~perf_stats( ) INTO DATA(stat) WITH KEY name = 'total'.
    IF sy-subrc = 0 AND stat IS NOT INITIAL.
      DATA(within_limits) = xsdbool( time > stat-value ).
    ELSE.
      DATA(stats_not_captured) = abap_true.
    ENDIF.

    IF within_limits = abap_true.

      RETURN.
    ENDIF.

    IF stats_not_captured = abap_true. "stats should be set to true by default;
      "if it is not, then it is intentionally set as "false"
      cl_aunit_assert=>fail( ).
    ENDIF.

    cl_aunit_assert=>fail( ).

  ENDMETHOD.

  METHOD modify_url_with_vars.

    LOOP AT vars ASSIGNING FIELD-SYMBOL(<var>).
      result = replace( val = url sub = `{` with = `'` ).
      result = replace( val = result sub = <var>-key with = <var>-value ).
      result = replace( val = result sub = `}` with = `'`  ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
