CLASS zcl_odata_test DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_odata_test .
    CLASS-METHODS new RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA test_client TYPE REF TO zif_odata_test.
    DATA odata_client TYPE REF TO zif_odata_client.
    DATA response TYPE REF TO zif_rest_client=>ty_response.
ENDCLASS.



CLASS zcl_odata_test IMPLEMENTATION.

  METHOD new.
    result ?= NEW zcl_odata_test( ).
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

  METHOD zif_odata_test~do_get.

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

    ENDTRY.

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

  METHOD zif_odata_test~get_csrf_token.

    IF me->odata_client IS NOT BOUND.
      me->odata_client = zcl_odata_client=>construct( ).
    ENDIF.
    result = me->odata_client->get_csrf_token( ).

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

ENDCLASS.
