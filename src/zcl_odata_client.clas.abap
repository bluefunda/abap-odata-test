CLASS zcl_odata_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE INHERITING FROM zcl_rest_client_abs.

  PUBLIC SECTION.

    INTERFACES zif_odata_client .
    CLASS-METHODS construct RETURNING VALUE(result) TYPE REF TO zif_odata_client.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA json TYPE abap_bool.
    DATA xml TYPE abap_bool.
    DATA stats TYPE abap_bool.
ENDCLASS.



CLASS zcl_odata_client IMPLEMENTATION.


  METHOD zif_odata_client~set_json.
    me->json = abap_true.
  ENDMETHOD.


  METHOD zif_odata_client~set_stats_true.
    me->stats = abap_true.
  ENDMETHOD.


  METHOD zif_odata_client~set_xml.
    me->xml = abap_true.
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

    result ?= delete(
                url = lv_url
                header = header
                timeout = timeout ).
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

    result ?= get(
                            url = lv_url
                            header = header
                            timeout = timeout ).
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

    result ?= post(
                        url = lv_url
                        header = header
                        body = body
                        timeout = timeout ).
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

    result ?= put(
                       url = lv_url
                       header = header
                       body = body
                       timeout = timeout ).
  ENDMETHOD.

  METHOD construct.
    result ?= NEW zcl_odata_client( ).
  ENDMETHOD.

  METHOD zif_odata_client~get_csrf_token.

    TRY.
        DATA(url) = `/sap/opu/odata/IWFND/CATALOGSERVICE;v=2/ServiceCollection`.

        DATA(response) = zif_odata_client~get(
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

ENDCLASS.
