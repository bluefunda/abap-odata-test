CLASS zcl_rest_client DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE INHERITING FROM zcl_rest_client_abs.

  PUBLIC SECTION.

    INTERFACES zif_rest_client .
    CLASS-METHODS construct
      IMPORTING internal_proxy TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result)  TYPE REF TO zif_rest_client.
  PROTECTED SECTION.
    METHODS constructor IMPORTING internal_proxy TYPE abap_bool.
  PRIVATE SECTION.
    DATA internal_proxy TYPE abap_bool.

ENDCLASS.



CLASS zcl_rest_client IMPLEMENTATION.

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

ENDCLASS.
