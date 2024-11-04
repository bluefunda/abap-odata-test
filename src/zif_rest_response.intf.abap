INTERFACE zif_rest_response PUBLIC .
  INTERFACES if_http_response.
*  TYPES: BEGIN OF ty_status,
*           code   TYPE i,
*           reason TYPE string,
*         END OF ty_status.
*  METHODS get_status
*    RETURNING VALUE(result) TYPE ty_status.
*  METHODS get_header
*    RETURNING VALUE(result) TYPE tihttpnvp.
*  METHODS get_body
*    RETURNING VALUE(result) TYPE string.
ENDINTERFACE.
