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
INTERFACE zif_odata_test PUBLIC .
  DATA response TYPE REF TO zif_rest_client=>ty_response.
  TYPES: BEGIN OF ty_stats,
           name  TYPE string,
           value TYPE string,
           desc  TYPE string,
         END OF ty_stats,
         t_stats TYPE SORTED TABLE OF ty_stats WITH UNIQUE KEY name.

  METHODS perf_stats
    RETURNING VALUE(result) TYPE t_stats.
  TYPES: BEGIN OF ty_vars,
           key   TYPE string,
           value TYPE string,
         END OF ty_vars,
         t_vars TYPE TABLE OF ty_vars.

  METHODS get
    IMPORTING url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
              csrf_fetch    TYPE abap_bool DEFAULT abap_false
              stats         TYPE abap_bool DEFAULT abap_true
              vars          TYPE t_vars OPTIONAL
    RETURNING VALUE(result) TYPE REF TO zif_odata_test
    RAISING   zcx_odata_client.
  METHODS post
    IMPORTING url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
              csrf_token    TYPE string OPTIONAL
              stats         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(result) TYPE REF TO zif_odata_test
    RAISING   zcx_odata_client.
  METHODS put
    IMPORTING url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
              csrf_token    TYPE string OPTIONAL
              stats         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(result) TYPE REF TO zif_odata_test
    RAISING   zcx_odata_client.
  METHODS delete
    IMPORTING url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
              csrf_token    TYPE string OPTIONAL
              stats         TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(result) TYPE REF TO zif_odata_test
    RAISING   zcx_odata_client.
  METHODS get_csrf_token
    RETURNING VALUE(result) TYPE string.
  METHODS assert_csrf_token_not_initial
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_status
    IMPORTING status        TYPE i
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_http_200
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_http_201
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_content_type
    IMPORTING content_type  TYPE string
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_json_content_type
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_json_not_empty
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_json_path_exists
    IMPORTING json_path     TYPE string
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS assert_total_time
    IMPORTING time          TYPE i "in milliseconds
    RETURNING VALUE(result) TYPE REF TO zif_odata_test.
  METHODS get_json_path_value
    IMPORTING json_path     TYPE string
    RETURNING VALUE(result) TYPE string.

ENDINTERFACE.
