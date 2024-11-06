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
INTERFACE zif_rest_client PUBLIC .
  TYPES: BEGIN OF ty_response,
           code            TYPE i,
           reason          TYPE string,
           header          TYPE tihttpnvp,
           body            TYPE string,
           response_object TYPE REF TO if_http_response,
         END OF ty_response.
  METHODS get
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS post
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS put
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              body          TYPE string
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS delete
    IMPORTING
              url           TYPE string
              header        TYPE tihttpnvp OPTIONAL
              timeout       TYPE i DEFAULT if_http_client=>co_timeout_default
    RETURNING VALUE(result) TYPE REF TO ty_response
    RAISING   RESUMABLE(zcx_rest_client).
  METHODS get_csrf_token
    RETURNING VALUE(result) TYPE string.
ENDINTERFACE.
