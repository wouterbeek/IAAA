:- module(
  iaaa_style,
  [
    sep//0
  ]
).

/** <module> IAAA style

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(html/html_default)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- multifile(user:body//2).
:- multifile(user:head//2).

user:body(iaaa, Content) -->
  user:body(default, Content).

user:head(iaaa, Content) -->
  user:head(default, html([\html_requires(css('iaaa.css'))|Content])).





%! sep// is det.
% Simple text-based separator.

sep -->
  html(" - ").
