% Load AAAI Web site.

:- use_module(library(html/html_resource)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(sb/sb_init)).
:- use_module(library(sb/sb_settings)).
:- use_module(library(settings)).

:- start_server.

:- load_settings('iaaa.db').

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, resource/css).
user:file_search_path(dtd, resource/dtd).

:- html_resource(css(poem), [requires([css('poem.css')]),virtual(true)]).

:- use_module(iaaa).
