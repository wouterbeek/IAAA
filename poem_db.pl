:- module(
  poem_db,
  [
    poem/4 % ?Name:atom
           % ?Title:string
           % ?Author:string
           % ?Stanzas:list(list(string))
  ]
).

/** <module> Poem DB

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(html/element/html_link)).
:- use_module(library(html/element/html_list)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(iaaa_style).
:- use_module(poem).

:- http_handler('/poem', poem, [prefix]).

:- initialization(load_poem_db).

%! poem(
%!   ?Name:atom,
%!   ?Title:string,
%!   ?Author:string,
%!   ?Stanzas:list(list(string))
%! ) is nondet.

:- dynamic(poem/4).
:- multifile(poem/4).





% HTTP HANDLERS %

poem(Req):-
  memberchk(path_info(Name0), Req),
  atomic_list_concat(['',Name], /, Name0),
  load_poem(Name),
  reply_html_page(iaaa, title(\poem_head(Name)), \poem(Name)).
poem(_):-
  reply_html_page(iaaa, title("Poem overview"), \poem_overview).





% HTML %

%! poem(+Name:atom)// is det.

poem(Name) -->
  {poem(Name, Title, Author, Stanzas)},
  poem(html(Title), html(Author), Stanzas).



%! poem_entry(+Name:atom)// is det.

poem_entry(Name) -->
  {http_link_to_id(poem, path_postfix(Name), Link)},
  html_link(Link, \(poem_db:poem_header(Name))).



%! poem_head(+Name:atom)// is det.

poem_head(Name) -->
  {poem(Name, Title, Author, _)},
  html(["Poem", \sep, Title, \sep, Author]).



%! poem_header(+Name:atom)// is det.

poem_header(Name) -->
  {poem(Name, Title, Author, _)},
  html([span(class(title), Title), \sep, span(class(author), Author)]).



%! poem_overview// is det.

poem_overview -->
  {aggregate_all(set(Name), poem(Name, _, _, _), Names)},
  html_list(Names, [item_writer(poem_entry),ordered(true)]).





% DB %

%! load_poem_db is det.

load_poem_db:-
  absolute_file_name('db/poem', Dir, [access(read),file_type(directory)]),
  directory_file_path(Dir, '*.pl', Wildcard),
  expand_file_name(Wildcard, Paths),
  maplist(ensure_loaded, Paths).



%! load_poem(+Name:atom) is det.
% Ensures that the poem with given Name is loaded.

load_poem(Name):-
  poem(Name, _, _, _), !.
load_poem(Name):-
  atomic_list_concat([db,poem,Name], /, SubPath),
  absolute_file_name(
    SubPath,
    File,
    [access(read),file_errors(fail),file_type(prolog)]
  ),
  ensure_loaded(File).
