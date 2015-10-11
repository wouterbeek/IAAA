:- module(iaaa, []).

/** <module> Poems Web

Web front-end for poems.

# Let's read a poem!

~~~{.txt}
Shall I compare thee to a summer's day?
Thou art more lovely and more temperate:
Rough winds do shake the darling buds of May,
And summer's lease hath all too short a date:
Sometime too hot the eye of heaven shines,
And often is his gold complexion dimm'd,
And every fair from fair sometime declines,
By chance, or nature's changing course untrimm'd:
But thy eternal summer shall not fade,
Nor lose possession of that fair thou ow'st,
Nor shall death brag thou wander'st in his shade,
When in eternal lines to time thou grow'st,
So long as men can breathe, or eyes can see,
So long lives this, and this gives life to thee.
~~~

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(html/html_default)).
:- use_module(library(html/element/html_link)).
:- use_module(library(html/element/html_list)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- http_handler(/, poem, [prefix]).





poem(Req):-
  memberchk(path_info(Name), Req),
  atomic_list_concat([db,poem,Name], /, SubPath),
  absolute_file_name(
    SubPath,
    File,
    [access(read),extensions([xml]),file_errors(fail)]
  ), !,
  load_html(File, Dom, []),
  reply_html_page(poem, html(\poem_heading(Dom)), Dom).
poem(_):-
  reply_html_page(poem, html('Poem overview'), html(\poem_overview)).

poem_overview -->
  {
    absolute_file_name('db/poem', Dir, [access(read),file_type(directory)]),
    directory_file_path(Dir, '*.xml', Wildcard),
    expand_file_name(Wildcard, Paths)
  },
  html([
    div(class(ptitle), 'Poems'),
    \html_list(Paths, [item_writer(poem_entry),ordered(true)])
  ]).

poem_entry(Path) -->
  {
    dtd(poem, Dtd),
    load_xml(Path, Dom, [dtd(Dtd)]),
    path_link(Path, Link)
  },
  html_link(Link, poem_heading(Dom)).



% HELPERS %

poem_author(Dom) -->
  {
    FN =.. ['first-name', text],
    xpath(Dom, //FN, FirstName),
    LN =.. ['last-name', text],
    xpath(Dom, //LN, LastName)
  },
  html([FirstName,' ',LastName]).

poem_heading(Dom) -->
  html([\poem_author(Dom),' - ',\poem_title(Dom)]).

path_link(Path, Link):-
  file_base_name(Path, Base),
  file_name_extension(Base0, xml, Base),
  http_link_to_id(poem, path_postfix(Base0), Link).

poem_title(Dom) -->
  {xpath(Dom, //ptitle(text), Title)},
  html(Title).



% HTML GENERATION %

user:body(poem, Content) -->
  user:body(default, Content).

user:head(poem, Content) -->
  user:head(default, html([\html_requires(css('poem.css'))|Content])).
