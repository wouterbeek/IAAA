:- module(
  poems_web,
  [
    poems_web/1 % -Markup:list
  ]
).

/** <module> Poems Web

Web front-end for poems.

<sonnet>
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
</sonnet>

@author Wouter Beek
@version 2012/12, 2013/02-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_session)).
:- use_module(library(xpath)).
:- use_module(project(poem)).
:- use_module(project(schaakbord)).
:- use_module(project(queneau)).
:- use_module(server(wallace)).
:- use_module(server(web_console)).
:- use_module(standards(http)).
:- use_module(standards(xml)).

% Register the XML file type.
:- assert_novel(user:prolog_file_type(xml, xml)).

% Use modules in Web interface.
:- register_module(schaakbord).
:- register_module(queneau).

% Serve CSS files.
http:location(css, root(css), []).
:- assert(user:file_search_path(css, project(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Assert the project-specific DTD file location.
:- assert(user:file_search_path(dtd, project(dtd))).

% HTTP handler for the index.
:- http_handler(root(.), index, [prefix]).

% HTTP handler for displaying individual poems.
%http:location(poem, root(poem), []).
%:- http_handler(poem(.), poem, [prefix]).



index(Request):-
  memberchk(path_info(Path), Request),
  absolute_file_name(data(Path), File, [access(read), file_type(xml)]),
  file_to_xml(File, Poem),
  !,
  reply_html_page(poem, Poem, Poem).
index(_Request):-
  poems_web(Markup),
  reply_html_page(poems, [], Markup).

% How can we push the XML poems to Wallace without redrawing the entire page?
% push(console_output, poem, poem, DOM).
poems_web([element(ol, [], ListItems)]):-
  absolute_file_name(
    data(.),
    Directory,
    [access(read), file_type(directory)]
  ),
  path_walk_tree(Directory, '.*.xml$', Paths),
  findall(
    element(li, [], [element(a, [href=URI], [Author, ' - ', Title])]),
    (
      member(Path, Paths),
      file_to_xml(Path, Poem),
      FN =.. ['first-name', text],
      xpath(Poem, //FN, FirstName),
      LN =.. ['last-name', text],
      xpath(Poem, //LN, LastName),
      format(atom(Author), '~w ~w', [FirstName, LastName]),
      xpath(Poem, //ptitle(text), Title),
      file_base_name(Path, Base),
      file_name_type(Base1, xml, Base),
      http_absolute_location(root(Base1), URI, [])
    ),
    ListItems
  ).



% HTML GENERATION %

user:body(poem, Body1) -->
  {
    strip_module(Body1, _Module, Body2),
    Body2 = [_Style, Root],
    dom_to_html(Root, HTML)
  },
  html(body([\html_requires(css('poem.css')), HTML])).

user:body(poems, Body) -->
  html(
    body(
      [
        \html_requires(css('poem.css')),
        div(class(ptitle), 'Poems'),
        Body
      ]
    )
  ).

user:head(poem, Poem1) -->
  {
    strip_module(Poem1, _Module, Poem2),
    FN =.. ['first-name', text],
    xpath(Poem2, //FN, FirstName),
    LN =.. ['last-name', text],
    xpath(Poem2, //LN, LastName),
    format(atom(Author), '~w ~w', [FirstName, LastName]),
    xpath(Poem2, //ptitle(text), Title)
  },
  html(head(title([Author, ' - ', Title]))).

user:head(poems, Head) -->
  html(head(Head)).

dom_to_html(DOM, HTML):-
  is_list(DOM),
  !,
  maplist(dom_to_html, DOM, HTML),
  !.
dom_to_html(Atom, Atom):-
  atomic(Atom),
  !.
dom_to_html(
  element(Name, Attributes, DOM_Contents),
  div([class(Name) | Attributes], HTML_Contents)
):-
  dom_to_html(DOM_Contents, HTML_Contents).

