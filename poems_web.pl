:- module(
  poems_web,
  [
    poem_dom/4, % +Title:atom
                % +Author:atom/atom
                % +Sentences:list(atom)
                % -DOM:list
    poem_web/2, % +Title:atom
                % -Markup:list
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
@version 2012/12, 2013/02-2013/03
*/

:- use_module(generic(file_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(xpath)).
:- use_module(server(wallace)).
:- use_module(standards(http)).
:- use_module(standards(xml)).

:- reexport(poems(schaakbord)).
:- reexport(poems(queneau)).

:- html_resource(http_www_css('poem.css'), []).

:- http_handler(http_root(poem), poem, [prefix]).
:- http_handler(http_root(poem_dummy), poem_dummy, [prefix]).

% TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%    % This should be loaded on the fly.
%    \html_requires(css('poem.css'))


poem(Request):-
  poem_request_to_dom(Request, DOM),
  serve_xml(poem, poem, DOM).

poem_dummy(Request):-
  poem_request_to_dom(Request, DOM),
  push(console_output, poem, poem, DOM),
  serve_nothing(Request).

poem_dom(
  Title,
  FirstName/LastName,
  Sentences0,
  [
    Stylesheet_PI,
    element(poem, [], [
      element(phead, [], [
        element(ptitle, [], [Title]),
        element(author, [], [
          element(first-name, [], [FirstName]),
          element(last-name, [], [LastName])
        ])
      ]),
      element(pbody, [], Stanzas)
    ])
  ]
):-
  stylesheet_pi(http_www_css('poem.css'), Stylesheet_PI),
  findall(
    element(stanza, [], Lines),
    (
      member(Sentences, Sentences0),
      findall(
        element(line, [], [Sentence]),
        member(Sentence, Sentences),
        Lines
      )
    ),
    Stanzas
  ).

poem_request_to_dom(Request, DOM):-
  memberchk(path_info(Path0), Request),
  atom_concat('/', Path, Path0),
  absolute_file_name(poems(Path), File, [access(read), file_type(xml)]),
  file_to_xml(File, DOM).

poem_web(Title, poem/poem/DOM):-
  absolute_file_name(poems(Title), File, [access(read), file_type(xml)]),
  file_to_xml(File, DOM).

% How can we push the XML poems to Wallace without redrawing the entire page?
% push(console_output, poem, poem, DOM).
poems_web([element(ol, [], ListItems)]):-
  absolute_file_name(poems(.), Directory, [file_type(directory)]),
  path_walk_tree(Directory, '.*.xml$', Paths),
  findall(
    element(li, [], [element(a, [href=URI], [Author, ' - ', Title])]),
    (
      member(Path, Paths),
      file_to_xml(Path, DOM),
      FN =.. ['first-name', text],
      xpath(DOM, //FN, FirstName),
      LN =.. ['last-name', text],
      xpath(DOM, //LN, LastName),
      format(atom(Author), '~w ~w', [FirstName, LastName]),
      xpath(DOM, //ptitle(text), Title),
      wallace_uri(URI0),
      file_base_name(Path, Base),
      file_name_type(Base1, xml, Base),
      format(atom(URI), '~wpoem_dummy/~w', [URI0, Base1])
    ),
    ListItems
  ).

