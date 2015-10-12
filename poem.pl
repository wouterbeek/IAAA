:- module(
  poem,
  [
    poem//3 % :Author_2
            % :Title_2
            % +Stanzas:list(list(string))
  ]
).

/** <module> Poem

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(http/html_write)).

:- html_meta(poem(html,html,+,?,?)).





%! poem(:Author_2, :Title_2, +Stanzas:list(list(string)))// is det.

poem(Author_2, Title_2, L) -->
  html(
    div(class(poem), [
      div(class(head), [
        div(class(title), Title_2),
        div(class(author), Author_2)
      ]),
      \stanzas(L)
    ])
  ).

stanzas([]) --> !, html([]).
stanzas([H|T]) --> stanza(H), stanzas(T).

stanza(L) --> html(div(class(stanza), \lines(L))).

lines([]) --> !, html([]).
lines([H|T]) --> line(H), lines(T).

line(H) --> html(div(class(line), H)).
