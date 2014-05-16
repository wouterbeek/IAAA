:- module(
  poem,
  [
    html_integer_size_sequence//1, % +IntegerSequence:list(nonneg)
    poem_dom/4 % +Title:atom
               % +Author:atom/atom
               % +Sentences:list(atom)
               % -Poem:list
  ]
).

/** <module> POEM

@author Wouter Beek
@version 2013/04, 2014/05
*/

:- use_module(standards(css), [attribute_value/3 as css_attribute_value]).
:- use_module(xml(xml)).



%! html_integer_sequence(+Sequence:list(integer)) is det.

html_integer_size_sequence([]) --> !, [].
html_integer_size_sequence([H|T]) -->
  FontSize is 100 + 10 * H,
  atomic_list_concat([FontSize,'%'], FontSize_pct),
  css_attribute_value('font-size', FontSize_pct, Style),
  html([
    span([style=Style], [H]),
    \html_integer_size_sequence(T)
  ]).


poem_dom(
  Title,
  FirstName/LastName,
  Sentences
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
  stylesheet_pi(css('poem.css'), Stylesheet_PI),
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

  ]
).

