:- module(
  poem,
  [
    poem_dom/4 % +Title:atom
               % +Author:atom/atom
               % +Sentences:list(atom)
               % -Poem:list
  ]
).

/** <module> POEM

@author Wouter Beek
@version 2013/04
*/

:- use_module(xml(xml)).



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

