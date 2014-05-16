:- module(
  html_sonnet,
  [
    html_sonnet//1 % +Lines:list(atom)
  ]
).

/** <module> HTML sonnet

Generates the HTML representation of a sonnet.

@author Wouter Beek
@version 
*/

:- use_module(library(http/html_write)).



%! sonnet(+Lines:list(atom))// is det.
% Generates the HTML representation of a sonnet.

sonnet([]) --> !, [].
sonnet([S1,S2]) --> !,
  html(p([S1,br(),S2])).
sonnet([S1,S2,S3,S4|T]) -->
  html([
    p([S1,br(),S2,br(),S3,br(),S4]),
    \sonnet(T)
  ]).

