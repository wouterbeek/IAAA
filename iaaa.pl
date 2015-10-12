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

:- use_module(library(html/element/html_link)).
:- use_module(library(html/element/html_list)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(iaaa_style).
:- use_module(poem_db).
:- use_module(queneau).
:- use_module(schaakbord).

:- http_handler(/, iaaa, [prefix]).





% HTTP HANDLER %

%! iaaa(+Request:list(compound)) is det.

iaaa(_):-
  reply_html_page(iaaa,
    html(title(["IAAA",\sep,"Instititute for Artificial Art Amsterdam"])),
    html(
      \html_list(
        [poem,queneau,schaakbord],
        [item_writer(handler_link),ordered(false)]
      )
    )
  ).

handler_link(HandlerId) -->
  {http_link_to_id(HandlerId, [], Link)},
  html_link(Link, HandlerId).
