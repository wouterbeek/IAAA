:- module(
  schaakbord,
  [
    schaakbord_web/1 % -Markup:list
  ]
).

/** <module> Schaakbord

Zouct, ende vindt hier, met staden,
Acht ende dertigh Baladen.

Tendimus huc omnes / (Naar) hier gaan we allemaal

Hec est domus ultima / Dit is het laatste huis

@author Wouter Beek
@version 2011, 2012/10, 2013/03, 2014/05
@tbd Add CSS: (1) cell height = cell width, (2) per-cell highlight.
*/

:- use_module(generics(list_ext)).
:- use_module(html(html)).
:- use_module(math(math_ext)). % Used by meta-predicates.
:- use_module(svg(svg)).



board_web([SVG_Root]):-
  Height = 2.5,
  Width = 2.5,
  XBorder = 0.5,
  YBorder = 0.5,

  schaakboard(Rows),

  atomic_list_concat([Height,cm], Height_cm),
  atomic_list_concat([Width,cm], Width_cm),
  findall(
    element(
      rect,
      [
        fill=white,
        height=Height_cm,
        stroke=black,
        'stroke-width'='0.05cm',
        width=Width_cm,
        x=X_cm,
        y=Y_cm
      ],
      []
    ),
    (
      nth0(I, Rows, Row),
      nth0(J, Row, _Sentence),
      X is I * Height + XBorder,
      atomic_list_concat([X,cm], X_cm),
      Y is J * Width + YBorder,
      atomic_list_concat([Y,cm], Y_cm)
    ),
    SVG_Body
  ),

  Height0 is (Height * 8) + (YBorder * 2),
  Width0 is (Width * 8) + (XBorder * 2),
  svg_head(Height0, Width0, SVG_Head),
  root_element(svg, SVG_Head, SVG_Body, SVG_Root).

horse(X/Y, NewX/NewY):-
  combination([succ,dsucc,pred,dpred], 2, Preds),
  member(dsucc, Preds),
  member(dpred, Preds),
  Preds = [Pred1, Pred2],
  Call1 =.. [Pred1, X, NewX],
  call(Call1),
  Call2 =.. [Pred2, Y, NewY],
  call(Call2).

% line(?Row, ?Column, ?String, ?RhymeCategory)

% 4.096
line(1, 1, 'Beterd dee man en wijf', ijf).
line(1, 2, 'Vreest hu voor thelsche vier', ier).
line(1, 3, 'Beschudt ziele ende lijf', ijf).
line(1, 4, 'Die pooght naer thelsch dangier', ier).
line(1, 5, 'Ghy moedt zeer curts van hier', ier).
line(1, 6, 'Dees blyschap moet ghy deeruen', uren).
line(1, 7, 'De dood besprijnghd u schier', ier).
line(1, 8, 'Peinst ghy zuld moeten steeruen', uren).

line(2, 1, 'Die zondigt telker spacien', ie).
line(2, 2, 'Vvacht hu voor deeuwigh blaken', aken).
line(2, 3, 'Naer dees eerdsche lammatie', ie).
line(2, 4, 'Droufheid zal dy ghenaken', aken).
line(2, 5, 'Ghedijnckt vp tcas van wraken', aken).
line(2, 6, 'Hu naeckt een ander deel', eel).
line(2, 7, 'Vvee, die Gods wet verbraken', aken).
line(2, 8, 'Daer en wert gheen adpeel', eel).

line(3, 1, 'Broosch, snood, ende keitijf', ijf).
line(3, 2, 'Ghy beydt de dood onghier', ier).
line(3, 3, 'Ghi en hebd hier gheen blijf', ijf).
line(3, 4, 'Beterd hu valsch bestier', ier).
line(3, 5, 'En weest dogh niet zo fier', ier).
line(3, 6, 'Coopt hier des hemels eeruen', uen).
line(3, 7, 'Hoe quaed hoe butertier', ier).
line(3, 8, 'Elck moet doordeel verweeruen', uen).

line(4, 1, 'Hoopt op Gods groote gracie', ie).
line(4, 2, 'Vvat dijnge waent ghy maken', aken).
line(4, 3, 'Zouckt elders habitatie', ie).
line(4, 4, 'Dijn leuen zal dy laken', aken).
line(4, 5, 'Ghy moedt de dood gesmaken', aken).
line(4, 6, 'Ontsiet tgemeen morseel', eel).
line(4, 7, 'Tsijn hier al broossche staken', aken).
line(4, 8, 'Int tsweerelds schoon casteel', eel).

line(5, 1, 'Ontvlied dees recreatie', ie).
line(5, 2, 'Laedt weldaed dy gheraken', aken).
line(5, 3, 'Pooght om Gods hoogste statie', ie).
line(5, 4, 'Anxt sal op hu becraken', aken).
line(5, 5, 'Vvild dees ghenoughte slaken', aken).
line(5, 6, 'Vvanhaeghd dit eerdsch tenneel', eel).
line(5, 7, 'Mijdt hu van idel zaken', aken).
line(5, 8, 'Hier en blijft niet gheheel', eel).

line(6, 1, 'Laedt dy dees vruegt verleeden', eden).
line(6, 2, 'Hacht op Gods grooten toren', oren).
line(6, 3, 'Met duegd wild hu vercleeden', eden).
line(6, 4, 'Vvacht hu voor thelsg verstoren', oren).
line(6, 5, 'Als zidy hooghe geboren', oren).
line(6, 6, 'Als es dijn schoonheid groot', oot).
line(6, 7, 'De dood zalt al versmoren', oren).
line(6, 8, 'Tsy precieus oft snoot', oot).

line(7, 1, 'Ghi en hebd hier gheen statie', ie).
line(7, 2, 'Laedt u ter dood niet vaken', aken).
line(7, 3, 'Curt werdt hier hu fundatie', ie).
line(7, 4, 'Versaeckt dees helsce draken', aken).
line(7, 5, 'Ter dueght steld al dijn haken', aken).
line(7, 6, 'Schuud tsweerelds loos riueel', eel).
line(7, 7, 'Ghy moedt tot Gode waken', aken).
line(7, 8, 'Als schijnd ghi frisch iuweel', eel).

line(8, 1, 'Ghi moedt van hier verscheeden', eden).
line(8, 2, 'Die quaed doet werdt verloren', oren).
line(8, 3, 'Ghi muegd hu wel bereeden', eden).
line(8, 4, 'Met die God toebehoren', oren).
line(8, 5, 'Ghy werdt hier naer vercoren', oren).
line(8, 6, 'Ontgaed tviands exploot', oot).
line(8, 7, 'Ick seghd u van te voren', oren).
line(8, 8, 'Dijnckt om die bitter doot', oot).

schaakboard(Rows):-
  findall(
    Row,
    (
      between(1, 8, I),
      findall(
        Element,
        (
          between(1, 8, J),
          line(I, J, Element, _Ending)
        ),
        Row
      )
    ),
    Rows
  ).

schaakbord_web(Markup):-
  schaakboard(Rows),
  list_to_table([header(false)], Rows, Markup).

