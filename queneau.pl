:- module(queneau, []).

/** <module> Queneau

1,000,000,000,000,000 by Raymond Queneau.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- use_module(iaaa_style).
:- use_module(poem).

:- http_handler('/queneau', queneau, [prefix]).





%! queneau(+Request:list(compound)) is det.
% The URI-path version of the Queneau poem generator.

queneau(Req):-
  (   memberchk(path_info(Path0), Req),
      atom_concat(/, Path, Path0),
      sub_atom(Path, 0, 14, _, Title)
  ->  true
  ;   random_queneau_title(Title)
  ),
  reply_html_page(iaaa, title(Title), \queneau(Title)).



%! queneau(+Title:atom)// is det.
% Generates one of the poems.

queneau(Title) -->
  {
    atom_digits(Title, Ns),
    findall(L, (nth1(I, Ns, N), line(N, I, L)), Ls),
    sonnet_lines_stanzas(Ls, Ss)
  },
  poem(html("Raymond Queneau"), \integer_size_sequence(Ns), Ss).



%! integer_sequence(+Sequence:list(nonneg)) is det.

integer_size_sequence([]) --> !, [].
integer_size_sequence([H|T]) -->
  {
    FontSize is 100 + 10 * H,
    format(atom(Style), 'font-size: ~d%', [FontSize])
  },
  html([
    span(style(Style), H),
    \integer_size_sequence(T)
  ]).



%! random_queneau_title(-Title:atom) is det.

random_queneau_title(Title):-
  length(Ns, 14),
  maplist(random_between(0, 9), Ns),
  atomic_list_concat(Ns, Title).



%! sonnet_lines_stanzas(?Lines:list(string), ?Stanzas:list(list(string))) is det.

sonnet_lines_stanzas([], []):- !.
% Prince.
sonnet_lines_stanzas([L1,L2], [[L1,L2]]):- !.
sonnet_lines_stanzas([L1,L2,L3,L4|T1], [[L1,L2,L3,L4]|T2]):-
  sonnet_lines_stanzas(T1, T2).





% DB %

% line(?Choice:between(0,9), ?Index:between(1,14), ?Content:atom) is nondet.

line(M, N, L):-
  is_line_det(M, N, L), !,
  once(line0(M, N, L)).
line(M, N, L):-
  line0(M, N, L).

is_line_det(M, N, _):-
  maplist(ground, [M,N]), !.
is_line_det(_, _, L):-
  ground(L).

line0(1, 1, 'Don Pedro from his shirt has washed the fleas').
line0(2, 1, 'The wild hose champs the Pantheon\'s top frieze').
line0(3, 1, 'At snuff no Cornish sailorman would sneeze').
line0(4, 1, 'At five precisely out went La Marquise').
line0(5, 1, 'From playboy Chance the nymph no longer flees').
line0(6, 1, 'He bent right down to pick up his valise').
line0(7, 1, 'When one with t\'other straightaway agrees').
line0(8, 1, 'Prose took the minstrel\'s verse without a squeeze').
line0(9, 1, 'The acid tongue with gourmet\'s expertise').
line0(0, 1, 'The marble tomb gapes wide with jangling keys').

line0(1, 2, 'The bull\'s horns ought to fry it like a bone').
line0(2, 2, 'Since Elgin left his nostrils in the stone').
line0(3, 2, 'His nasal ecstasy beats best Cologne').
line0(4, 2, 'For tea cucumber sandwiches a scone').
line0(5, 2, 'Through snobbish growing round her hemline zone').
line0(6, 2, 'That hordes of crooks felt they\'d more right to own').
line0(7, 2, 'The answer is they could be twins full-grown').
line0(8, 2, 'His exaltation shocked both youth and crone').
line0(9, 2, 'Licks round carved marble chops on snails full-blown').
line0(0, 2, 'When masons clutch the breath we held on loan').

line0(1, 3, 'Old corned-beef\'s rusty armor spreads disease').
line0(2, 3, 'The Turks said just take anything you please').
line0(3, 3, 'Upon his old oak chest he cuts his cheese').
line0(4, 3, 'Her native chauffeur waited in the breeze').
line0(5, 3, 'His toga rumpled high above his knees').
line0(6, 3, 'He bent right down and well what did he seize').
line0(7, 3, 'Replies like this the dumbstruck brain may tease').
line0(8, 3, 'The understanding critic firstly sees').
line0(9, 3, 'The snowman gargles fire and sword with ease').
line0(0, 3, 'Forms shadowy with indecision wheeze').

line0(1, 4, 'That suede ferments is not is not at all well known').
line0(2, 4, 'And loudly sang off-key without a tone').
line0(3, 4, 'With cherry-pips his cottage floor is sown').
line0(4, 4, 'Which neither time nor tide can long postpone').
line0(5, 4, 'One gathers rosebuds or grows old alone').
line0(6, 4, 'The thumb- and finger-prints of Al Capone').
line0(7, 4, 'Normal one aims to be and share the throne').
line0(8, 4, '\'Ere meanings new to ancient tribes are thrown').
line0(9, 4, 'While sharks to let\'s say potted shrimps are prone').
line0(0, 4, 'And empty cages show life\'s bird has flown').

line0(1, 5, 'To one sweet hour of bliss my memory clings').
line0(2, 5, 'O Parthenon you hold the charger\'s string').
line0(3, 5, 'The Frisian Isles my friends are cherished things').
line0(4, 5, 'How it surprised us pale grey underlings').
line0(5, 5, 'Old Galileo\'s Pisan offerings').
line0(6, 5, 'Oh how oh how he hates such pilferings').
line0(7, 5, 'And yet \'twas he the beggar Fate just flings').
line0(8, 5, 'They both are right not untamed mutterings').
line0(9, 5, 'The roundabout eats profits made on swings').
line0(0, 5, 'It\'s one of many horrid happenings').

line0(1, 6, 'Signaling gauchos very rarely shave').
line0(2, 6, 'The North Wind bites into his architrave').
line0(3, 6, 'Whose ocean still-born herrings madly brave').
line0(4, 6, 'When flame a form to wrath ancestral gave').
line0(5, 6, 'Were pots graffiti\'d over by a slave').
line0(6, 6, 'Filching the lolly country thrift helped save').
line0(7, 6, 'Rejecting ermine to become a knave').
line0(8, 6, 'That metered rhyme alone can souls enslave').
line0(9, 6, 'Nought can the mouse\'s timid nibbling stave').
line0(0, 6, 'With somber thoughts they grimly line the nave').

line0(1, 7, 'An icicle of frozen marrow pings').
line0(2, 7, 'Th\'outrageous Thames a troubled arrow slings').
line0(3, 7, 'Such merchandise a melancholy brings').
line0(4, 7, 'A daring baron pockets precious Mings').
line0(5, 7, 'The leaning linguist cameramaniac sings').
line0(6, 7, 'He\'s gone to London how the echo rings').
line0(7, 7, 'The fertile mother changelings drops like kings').
line0(8, 7, 'They both are right not unformed smatterings').
line0(9, 7, 'In salads all chew grubs before they\'ve wings').
line0(0, 7, 'Proud death quite il-le-gi-ti-mate-ly strings').

line0(1, 8, 'As sleeping-bags the silent landscape pave').
line0(2, 8, 'To break a rule Britannia\'s might might waive').
line0(3, 8, 'For burning bushes never fish forgave').
line0(4, 8, 'Till firemen come with hose-piped tidal wave').
line0(5, 8, 'Etruscan words which Greece and Rome engrave').
line0(6, 8, 'Through homestead hillside woodland rock and cave').
line0(7, 8, 'In purest cradles tha\'s how they behave').
line0(8, 8, 'That every verbal shock aims to deprave').
line0(9, 8, 'The nicest kids for stickiest toffees crave').
line0(0, 8, 'Victorious worms grind all into the grave').

line0(1, 9, 'Staunch pilgrims longest journeys can\'t depress').
line0(2, 9, 'Platonic Greece was not so talentless').
line0(3, 9, 'When dried the terrapin can naught express').
line0(4, 9, 'The fasting fakir doesn\'t smell the less').
line0(5, 9, 'Emboggled minds may puff and blow and guess').
line0(6, 9, 'The peasant\'s skirts on rainy days she\'d tress').
line0(7, 9, 'The genealogist with field and fess').
line0(8, 9, 'Poetic license needs no strain or stress').
line0(9, 9, 'The wolf devours both sheep and shepherdess').
line0(0, 9, 'It\'s no good rich men crying Heaven Bless').

line0(1, 10, 'What things we did we went the whole darned hog').
line0(2, 10, 'A piercing wit would sprightliest horses flog').
line0(3, 10, 'Shallots and sharks\' fins face the smould\'ring log').
line0(4, 10, 'In Indian summers Englishmen drink grog').
line0(5, 10, 'With gravity at gravity\'s great cog').
line0(6, 10, 'And starve the sniveling baby like a dog').
line0(7, 10, 'With quill white-collared through his life will jog').
line0(8, 10, 'One tongue will do to keep the verse agog').
line0(9, 10, 'A bird-brain banquet melts bold Mistress Mog').
line0(0, 10, 'Or grinning like a pale-faced golliwog').

line0(1, 11, 'And played their mountain croquet jungle chess').
line0(2, 11, 'Socrates watched his hemlock effervesce').
line0(3, 11, 'While homeward thirsts to each quenched glass say yes').
line0(4, 11, 'The colonel\'s still escutcheoned in undress').
line0(5, 11, 'On wheels the tourist follows his hostess').
line0(6, 11, 'Watching manure and compost coalesce').
line0(7, 11, 'To prove mamma an adult with a tress').
line0(8, 11, 'From cool Parnassus down to wild Loch Ness').
line0(9, 11, 'The country lane just thrives on farmyard mess').
line0(0, 11, 'Poor Yorick comes to bury not address').

line0(1, 12, 'Southern baroque\'s seductive dialogue').
line0(2, 12, 'Their sculptors did our best our hulks they clog').
line0(3, 12, 'Lobsters for sale must be our apologue').
line0(4, 12, 'No need to cart such treasures from the fog').
line0(5, 12, 'With breaking voice across the Alps they slog').
line0(6, 12, 'One misses cricket hearth and croaking frog').
line0(7, 12, 'But I can understand you Brother Gog').
line0(8, 12, 'Bard I adore your endless monologue').
line0(9, 12, 'Whiskey will always wake an Irish bog').
line0(0, 12, 'We\'ll suffocate before the epilogue').

line0(1, 13, 'Suits lisping Spanish tongues for whom say some').
line0(2, 13, 'With marble souvenirs then fill a slum').
line0(3, 13, 'On fish-slab whale nor seal has ever swum').
line0(4, 13, 'The Taj Mahal has trinkets spice and gum').
line0(5, 13, 'Do bank clerks rule their abacus by thumb?').
line0(6, 13, 'Where no one bothered how one warmed one\'s bum').
line0(7, 13, 'And let you off from your opinions glum').
line0(8, 13, 'Ventriloquists be blowed you strike me dumb').
line0(9, 13, 'Through bretzels take the dolls from board-room drum').
line0(0, 13, 'Poor reader smile before your lips go numb').

line0(1, 14, 'The bell tolls fee-less fi-less fo-less fum').
line0(2, 14, 'For Europe\'s glory while Fate\'s harpies strum').
line0(3, 14, 'They\'re kings we\'re mammal-cousins hi ho hum').
line0(4, 14, 'And lessors\' dates have all too short a sum').
line0(5, 14, 'In cognac brandy is Bacardi rum?').
line0(6, 14, 'Yet from the City\'s pie pulled not one plum').
line0(7, 14, 'A wise loaf always knows its humblest crumb').
line0(8, 14, 'Soliloquies predict great things old chum').
line0(9, 14, 'Fried grilled black pudding\'s still the world\'s best yum').
line0(0, 14, 'The best of all things to an end must come').





% HELPERS %

%! atom_digits(+Atom:atom, -Digits:list(between(0,9))) is det.
% Succeeds if Atom is a concatenation of characters that represent Digits.

atom_digits(A, Ns):-
  atom_chars(A, Chars),
  maplist(atom_number, Chars, Ns).
