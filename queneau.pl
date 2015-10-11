:- module(
  queneau,
  [
    queneau/1, % +Request
    queneau/2, % -Title:atom
               % -DOM_Triple
    queneau_web/1, % -Markup:list
    random_queneau/1 % -DOM_Triple,
  ]
).

/** <module> Queneau

1,000,000,000,000,000 by Raymond Queneau.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- http_handler('/queneau', queneau, [prefix]).



% line(?Poem:integer, ?Line:integer, ?Sentence:atom) is nondet.

line(1, 1, 'Don Pedro from his shirt has washed the fleas').
line(2, 1, 'The wild hose champs the Pantheon\'s top frieze').
line(3, 1, 'At snuff no Cornish sailorman would sneeze').
line(4, 1, 'At five precisely out went La Marquise').
line(5, 1, 'From playboy Chance the nymph no longer flees').
line(6, 1, 'He bent right down to pick up his valise').
line(7, 1, 'When one with t\'other straightaway agrees').
line(8, 1, 'Prose took the minstrel\'s verse without a squeeze').
line(9, 1, 'The acid tongue with gourmet\'s expertise').
line(0, 1, 'The marble tomb gapes wide with jangling keys').

line(1, 2, 'The bull\'s horns ought to fry it like a bone').
line(2, 2, 'Since Elgin left his nostrils in the stone').
line(3, 2, 'His nasal ecstasy beats best Cologne').
line(4, 2, 'For tea cucumber sandwiches a scone').
line(5, 2, 'Through snobbish growing round her hemline zone').
line(6, 2, 'That hordes of crooks felt they\'d more right to own').
line(7, 2, 'The answer is they could be twins full-grown').
line(8, 2, 'His exaltation shocked both youth and crone').
line(9, 2, 'Licks round carved marble chops on snails full-blown').
line(0, 2, 'When masons clutch the breath we held on loan').

line(1, 3, 'Old corned-beef\'s rusty armor spreads disease').
line(2, 3, 'The Turks said just take anything you please').
line(3, 3, 'Upon his old oak chest he cuts his cheese').
line(4, 3, 'Her native chauffeur waited in the breeze').
line(5, 3, 'His toga rumpled high above his knees').
line(6, 3, 'He bent right down and well what did he seize').
line(7, 3, 'Replies like this the dumbstruck brain may tease').
line(8, 3, 'The understanding critic firstly sees').
line(9, 3, 'The snowman gargles fire and sword with ease').
line(0, 3, 'Forms shadowy with indecision wheeze').

line(1, 4, 'That suede ferments is not is not at all well known').
line(2, 4, 'And loudly sang off-key without a tone').
line(3, 4, 'With cherry-pips his cottage floor is sown').
line(4, 4, 'Which neither time nor tide can long postpone').
line(5, 4, 'One gathers rosebuds or grows old alone').
line(6, 4, 'The thumb- and finger-prints of Al Capone').
line(7, 4, 'Normal one aims to be and share the throne').
line(8, 4, '\'Ere meanings new to ancient tribes are thrown').
line(9, 4, 'While sharks to let\'s say potted shrimps are prone').
line(0, 4, 'And empty cages show life\'s bird has flown').

line(1, 5, 'To one sweet hour of bliss my memory clings').
line(2, 5, 'O Parthenon you hold the charger\'s string').
line(3, 5, 'The Frisian Isles my friends are cherished things').
line(4, 5, 'How it surprised us pale grey underlings').
line(5, 5, 'Old Galileo\'s Pisan offerings').
line(6, 5, 'Oh how oh how he hates such pilferings').
line(7, 5, 'And yet \'twas he the beggar Fate just flings').
line(8, 5, 'They both are right not untamed mutterings').
line(9, 5, 'The roundabout eats profits made on swings').
line(0, 5, 'It\'s one of many horrid happenings').

line(1, 6, 'Signaling gauchos very rarely shave').
line(2, 6, 'The North Wind bites into his architrave').
line(3, 6, 'Whose ocean still-born herrings madly brave').
line(4, 6, 'When flame a form to wrath ancestral gave').
line(5, 6, 'Were pots graffiti\'d over by a slave').
line(6, 6, 'Filching the lolly country thrift helped save').
line(7, 6, 'Rejecting ermine to become a knave').
line(8, 6, 'That metered rhyme alone can souls enslave').
line(9, 6, 'Nought can the mouse\'s timid nibbling stave').
line(0, 6, 'With somber thoughts they grimly line the nave').

line(1, 7, 'An icicle of frozen marrow pings').
line(2, 7, 'Th\'outrageous Thames a troubled arrow slings').
line(3, 7, 'Such merchandise a melancholy brings').
line(4, 7, 'A daring baron pockets precious Mings').
line(5, 7, 'The leaning linguist cameramaniac sings').
line(6, 7, 'He\'s gone to London how the echo rings').
line(7, 7, 'The fertile mother changelings drops like kings').
line(8, 7, 'They both are right not unformed smatterings').
line(9, 7, 'In salads all chew grubs before they\'ve wings').
line(0, 7, 'Proud death quite il-le-gi-ti-mate-ly strings').

line(1, 8, 'As sleeping-bags the silent landscape pave').
line(2, 8, 'To break a rule Britannia\'s might might waive').
line(3, 8, 'For burning bushes never fish forgave').
line(4, 8, 'Till firemen come with hose-piped tidal wave').
line(5, 8, 'Etruscan words which Greece and Rome engrave').
line(6, 8, 'Through homestead hillside woodland rock and cave').
line(7, 8, 'In purest cradles tha\'s how they behave').
line(8, 8, 'That every verbal shock aims to deprave').
line(9, 8, 'The nicest kids for stickiest toffees crave').
line(0, 8, 'Victorious worms grind all into the grave').

line(1, 9, 'Staunch pilgrims longest journeys can\'t depress').
line(2, 9, 'Platonic Greece was not so talentless').
line(3, 9, 'When dried the terrapin can naught express').
line(4, 9, 'The fasting fakir doesn\'t smell the less').
line(5, 9, 'Emboggled minds may puff and blow and guess').
line(6, 9, 'The peasant\'s skirts on rainy days she\'d tress').
line(7, 9, 'The genealogist with field and fess').
line(8, 9, 'Poetic license needs no strain or stress').
line(9, 9, 'The wolf devours both sheep and shepherdess').
line(0, 9, 'It\'s no good rich men crying Heaven Bless').

line(1, 10, 'What things we did we went the whole darned hog').
line(2, 10, 'A piercing wit would sprightliest horses flog').
line(3, 10, 'Shallots and sharks\' fins face the smould\'ring log').
line(4, 10, 'In Indian summers Englishmen drink grog').
line(5, 10, 'With gravity at gravity\'s great cog').
line(6, 10, 'And starve the sniveling baby like a dog').
line(7, 10, 'With quill white-collared through his life will jog').
line(8, 10, 'One tongue will do to keep the verse agog').
line(9, 10, 'A bird-brain banquet melts bold Mistress Mog').
line(0, 10, 'Or grinning like a pale-faced golliwog').

line(1, 11, 'And played their mountain croquet jungle chess').
line(2, 11, 'Socrates watched his hemlock effervesce').
line(3, 11, 'While homeward thirsts to each quenched glass say yes').
line(4, 11, 'The colonel\'s still escutcheoned in undress').
line(5, 11, 'On wheels the tourist follows his hostess').
line(6, 11, 'Watching manure and compost coalesce').
line(7, 11, 'To prove mamma an adult with a tress').
line(8, 11, 'From cool Parnassus down to wild Loch Ness').
line(9, 11, 'The country lane just thrives on farmyard mess').
line(0, 11, 'Poor Yorick comes to bury not address').

line(1, 12, 'Southern baroque\'s seductive dialogue').
line(2, 12, 'Their sculptors did our best our hulks they clog').
line(3, 12, 'Lobsters for sale must be our apologue').
line(4, 12, 'No need to cart such treasures from the fog').
line(5, 12, 'With breaking voice across the Alps they slog').
line(6, 12, 'One misses cricket hearth and croaking frog').
line(7, 12, 'But I can understand you Brother Gog').
line(8, 12, 'Bard I adore your endless monologue').
line(9, 12, 'Whiskey will always wake an Irish bog').
line(0, 12, 'We\'ll suffocate before the epilogue').

line(1, 13, 'Suits lisping Spanish tongues for whom say some').
line(2, 13, 'With marble souvenirs then fill a slum').
line(3, 13, 'On fish-slab whale nor seal has ever swum').
line(4, 13, 'The Taj Mahal has trinkets spice and gum').
line(5, 13, 'Do bank clerks rule their abacus by thumb?').
line(6, 13, 'Where no one bothered how one warmed one\'s bum').
line(7, 13, 'And let you off from your opinions glum').
line(8, 13, 'Ventriloquists be blowed you strike me dumb').
line(9, 13, 'Through bretzels take the dolls from board-room drum').
line(0, 13, 'Poor reader smile before your lips go numb').

line(1, 14, 'The bell tolls fee-less fi-less fo-less fum').
line(2, 14, 'For Europe\'s glory while Fate\'s harpies strum').
line(3, 14, 'They\'re kings we\'re mammal-cousins hi ho hum').
line(4, 14, 'And lessors\' dates have all too short a sum').
line(5, 14, 'In cognac brandy is Bacardi rum?').
line(6, 14, 'Yet from the City\'s pie pulled not one plum').
line(7, 14, 'A wise loaf always knows its humblest crumb').
line(8, 14, 'Soliloquies predict great things old chum').
line(9, 14, 'Fried grilled black pudding\'s still the world\'s best yum').
line(0, 14, 'The best of all things to an end must come').

%% generate(+Choices:list(integer), -DOM_Triple) is det.
% Generates one of the poems.

queneau(Title, poem/DOM):-
  atom_chars(Title, Chars),
  maplist(atom_number, Chars, Choices),
  numlist(1, 14, PoemNumbers),
  maplist(
    line,
    Choices,
    PoemNumbers,
    [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14]
  ),
  poem_dom(
    Title,
    'Raymond'/'Queneau',
    [[S1,S2,S3,S4],[S5,S6,S7,S8],[S9,S10,S11,S12],[S13,S14]],
    DOM
  ).

%% queneau(+Request:list) is det.
% The URI-path version of the Queneau poem generator.

queneau(Request):-
  (
    memberchk(path_info(Path0), Request),
    atom_concat('/', Path, Path0),
    sub_atom(Path, 0, 14, _After, Title)
  ->
    queneau(Title, DTD/DOM)
  ;
    random_queneau(DTD/DOM)
  ),
  serve_xml(DTD, DOM).

%% queneau_web(-Markup:list) is det.
% Returns the markup for a poem.

queneau_web(DOM_Triple):-
  random_queneau(DOM_Triple).

random_queneau(DOM_Triple):-
  length(Numbers, 14),
  maplist(random_betwixt(0, 9), Numbers),
  maplist(atom_number, Atoms, Numbers),
  atomic_list_concat(Atoms, Atom),
  queneau(Atom, DOM_Triple).

