:- module(
  yu,
  [
    yu_web/2 % +Input:atom
             % -Markup:list
  ]
).

/** <module> Yu

Predicates for computationally translated poetry.

@author Wouter Beek
@version 2012/12
*/



yu_web(Input, Markup):-
  split_atom_exclusive(Input, '\n', Lines),

  % Create the directory.
  get_time(TimeStamp),
  stamp_date_time(TimeStamp, DateTime, 'UTC'),
  date_time_value(second, DateTime, DirectoryName),
  absolute_file_name(audio(DirectoryName), Directory),
  create_directory(Directory),

  findall(
    element(
      p,
      [],
      [
        element(
          audio,
          [controls=controls],
          [element(source, [src=MP3_URI, type='audio/mp3'], [])]
        )
      ]
    ),
    (
      nth0(I, Lines, Line),
      uri_normalized(Line, NormalizedLine),
      format(atom(FileName), '~w_~w', [I, NormalizedLine]),
      create_file(Directory, FileName, mp3, MP3_File),
      atom_to_mp3(Line, MP3_File),
      file_to_uri(MP3_File, MP3_URI)
    ),
    Markup
  ).

