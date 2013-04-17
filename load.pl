% Thhe load file for the IAAA project.
% This uses the Prolog Generics Collection as a Git submodule.

load:-
  source_file(load, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  
  % Assert the PGC file search path.
  assert(user:file_search_path(pgc, project('PGC'))),
  % Load the PGC load file.
  ensure_loaded(pgc(load)).

:- load.
