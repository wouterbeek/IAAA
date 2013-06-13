% The load file for the IAAA project.

project_name('IAAA').

load_iaaa:-
  source_file(load_iaaa, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  assert(user:file_search_path(iaaa, ThisDirectory)),
  
  % Assert data subdirectory.
  assert(user:file_search_path(data, iaaa('Data'))),
  
  % Load the PGC.
  assert(user:file_search_path(pgc, iaaa('PGC'))),
  (
    predicate_property(debug, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),
  
  % Load the IAAA module.
  ensure_loaded(iaaa(iaaa)).
:- load_iaaa.

