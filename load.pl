% The load file for the IAAA project.

project_name('IAAA').

load_iaaa:-
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),
  
  % Project directory.
  source_file(load_iaaa, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(project, ThisDirectory)),
  
  % Assert data subdirectory.
  assert(user:file_search_path(data, project('Data'))),
  
  % Assert the PGC file search path.
  assert(user:file_search_path(pgc, project('PGC'))),
  % Load the PGC load file.
  (
    predicate_property(debug, visible)
  ->
    ensure_loaded(pgc(debug))
  ;
    ensure_loaded(pgc(load))
  ),
  
  % Load the IAAA module.
  ensure_loaded(project(iaaa)).
:- load_iaaa.

