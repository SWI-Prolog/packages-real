:- module(test_real,
	  [ test_real/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module( library(real) ).
:- use_module(library(plunit)).

test_real :-
	run_tests([ real
		  ]).

:- begin_tests(real).

test(int_array) :-
	x <-  c(1,2,3),
	X <- x,
	X == [1,2,3].

test(mixed_array)  :-
	y <- c(1,2,3.1),
	Y <- y,
	Y = [1.0,2.0,3.1].

:- end_tests(real).
