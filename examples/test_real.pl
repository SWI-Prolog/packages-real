
:- ensure_loaded( library(real) ).

test_real :-
	test_real( test_1 ),
	test_real( test_2 ).

test_real( Goal ) :-
	% shall we report ?
	write( doing(Goal) ), nl,
	( call(Goal) ->
		write( done(Goal) ), nl
		;
		write( failed(Goal) ), nl
	).


test_1 :-
	x <-  c(1,2,3),
	X <- x,
	X == [1,2,3].

test_2 :-
	y <- c(1,2,3.1),
	Y <- y,
	Y = [1.0,2.0,3.1].
