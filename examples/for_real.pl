
:- ensure_loaded( library(real) ).
:- ensure_loaded( library(lists) ).
:- use_module( library(apply_macros) ).
:- use_module( library(readutil) ).

% for_real.
%
%  Some examples illustrating usage of the r..eal library.

for_real :-
     ( Head = ex(_Ex); Head = tut(_Tut) ),
     clause( Head, Body ),
     write(running:Head), nl, nl,
     portray_clause( (Head:-Body) ), nl, nl,
     write( 'Output: ' ), nl,
     ( catch(Head,Exc,Fex=true) -> 
          ( Fex==true-> 
               write( '! ' ), write( caught(Exc) ), nl, abort
               ;
               write(status:true)
          )
          ;
          write( '! ' ), write( failure ), nl, abort
     ),
     nl, nl, write('-----'), nl, nl,
     fail.
for_real :-
     write( 'All done.' ), nl.


% ex(int).
%
%  Pass the salt please. 
%  The most basic example: pass a Prolog list of integers to an R variable 
%  and then back again to a Prolog variable.
%
ex(int) :-
     i <- [1,2,3,4],
     <- i,
     I <- i,
     write( i(I) ), nl.

% float.
%
%  Pass a Prolog list of floats to an R variable and then back again to a Prolog variable.
%
ex(float) :-
     f <- [1.0,2,3,4],
     <- f,
     F <- f,
     write( f(F) ), nl.

% ex( float ).
%
%  Pass a mixed Prolog list of integers and floats to an R variable and
%  then back again to a Prolog variable.
%  The returning list is occupied by floats as is the R variable.
%
ex(to_float) :-
     m <- [1,2,3,4.0],
     <- m,
     M1 <- m,
     write( m(M1) ), nl,
     m <- [1,2,3,4.1],
     <- m,
     M2 <- m,
     write( m(M2) ), nl.

% ex(bool).
%
%
ex(bool) :- 
     b <- [true,false,true,true],
     <- b,
     B <- b,
     write( b(B) ), nl.

% at_bool.
%
%  In cases where disambiguation is needed, boolean values can be represented by @Val terms.
%
ex(at_bool) :- 
     b <- [@true,@false,@true,@true],
     <- b,
     B <- b,
     write( at_b(B) ), nl.

%  ex(bool_f).
%
%   This fails since there is a non boolean value in a list.
%
%   On SWI this fails...
%   On YAP this throuws exception....
%
ex(bool_f) :-
     ( catch(b <- [true,false,true,true,other],_,fail) ->
          fail
          ;
          true
     ).

%  ex(bool_back).
%
%   Get some boolean values back from applying a vector element equality to an integer
%   vector we just passed to R. Prints the R bools first for comparison.
%
ex(bool_back) :- 
     t <- [1,2,3,4,5,1],
     <- t,
     s <- t==1,
     <- s,
     S <- s,
     write( s(S) ), nl.

% ex(atom_char).
%
%  Pass some atoms to an R vector of characters.
%
ex(atom_char) :- 
     f <- [a,b,c],
     <- f,
     F <- f,
     write( f(F) ), nl.

% ex(matrix_int). 
%
%  Pass a 2-level list of lists of integers to an R matrix (and back again).
%
ex(matrix_int) :-
     a <- [[1,2,3],[4,5,6]],
     <- a,
     A <- a,
     nl, write( a(A) ), nl.

% ex(matrix_char). 
%
%  Pass a 2-level list of lists of characters to an R matrix (and back again).
%
ex(matrix_char) :-
     a <- [[a,b,c],[d,e,f]],
     <- a,
     A <- a,
     write( a(A) ), nl.


/*
% ex(matrix_idx).
%
ex(matrix_idx) :-
     a <- [[1,2,3],[4,5,6]],
     <- a,
	J <- aˆ[[1,3],*],
	write( j(J) ), nl.
	*/

% ex(list).
%
%  A Prolog = pairlist to an R list. Shows 2 alternative ways to access the list items.
% 
ex(list) :-
     a <- [x=1,y=0,z=3],
     <- a,
     write( 'First element of list :' ), nl,
     <- a^[[1]],
     write( 'Second element of list :' ), nl,
     <- a$y,
     A <- a,
     write( a(A) ), nl.

% ex(list).
%
%  R allows for unamed lists.
% 
ex(unamed) :-
	 li <- 'list()',
	 li[[1]] <- c(1,2,3),
	  <- li,
	  L <- li,
	  write( l(L) ), nl.

% ex(list_ea).
%
% Produces error due to name of constructor: -.
%
ex(list_ea) :-  % produces error
     catch_controlled( a <- [x=1,y=0,z-3] ),
     <- a,
     A <- a,
     write( a(A) ), nl.

% ex(list_eb).
%
% Produces an error due to mismatch of arity of =.
%
ex(list_eb) :- 
     catch_controlled( a <- [x=1,y=0,=(z,3,4)] ),
     <- a,
     A <- a,
     write( a(A) ), nl.

% ex(char_list).
%
%  Pass a list which has a char value.
%
ex(char_list) :-
     a <- [x=1,y=0,z=three],
     <- a,
     A <- a,
     memberchk( Z=three, A ),
     write( z(Z):a(A) ), nl.

% ex(mix_list). 
%
%  An R-list of mixed types.
%
ex(mix_list) :-
     a <- [x=1,y=[1,2,3],z=[[a,b,c],[d,e,f]],w=[true,false]],
     A <- a, 
     <- print(a),
     write( a(A) ), nl.

% ex(list2).
%
%  Illustrates ways of accessing list elements.
% 
ex(list2) :-
     l <- list(.), 
     l^[[+"what"]] <- c(1,2,3),
     l$who <- c(4,5,6),
     <- l,
     L <- l,
     write( l(L) ), nl.

% ex(slot).
%
%  Creating formal objects and accessing their content.
%
ex(slot) :-
     <- setClass(+"track", representation(x=+"numeric", y=+"numeric")),
     myTrack <- new(+"track", x = -4:4, y = exp(-4:4)),
      <- print( myTrack@x ),
     % [1] -4 -3 -2 -1  0  1  2  3  4
      Y <- myTrack@y, 
      write( y(Y) ), nl,
      <- setClass(+"nest", representation(z=+"numeric", t=+"track")),
      myNest <- new(+"nest", z=c(1,2,3) ),
      myNest@t <- myTrack,
      myNest@t@x <- Y+1,  % good ex. for hidden vars.
      <- myNest,
      % N <- myNest, % unsupported r-type
      % X <- myNest@t@x,
      <- print(myNest@t@x),
      X <- myNest@t@x,
      <- myNest@t@x,
      write( x(X) ), nl.

      % myTrack@x <- c(1,2,3).

% ex(add_element).
%
%   Adds a third element to a list after creation.
%
ex(add_element) :-
     x <- [a=1,b=2],
     x$c <- [3,4], 
     <- x,    % print   =   $a 3
     X <- x,
     write( x(X) ), nl.   % X = [a=3.0].

% ex(singletons).
%
%  Pass an integer and a singleton number list and get them back.
% Although in R both are passed as vectors of length on, when back in Prolog
% the singleton list constructors are stripped, returing a single integer value in both cases.
%  
ex(singletons) :- 
     s <- 3,
     <- s,
     S <- s,
     <- s,
     t <- [3],
     <- t,
     T <- t,
     write( s(S)-t(T) ), nl.

% ex(assign). 
%
% Simple assignment of an R function (+) application on 2 R values originated in Prolog.
% 
ex(assign) :- 
     a <- 3,
     <- a,
     b <- [2],
     <- b,
     C <- a + b,
     write( c(C) ), nl.


% ex(assign_1).
%
%  Assign the result of an R operation on matrix and value to a Prolog variable.
%
ex(assign_1) :- 
     a <- [[1,2,3],[4,5,6]], 
     <- a,
     B <- a*3, 
     write( b(B) ), nl.

% ex(assign_2).
%
%  Assign the result of an R operation on matrices to a Prolog variable.
%
ex(assign_2) :- 
     a <- [[1,2,3],[4,5,6]], 
     <- a,
     b <- 3,
     <- b,
     C <- a*b,
     write( c(C) ), nl.

% ex(assign_r).
%
% Assign values to R variables and operate on them. 
% Using c as an R variable is also a good test, as we test against c(...).
%
ex(assign_r) :- 
     a <- [3],
     <- a,
     b <- [2],
     <- b,
     c <- a + b,
     <- c.

/*  disable for now. once Yap supports . in atoms
    re-establish this, but make sure you restor
    relevant flag back to its original setting.

% ex(dot_in_function_names).
%
% Test dots in functions names via the .. mechanism.
%
ex(dot_in_function_names) :-
     a <- [1.1,2,3],
     <- a,
     x <- as..integer(a),
     <- x.
	*/

/* as aove
% ex(dot_in_rvars).
%
%  Test dots in R variable names via the .. mechanism. Generates an error on the last goal.
%
ex(dot_in_rvar) :-
     a..b <- [1,2,3],
     <- a..b,
     <- 'a.b',
     catch_controlled( <- print('a..b') ).
	*/

% ex(semi_column).
%
%  A:B in R generates a vector of all integers from A to B.
%
ex(semi_column) :-
     z <- 1:50,
     <- z,
     Z <- z, 
     length( Z, Len ),
     write( len(Len) ), nl.

% ex(c_vectors).
%
%  r.eal also supports c() R function concatenation.
%
ex(c_vectors) :-
     a <- c(1,2,3,5),  % this goes via the fast route
     <- a,
     b <- c(1,1,2,2) + c(1:4),
     <- b,
     C <- a+b,
     write( 'C'(C) ), nl.

% ex(empty_args).
%
%  Test calling R functions that take no arguments (via foo(.)).
%
ex(empty_args) :-
     <- plot( 1:10, 1:10 ),
     findall( I, (between(1,6,I),write('.'), flush_output, sleep(1)), _ ),
     nl,
     devoff.  % fixme use dev.off() when Yap starts supporting it.

% ex(string).
%
%  Test new (2013/11/22) string type in SWI Prolog v7.
%
ex(string) :-
	( (current_predicate(string/1),string("abc")) ->
     	<- plot( 1:10, 1:10, main="native string type has arrived to Prolog" ),
     	findall( I, (between(1,6,I),write('.'), flush_output, sleep(1)), _ )
		;
		true
	).

% ex(binary_op).
%
% Early versions of r..eal were not handling this example properly.  
% Thanks to Michiel Hildebrand for spotting this.
% The correct answer is =|[0.0,4.0]|=. First subtract v1 from v2 and then take power 2.
%
ex(binary_op) :-
     v1 <- c(1,1),
     <- v1,
     v2 <- c(1,-1),
     <- v2,
     P <- (v1-v2)^2,
     write( P ), nl.
     % not !!! : P = [0.0, 0.0].

% ex(utf).
%
% Plots 3 points with the x-axis label showing some Greek letters (alpha/Omega).
%
ex(utf) :-
	<- plot( c(1,2,3), c(3,2,1), xlab= +[945,0'/,937] ),
	findall( I, (between(1,4,I),write('.'), flush_output, sleep(1)), _ ),
	nl,
	devoff.


% ex(utf_atom).
%
% Plots 3 points with the x-axis label showing some Greek letters (alpha/Omega) as atom preceded by +.
%
ex(utf_atom) :-
	<- plot( c(1,2,3), c(3,2,1), xlab= +'α/Ω' ),
	findall( I, (between(1,4,I),write('.'), flush_output, sleep(1)), _ ),
	nl,
	devoff.

% ex( utf_1 ).
%
%  Thanks to  Guillem R.
%
ex(utf_1) :-
     s <- ['Pour ce garçon être sur une île, y avoir des histoires de cœur ambiguës et vider un fût de bière sur un canoë entouré par des caïmans, ne fut pas un mince affaire.'],
     <- s,
     S <- s,
     write( s(S) ), nl.

% ex( utf1 ).
%
%  Mostly Vitor's then Sander and last one from Nicos.
%
ex(utf_2) :-
     x <- [hello, 'olá', 'जैसा कहर बरपा तो बर्बाद हो जाएगी मुंबई','Beëindigen','άμπελος'],
     <- x,
     X <- x,
     write( x(X) ), nl.


% ex(plot_cpu).
%
%  Create a plot of 4 time points. Each having a push and a pull time component.
%  These are the time it takes to push a list through to R and the time to Pull the same
%  (very long) list back.
%
ex(plot_cpu) :-
     plot_cpu( 1000 ).

ex(debug) :-
     real_debug,
     write( started_debugging ), nl,
     x <- c(1,2,3),  % c-vectors
     y <- [1,2,3],   % PL data lists
     X <- x,         % R var to PL var
     x <- [a=[1,2,4],b=[4,5,6]],
     A <- x,
     B <- x$b,       % R expression to PL var
     Y <- x$b + x$a,
     x$c <- [6,3,7],
     real_nodebug,
     write( x(X) ), nl,
     write( a(A) ), nl,
     write( b(B) ), nl,
     write( y(Y) ), nl,
     write( stopped_debugging ), nl.

% ex(rtest).
% Some tests from r_session, 
%
ex(rtest) :-
     <- 'set.seed'(1),  % fixme: dot
	y <- rnorm(50),
	<- y,
	x <- rnorm(y),
     <- x,
     <- x11(width=5,height=3.5),
	<- plot(x,y),
     r_wait,
	devoff,
	Y <- y,
	write( y(Y) ), nl,
	findall( Zx, between(1,9,Zx), Z ),
	z <- Z,
	<- z,
	cars <- [1, 3, 6, 4, 9],
	% cars <- c(1, 3, 6, 4, 9),
     <- cars,
	<- pie(cars),
     r_wait,
	devoff.

% list_times.
%
% Print some timing statistics for operations on a long list of integers.
%
list_times :-
     findall( I, between(1,10000000,I), List ),
     statistics( cputime, Cpu1 ), write( cpu_1(Cpu1) ), nl,
     l <- List,
     a <- median( l ),
     statistics( cputime, Cpu2 ), write( cpu_2(Cpu2) ), nl,
     b <- median( List ),
     statistics( cputime, Cpu3 ), write( cpu_3(Cpu3) ), nl,
     <- a,
     <- b.

% adapted from YapR

% Intrinsic attributes: mode and length
tut(tut1) :-
	z <- 0:9,
	<- z,
	digits <- 'as.character'(z), % fixme: dot
	<- digits,
	d <- 'as.integer'(digits),  % fixme: dot
	<- d.

% changing the length of an object
tut(tut2) :-
	e <- numeric(.),
	(e^[3]) <- 17,
	<- e,
	alpha <- 1:10,
	alpha <- alpha^[2 * 1:5],
     <- alpha,   % = 2, 4, 6, 8 10
	length(alpha) <- 3,
	<- alpha,   % = 2, 4, 6
     nl, write( ' on beta now ' ), nl, nl,
     beta <- 1:10,
     beta <- 2 * beta,
     <- beta,    % 2  4  6  8 10 12 14 16 18 2
     length(beta) <- 3,
     <- beta.    % 2 4 6

% Getting and setting attributes
tut(tut3) :-
	z <- 1:100,
	attr(z, +"dim") <- c(10,10),
	<- z.

% factors and tapply.
tut(tut4) :-
      /* state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
                  "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
                  "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
                  "sa",  "act", "nsw", "vic", "vic", "act"), */
     state <- [tas,sa,qld,nsw,nsw,nt,wa,wa,qld,vic,nsw,vic,qld,qld,sa,tas,sa,nt,wa,vic,qld,nsw,nsw,wa,sa,act,nsw,vic,vic,act],
	<- state,
     % <- astate,
	statef <- factor(state),
	<- statef,
	<- levels(statef),
	incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
                    61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
                    59, 46, 58, 43),
	incmeans <- tapply(incomes, statef, mean),
	% notice the function definition.
	stderr <- ( function(x) :-  sqrt(var(x)/length(x)) ),
	incster <- tapply(incomes, statef, stderr),
	<- incster.

tut(tut5) :-
	z <- 1:1500,
	dim(z) <- c(3,5,100),
	a <- 1:24,
	dim(a) <- c(3,4,2),
	<- print(a^[2,*,*]),
	<- print(dim(a)),
	x <- array(1:20, dim=c(4,5)),
	<- x,
	i <- array(c(1:3,3:1), dim=c(3,2)),
	<- i,
	x^[i] <- 0,
	<- x,
	h <- 1:10,
	z <- array(h, dim=c(3,4,2)),
	<- z,
	a <- array(0, dim=c(3,4,2)),
	<- a,
	% ab <- z '%o%' a,
	ab <- z @^@ a,
	<- ab,
     f <- ( function(x, y) :- cos(y)/(1 + x^2) ),
	w <- outer(z, a, f),
	<- w.

tut(tut6) :-
	d <- outer(0:9, 0:9),
	fr <- table(outer(d, d, +"-")),
	r(plot('as.numeric'(names(fr)), fr, type=+"h", xlab=+"Determinant", ylab=+"Frequency")), % fixme: dot
     nl, write( '   type query: ``devoff.\'\' to close the plot display ' ), nl, nl.

% auxiliary,
cpu_points( [], [], [] ).
cpu_points( [H|T], [S|Ss], [L|Ls] ) :-
     between_1_and(H,Long),
     statistics( cputime, _) , 
     length( Long, Lengtho ), write( leno(Lengtho) ), nl,
     statistics( cputime, S0 ), 
     ( number(S0) -> S0 = S ; S0 = [_,S] ),
     % statistics( cputime, [_,S] ), 
     long <- Long,
     Back <- long,
     Back = [Hb|_],
     Hb =:= 1,
     statistics( cputime, L0 ), 
     ( number(L0) -> L0 = L ; L0 = [_,L] ),
     % statistics( cputime, [_,L] ), 
     length( Back, BackLen ),
     write( back_len(BackLen) ), nl,
     % L = 0,
     cpu_points( T, Ss, Ls ) .

% auxiliaries,

catch_controlled( Expr ) :-
     catch( Expr, Caught, true ),
     ( \+ var(Caught) -> write( caught_controlled(Caught) ), nl; fail ).

between_1_and(N,X) :-
     ( var(N) -> N is 100; true ),
     IntN is integer(N),
     findall( I, between(1,IntN,I), Is ),
     i <- Is,
     X <- i.
cpu( R ) :-
     ( var(R) -> R is 10000; true ),
     findall( F, between_1_and(R,F), Fs ),
     f <- Fs, 
     statistics( cputime,  Cpu1 ),
     write( cputime_to_push(Cpu1) ), nl,
     X <- f,   % when   F <- f   the predicate fails midway for large Len !!!
     statistics( cputime,  Cpu2 ),
     write( cputime_to_pull(Cpu2) ), nl,
     length( X, Len ),
     write( len(Len) ), nl.
plot_cpu( Factor ) :-
     nl,
     ( Factor > 10 ->
         M='if your set-up fails on this test increase the size of stack.',
         write( M ), nl, nl 
          ;
          true
     ),
     points <- [10,100,500,1000],
     points <- 'as.integer'( points * Factor ), % fixme: dot
     <- points,
     Points <- points,
     write( points(Points) ), nl,
     cpu_points( Points, Push, Pull ), 
     push <- Push,
     pull <- Pull,
     write( plotting(Pull,Push) ), nl,
     <- plot( points, pull, ylab =+"pull & push (red) - in seconds" ),
     r_char( red, Red ),
     <- points( points, push, col=Red ).
