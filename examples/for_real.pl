
:- ensure_loaded( library(real) ).
:- ensure_loaded( library(lists) ).
:- use_module( library(apply_macros) ).
:- use_module( library(readutil) ).


% for r...eal
%
int :-
     i <- [1,2,3,4],
     I <- i,
     write( i(I) ), nl.

float :-
     f <- [1.0,2,3,4],
     F <- f,
     write( f(F) ), nl.

to_float :-
     m <- [1,2,3,4.0],
     M1 <- m,
     write( m(M1) ), nl,
     m <- [1,2,3,4.1],
     M2 <- m,
     write( m(M2) ), nl.

bool :- 
     b <- [true,false,true,true],
     B <- b,
     write( b(B) ), nl.

bool_e :-    % generates error
     b <- [true,false,true,true,other].

bool_back :- 
     t <- [1,2,3,4,5,1],
     s <- t==1,
     <- s,
     S <- s,
     write( s(S) ), nl.

mix_vector(Z) :-
   z <- [1.2,2,3],
   Z <- z.

char :- 
     f <- [a,b,c],
     F <- f,
     write( f(F) ), nl.

matrix_int :-
     a <- [[1,2,3],[4,5,6]],
     A <- a,
     write( a(A) ), nl.

matrix_char :-
     a <- [[a,b,c],[d,e,f]],
     A <- a,
     write( a(A) ), nl.

list :-
     a <- [x=1,y=0,z=3],
     A <- a,
     write( a(A) ), nl.

list_ea :-  % produces error
     a <- [x=1,y=0,z-3],
     A <- a,
     write( a(A) ), nl.

list_eb :- % produces error
     a <- [x=1,y=0,=(z,3,4)],
     A <- a,
     write( a(A) ), nl.

char_list :-
     a <- [x=1,y=0,z=three],
     A <- a,
     memberchk( Z=three, A ),
     write( z(Z):a(A) ), nl.

mix_list :-
     a <- [x=1,y=[1,2,3],z=[[a,b,c],[d,e,f]],w=[true,false]],
     A <- a, 
     <- print(a),
     write( a(A) ), nl.

composite_list :-
     x <- [a=1,b=2],
     x$c <- [3,4], 
     <- x,    % print   =   $a 3
     X <- x,
     write( 'X'(X) ), nl.   % X = [a=3.0].
     

singleton :- 
     s <- 3,
     S <- s,
     write( 'S'(S) ), nl.

assign :- 
     a <- 3,
     b <- [2],
     C <- a + b,
     write( 'C'(C) ), nl.


assign_1 :- 
     a <- [[1,2,3],[4,5,6]], B <- a*3, write( 'B'(B) ), nl.

assign_2 :- 
     a <- [[1,2,3],[4,5,6]], b <- 3, C <- a*b, write( 'C'(C) ), nl.

assign_r :- 
     a <- [3],
     b <- [2],
     c <- a + b,
     <- c.

dot_in_function_names :-
     a <- [1.1,2,3],
     <- a,
     x <- as..integer(a),
     <- x.

dot_in_rvars :-
     a..b <- [1,2,3],
     <- a..b,
     <- 'a.b',
     <- print('a..b').  % produces an error, as it should

semi_column :- 
     z <- 1:50,
     Z <- z, 
     length( Z, Len ),
     write( len(Len) ), nl.

empty_args :-
     <- plot( 1:10, 1:10 ),
     findall( I, (between(1,6,I),write('.'), flush_output, sleep(1)), _ ),
     nl,
     <- dev..off(.).

% thanks to Michiel Hildebrand 
%
binary_op :-
     v1 <- c(1,1),
     v2 <- c(1,-1),
     P <- (v1-v2)^2,
     write( P ), nl.
     % not !!! : P = [0.0, 0.0].

plot_cpu :-
     plot_cpu( 10 ).

list_args :-
     findall( I, between(1,10000000,I), List ),
     statistics( cputime, Cpu1 ), write( cpu_1(Cpu1) ), nl,
     l <- List,
     a <- median( l ),
     statistics( cputime, Cpu2 ), write( cpu_2(Cpu2) ), nl,
     b <- median( List ),
     statistics( cputime, Cpu3 ), write( cpu_3(Cpu3) ), nl,
     <- a,
     <- b.

% from r_session, 
%
rtest :-
     <- set..seed(1),
	y <- rnorm(50),
	<- y,
	x <- rnorm(y),
     <- x11(width=5,height=3.5),
	<- plot(x,y),
     r_wait,
	<- dev..off(.),
	Y <- y,
	write( y(Y) ), nl,
	findall( Zx, between(1,9,Zx), Z ),
	z <- Z,
	<- z,
	cars <- [1, 3, 6, 4, 9],
	% cars <- c(1, 3, 6, 4, 9),
	<- pie(cars),
     r_wait,
	devoff.

% adapted from YapR

% Intrinsic attributes: mode and length
tut1 :-
	z <- 0:9,
	<- z,
	digits <- as..character(z),
	<- digits,
	d <- as..integer(digits),
	<- d.

% changing the length of an object
tut2 :-
	e <- numeric(.),
	(e^[3]) <- 17,
	<- e,
	alpha <- 1:10,
	alpha <- alpha^[2 * 1:5],
	length(alpha) <- 3,
	<- alpha,
     nl, write( ' on beta now ' ), nl, nl,
     beta <- 1:10,
     beta <- 2 * beta,
     <- beta,
     length(beta) <- 3,
     <- beta.

% Getting and setting attributes
tut3 :-
	z <- 1:100,
	attr(z, "dim") <- c(10,10),
	<- z.

% factors and tapply.
tut4 :-
	state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
                  "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
                  "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
                  "sa",  "act", "nsw", "vic", "vic", "act"),
     astate <- [tas,sa,qld,nsw,nsw,nt,wa,wa,qld,vic,nsw,vic,qld,qld,sa,tas,sa,nt,wa,vic,qld,nsw,nsw,wa,sa,act,nsw,vic,vic,act],
	<- state,
     <- astate,
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

tut5 :-
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
	ab <- z '%o%' a,
	<- ab,
     f <- ( function(x, y) :- cos(y)/(1 + x^2) ),
	w <- outer(z, a, f),
	<- w.

tut6 :-
	d <- outer(0:9, 0:9),
	fr <- table(outer(d, d, "-")),
	r(plot(as..numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency")),
     nl, write( '   type query: ``devoff.\'\' to close the plot display ' ), nl, nl.

% auxiliary,
cpu_points( [], [], [] ).
cpu_points( [H|T], [S|Ss], [L|Ls] ) :-
     between_1_and(H,Long),
     statistics( cputime, _) , 
     length( Long, Lengtho ), write( leno(Lengtho) ), nl,
     statistics( cputime, S ), 
     % statistics( cputime, [_,S] ), 
     long <- Long,
     Back <- long,
     Back = [Hb|_],
     Hb =:= 1,
     statistics( cputime, L ), 
     % statistics( cputime, [_,L] ), 
     length( Back, BackLen ),
     write( back_len(BackLen) ), nl,
     % L = 0,
     cpu_points( T, Ss, Ls ).

% auxiliary,
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
     points <- as..integer( points * Factor ),
     <- points,
     Points <- points,
     write( points(Points) ), nl,
     cpu_points( Points, Push, Pull ), 
     push <- Push,
     pull <- Pull,
     write( plotting(Pull,Push) ), nl,
     <- plot( points, pull, ylab ='"pull + push (red) - in seconds"' ),
     r_char( red, Red ),
     <- points( points, push, col=Red ).
