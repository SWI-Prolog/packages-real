:- module('real', [
     init_r/0,
     end_r/0,
	r/2,
	r/1,
     is_rvar/1,
     is_rvar/2,
	op(950,fx,<-),
	op(950,yfx,<-),
     (<-)/1,
     real/2,
     r_char/2,
     devoff/0,
	real_type/1,
	(<-)/2,
	op(600,xfy,~),
	op(600,xfy,'..'),
	op(400,yfx,'%x%'),
	op(400,yfx,'%%'),
	op(400,yfx,'%/%'),
	op(400,yfx,'%*%'),
	op(400,yfx,'%o%'),
	op(400,yfx,'%in%'),
	op(400,yfx,$),
	op(400,fx,@)
     ]).

:- use_foreign_library(foreign(real)).

:- use_module(library(lists) ).
:- use_module(library(apply_macros)).

/** <module> r..eal

A C-based  Prolog interface to R.

---++ Introduction

This library enables the communication with an R process started as a shared library. It is the result of the
efforts of two research groups that have worked in parallel.
The syntactic emphasis on a minimalistic interface. A single predicate (<-/2,<-/1) channels the bulk of the interactions.
In addition to using R as a shared library, r..eal uses the c-interfaces of SWI/Yap and R to pass objects
in both directions. The usual mode of operation is to load Prolog values on to R variables and then
call an R function on these values. The return value of the called function can be either placed on R variable
or passed back to Prolog.  It has been tested extensively on current SWI and  YAP 6.3.0 on Linux machines but
it should also compile and work on MS operating systems and Macs.

The main modes for utilising the interface are
==
	<- +Rexpr
	<- +Rvar
==

     Print  Rvar or evaluate expression, Rexpr,  in R
==
	+PLvar  <- +Rexpr
	+Rexpr1 <- +Rexpr2
	+Rvar   <- +PLval
	+PLvar  <- +Rvar
==


Evaluate Rexpr and pass result to PLvar.

Pass Rexpr1 <- Rexpr2 to R.

Pass PLval to Rvar (using the c-interface when Rvar is numeric, list or list of 2 levels).  See data transfer below.

Assign contents of Rvar to PLvar using the c-interface.

---++ Syntax

There are syntactic conventions in R that make unparsable prolog code. Notably function and variable names
are allowed to contain dots, square brackets are used to access parts of vectors and arrays and functions are allowed
empty argument tuples. We have introduced relevant syntax which allows for easy transition between prolog
and R. Prolog constructs are converted by the library as follows:


     * =|..|= within atoms  ->  =|.|= (ex. =| as..integer(c(1,2,3)) ->  as.integer(c(1,2,3))|= )
     * =|^[]|=  after atoms -> =|[]|= (ex. =|a^[2] -> a[2] |=)
     * =|(.)|= at the end of atoms that are known R functions -> =|()|=  (ex. =|dev..off(.) -> dev.off()|= )
     * =|[]|= -> c() (which equal to R's NULL value)
     * ( f(x) :-  (..))   -> f(x) (...)
     * Lists of lists of numbers are converted to matrices. All first level lists must have the same length.
     * Filenames must be given as Prolog strings.
     * R specific operators (eg. %*% should be quoted in Prolog.
     * expressions that pose difficulty in translation can alwasy be passed as Prolog atoms or strings eg. 'inp$id'


---++ Data transfers

Following R convension we will talk about the type of and the class of data.
i, defined by i <- as.integer(c(1,2,3)), is of class integer vector and type integer.
R vectors are mapped to prolog lists and matrices are mapped to nested lists.
The convention works the other way around too.

There are two ways to pass prolog data to R. The more efficient one is by using
==
 Rvar <- PLval
==

We will call this the low-level interface. It is implemented as C code and
transfers via C data between R and Prolog.
In what follows atomic PLval data are simply considered as singleton lists.
Flat Plval lists are translated to R vectors and lists of one level of nesting to R matrices (which are 2 dimensional arrays
in R parlance). The type of values of the vector or matrice is taken to be the type of the first data element of the Plval
according to the following :

     * integer -> integer
     * float   -> double
     * atom    -> char
     * boolean -> logical

Booleans are represented in prolog as true/false atoms.
Currently arrays of aribtrary dimensions are not supported in the low-level interface. Note that in R a scalar is just a
one element vector.  When passing Prolog objects to R, the user can specify the type of the
R object by using the (:) operator (see real_type/1). For example:

==
r <- float:[1,2,3].
<- r.					% don't be deceived :
R <- r.

i <- int:[1,2,3].
<- i.
I <- i.

==

In addition Prolog data can be passed through the expression mechanism. That is, data appearing in an arbitrary
R expression will be parsed and be part of the long string that will be passed from Prolog to R for evaluation.
This is only advisable for short data structures. For instance,

==
     tut_4a :-
          state <- c("tas", "sa",  "qld", "nsw", "nsw"),
          <- state.
==

Through this interface it is more convenient to represent R chars by Prolog list of codes, as in the above example.

---++ Examples

==

?- e <- numeric.
yes
?- e^[3] <- 17.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0]
?- e^[10] <- 12.
yes
?- Z <- e.
Z = ['$NaN','$NaN',17.0,'$NaN','$NaN','$NaN','$NaN','$NaN','$NaN',12.0]

int :-
     i <- [1,2,3,4], I <- i, write( i(I) ), nl.

bool_back :-
     t <- [1,2,3,4,5,1], s <- t==1, S <- s, write( s(S) ), nl.

list :-
     a <- [x=1,y=0,z=3], A <- a, write( 'A'(A) ), nl.

matrix :-
     a <- [[1,2,3],[4,5,6]], A <- a.

expr1 :-
     a <- [[1,2,3],[4,5,6]], B <- a*3.

expr2 :-
     a <- [[1,2,3],[4,5,6]], b <- 3, C <- a*b.


between_1_and( N, Bs ) :-
     findall( B, between(1,N,B), Bs ).

plot_cpu :-
     points <- [10,100,500,1000],
     points <- 'as.integer'(points * 10),
     <- points,
     Points <- points,
     write( points(Points) ), nl,
     findall( T, (member(P,Points),between_1_and(P,Long),
                   write( '.' ), flush_output,
                   statistics( cputime, _) ,
                   long <- Long, statistics( cputime, [_,SCpu] ),
                   _Back <- long, statistics( cputime, [_,TCpu] ),
                   T = SCpu-TCpu
                 ),
               Ts
            ),
     write( ts(Ts) ), nl,
     maplist( arg(1), Ts, Push ),
     write( push(Push) ), nl,
     maplist( arg(2), Ts, Pull ),
     write( pull(Pull) ), nl,
     push <- Push, pull <- Pull,
     <- plot( points, push, ylab='"push + pull (red)"' ),
     r_char( red, Red ),
     <- points( points, pull, col=Red ).

rtest :-
	y <- rnorm(50),
	<- y,
	x <- rnorm(y),
     <- x11(width=5,height=3.5),
	<- plot(x,y),
	write( 'Press Return to continue...' ), nl,
	read_line_to_codes( user_input, _ ),
	<- dev..off(.),
	Y <- y,
	write( y(Y) ), nl,
	findall( Zx, between(1,9,Zx), Z ),
	z <- Z,
	<- z,
	cars <- [1, 3, 6, 4, 9],
	% cars <- c(1, 3, 6, 4, 9),
	<- pie(cars),
	write( 'Press Return to continue...' ), nl,
	read_line_to_codes( user_input, _ ),
	devoff.


tut6 :-
	d <- outer(0:9, 0:9),
	fr <- table(outer(d, d, "-")),
	r(plot(as.numeric(names(fr)), fr, type="h",
            xlab="Determinant", ylab="Frequency")).

tut4b :-
     astate <- [tas,sa,qld,nsw,nsw,nt,wa],
     statef <- factor(state),
     incmeans <- tapply( c(60, 49, 40, 61, 64, 60, 59, 54), statef, mean ),
     <- incmeans.

logical :-
     t <- [1,2,3,4,5,1],
     s <- t==1,
     <- s,
     S <- s,
     write( s(S) ), nl.

==

Note, r..eal is sensitive to the amount of stack made available to the Prolog engine.
With a generous stack, huge objects can be passed between the two systems.

==

κρότος;~/sureal% yap -s80000 -f -g "ensure_loaded(library(real))"
YAP 6.3.0 (x86_64-linux): Fri Nov 11 16:06:00 CET 2011
MYDDAS version MYDDAS-0.9.1
?- findall( I, between(1,1000000,I), Is), i <- Is, K <- i, length( K, Len ), write( len(Len) ), nl, fail.
len(1000000)

==

---++ Info

We are grateful to Jan Wielemaker for re-setting and improving the whole of the C code.
Many thanks to the original YapR developers for inspiration and some functions.

@author		Nicos Angelopoulos
@author		Vitor Costa Santos
@version	0:0:3, 2011/12/23
@license	whatever
@see		examples/real_ex.pl
@see		http://www.r-project.org/

*/

%%%

init_r_env :-
	% done
	getenv('R_HOME',_), !.
% windows is windows
init_r_env :-
	current_prolog_flag(windows, true),
	catch(win_registry_get_value('HKEY_LOCAL_MACHINE/Software/R-core/R','Current Version', Version),_,fail),
	atom_concat('HKEY_LOCAL_MACHINE/Software/R-core/R/',Version,SecondKey),
	catch(win_registry_get_value(SecondKey,'InstallPath', RPath),_,fail), !,
	% now we need to have the DLL in our path
	install_in_win32_path(RPath).
init_r_env :-
	% typical Linux 64 bit setup (fedora)
	current_prolog_flag(address_bits, 64),
	exists_directory('/usr/lib64/R'), !,
	setenv('R_HOME','/usr/lib64/R').
init_r_env :-
	% typical Linux  setup (Ubuntu)
	exists_directory('/usr/lib/R'), !,
	setenv('R_HOME','/usr/lib/R').
init_r_env :-
	% typical MacOs setup
	exists_directory('/Library/Frameworks'), !,
	install_in_osx.

install_in_win32_path(RPath) :-
	current_prolog_flag(address_bits, 64), !,
	getenv('PATH',OPath),
	atomic_list_concat([OPath,';',RPath,'\\bin\\x64'],Path),
	setenv('PATH',Path).
install_in_win32_path(RPath) :-
	getenv('PATH',OPath),
	atomic_list_concat([OPath,';',RPath,'\\bin\\i386'],Path),
	setenv('PATH',Path).

install_in_osx :-
	% typical MacOs setup
	exists_directory('/Library/Frameworks/R.framework/Resources'), !,
	setenv('R_HOME','/Library/Frameworks/R.framework/Resources').
install_in_osx :-
	current_prolog_flag(address_bits, 64),
	exists_directory('/Library/Frameworks/lib64/R'), !,
	setenv('R_HOME','/Library/Frameworks/lib64/R').
install_in_osx :-
	setenv('R_HOME','/Library/Frameworks/lib/R').

% this must be done before we load R
:- init_r_env.


% interface predicates

%%	init_r.
%	Start an R object. This is done automatically upon loading the library. We should add some checking that none exists here
%    before starting a new one. This is currently not the case.

%%	end_r.
%	Stop the R object and close the communication channel.

%% real( Version,  Date ).
%          Version and release date.
%
real( 0:0:3, 2011/12/22 ).

%%	'<-'(+Rvar).
%%	'<-'(+Rexpr).
%
%         If Rvar is an atom and a known R object, then print Rvar on R.
%         Else treat the input as an R expression and pass it on R for interpretation. (Throws result away, if expression
%         is not a <- expression itself).
%
'<-'(X) :-
     r(X).

%%	'<-'(+PLvar,+Rexpr).
%%	'<-'(+Rexpr1,+Rexpr2).
%%	'<-'(+Rvar,+PLval).
%%	'<-'(+PLvar,+Rvar).
%
%    Evaluate Rexpr and store its return value to PLvar.
%
%    Pass Rexpr1 <- Rexpr2 to R.
%
%    Pass Prolog value PLval to Rvar. If PLval is a single value, flat list or list of depth 2 use the low-level interface to pass the value to an R variable (see section on data transfer).
%
%    Place value of Rvar to PLvar.
%
%    Note that all Rexpr* are first processed as described in the section about syntax before passed to R.
%
'<-'(X,Y) :-
     r(X,Y ).

%% r( R )
%   Nickname for <-(R).
%
r( RvarIn ) :-
     is_rvar( RvarIn, Rvar ),
     !,
     r( print(Rvar) ).
r( R ) :-
     rexpr_codes( R, Rcodes ),
     !,
	send_r_command(Rcodes).
r( R ) :-
     catch( rterm_to_string(R,Rcodes,[]), _, fail ),
     !,
	send_r_command(Rcodes).
r( _Other ) :-
     write( user_error, 'Cannot use input to <-/1. \n ' ), nl,
     fail.

%%	r( ?L, +R ).
%    Nickname for <-(L,R).
%
r( Plvar, RvarIn ) :-
     var(Plvar),
     is_rvar(RvarIn,Rvar),
     !,
     robj_to_pl_term( Rvar, Plvar ).

r( RvarIn, PlrExpr ) :-
     rvar_identifier( RvarIn, Rvar ),
     !,
     assignment( PlrExpr, Rvar ).
r( Plvar, Rexpr ) :-
     var(Plvar),
     rexpr_codes( Rexpr, Rcodes ),
     !,
     rexpr_to_pl_term( Rcodes, Plvar ).
r( LRexpr, RRexpr ) :-
     catch( rexpr_codes('<-'(LRexpr,RRexpr),Rcodes),_,fail),
     !,
     atom_codes( Ratom, Rcodes ),
     write( sending(Ratom) ), nl,
     send_r_command( Rcodes ).
r( _Plvar, _Rexpr ) :-
     write( user_error, 'Cannot decipher modality of <-/2. \n ' ), nl,
     fail.

%%	is_rvar(+Rvar).
%         True if Rvar is an atom and a known variable in the R environment.
is_rvar( Rvar ) :-
     is_rvar( Rvar, _ ).
%%	is_rvar(+Rvar,-RvarAtom).
%         True if Rvar is an atom and a known variable in the R environment
%         and RvarAtom is the atomic representation of the Rvar term.
%
is_rvar( RvarIn, Rvar ) :-
     % atom( Rvar ),
     rvar_identifier( RvarIn, Rvar ),
     r_char( Rvar, Rchar ),
     rexpr_codes( exists(Rchar), Rcodes ),
     %here: rexpr_to_pl_term( Rcodes, true ).
     rexpr_to_pl_term( Rcodes, Rbool ),
     Rbool == true,
     rexpr_codes( mode(Rvar), Rmode ),
     rexpr_to_pl_term( Rmode, Plmode ),
     % atom_codes( Ratom, Rmode ),
     RvarModes  = [character,complex,list,logical,'NULL',numeric,raw],
     memberchk( Plmode, RvarModes ).

%% real_type( -Type ).
%  Enumerate or test all Types known to r..eal. These can be used as i <- Type:Values. Eg.
%  ==
%  i <- int:[1,2.0,3].
%  ==
%
real_type( Type ) :-
	val_type_real( Type, _ ).

%%	r_char( +Atomic, +RcharAtom ).
%   Wrap an atomic value with double quotes so it can pass as an R char type.
%
r_char( Atomic, Rchar ) :-
     atomic( Atomic ),
     !,
     atomic_list_concat( ['"',Atomic,'"'], Rchar ).
r_char( Rchar, Rchar ).

%%	devoff.
%  Close the current plot devise without any reporting. Short for <- invisible('dev.off'()').
devoff :-
     <- invisible('dev.off()').

%%% end of interface predicates

rexpr_codes( Rexpr, Rcodes ) :-
     atomic( Rexpr ),
     !,
     atom_codes( Rexpr, Rcodes ).
rexpr_codes( Rcodes, Rcodes ) :-
     Rcodes = [_|_],
     ascii_string( Rcodes ),
     !.
rexpr_codes( Rterm, Rcodes ) :-
     rterm_to_string( Rterm, Rcodes, [] ).
     % atom_codes( Ratom, Rcodes ),
     % write( sending(Ratom) ), nl.

assignment( PlData, Rvar ) :-
     ( number(PlData); PlData=[_|_]; boolean_atom(PlData); PlData = @_),
     !,
     set_r_variable(Rvar, PlData).
assignment( Rexpr, Rvar ) :-
     rexpr_codes( '<-'(Rvar,Rexpr), Rcodes ),
     !,
     send_r_command( Rcodes ).
/*
assignment( Rexpr, Rvar ) :-
     rexpr_codes( '<-'(Rvar,Rexpr), Rcodes ),
     send_r_command( Rcodes ).
     */


rvar_identifier( Rvar, Rvar ) :-
    atom( Rvar ).
rvar_identifier( A..B, Atom ) :-
     atom(A), atom(B),
     atomic_list_concat( [A,'.',B], Atom ).
/* fixme:
rvar_identifier( A^B ) :-
     % write( a(A)-b(B) ),
     is_list( B ).
     */


val_type_real( int, 1 ).
val_type_real( integer, 1 ).
val_type_real( float, 2 ).
val_type_real( char, 3 ).
val_type_real( codes, 3 ).
val_type_real( boolean, 4 ).
val_type_real( bool, 4 ).


rterm_to_string(V) -->
	{ var(V) }, !,
	{ throw(error(instantiation_error,r_interface)) }.
rterm_to_string(A) -->
	{ ascii_string(A) }, !,
        "\"", A, "\"".
rterm_to_string(A) -->
	% x.y
	{ A = [Head|Tail], Tail \= [_|_], Tail \= [] }, !,
        rterm_to_string(Head),
	".",
	rterm_to_string(Tail).
/* Deal with this later...
rterm_to_string(Array) -->
	{ Array = [_|_] },
	array_to_c(Array), !.
     */
rterm_to_string(A) -->
     { functor(A,Name,1),arg(1,A,'.')}, !,
       add_atom(Name), "()".
	% { rfunc(A) }, !, add_atom(A), "()".
rterm_to_string(A) -->
	{ atom(A) }, !,
        add_atom(A).
rterm_to_string(A) -->
	{ number(A) }, !,
	add_number(A).
rterm_to_string(A^List) -->
	{ atom(A), is_list(List) },
	add_atom(A),
	"[",
	rlist_to_string(List,first),
	"]".
% convert : to ., . has different associativity in Prolog.
rterm_to_string(A1..A2) --> { atom(A1) }, !,
	add_atom(A1),
	".",
	rterm_to_string(A2).
% R function definition
rterm_to_string((A1 :- A2)) -->
	!,
	rterm_to_string(A1),
	" ",
	rterm_to_string(A2).
rterm_to_string(S) -->
	{ functor(S, Na, 2), binary(Na), atom_codes(Na,NaS), arg(1,S,A1), arg(2,S,A2) }, !,
	rterm_to_string(A1),
	" ", NaS, " ",
	rterm_to_string(A2).
rterm_to_string(S) -->
	{ S =.. [F|Args], F \== '.' },
	add_atom(F),
	"(",
	rterms_to_string(Args, first),
	")".

rterms_to_string([], _) --> [].
rterms_to_string([Arg|Args], First) -->
	( { var(First) } -> "," ; "" ),
	rterm_to_string(Arg),
	rterms_to_string(Args, _).

rlist_to_string([],_) --> [].
rlist_to_string(.('*',L),Start) -->
	( { var(Start) } -> "," ; "" ),
	rlist_to_string(L,_).
rlist_to_string(.(A,L),Start) -->
	( { var(Start) } -> "," ; "" ),
	rterm_to_string(A),
	rlist_to_string(L,_).

ascii_string([]).
ascii_string(.(C,Cs)) :-
	integer(C),
	char_type(C,ascii),
        \+ char_type(C,cntrl),
	ascii_string(Cs).

ascii_list( List ) :-
     maplist( ascii_string, List ).

%
% a nil atom in Prolog is likely to be the empty string in R.
%
add_atom([]) --> !,
	"\"\"".
add_atom(A) -->
	{ atom_codes(A, Codes) },
	Codes.

add_number(El) -->
	{ number_codes(El, Codes) },
	Codes.


array_to_c(Array) -->
	{ Array = [[_|_]|_] }, !,
	/* matrix */
        "array(c(",
	flatten_matrix(Array, Dims, first),
	"),dim=",
	output_list(Dims),
	")".
array_to_c(Array) -->
	output_list(Array).

output_list(List) -->
	"c(",
	matrix_inside(List, _, first, 0, _),
	")".

flatten_matrix(Array, .(Length,Dims), Start) -->
	matrix_inside(Array, Dims, Start, 0, Length).

matrix_inside([], _, _, N, N) --> [].
matrix_inside(.(El,Array), Dims, Start, N0, N) -->
	matrix_el(El, Dims, Start),
	{ N1 is N0+1 },
	matrix_inside(Array, Dims, _, N1, N).

matrix_el(Array, Dims, Start) -->
	{ Array = [_|_] },
	flatten_matrix(Array, Dims, Start).
matrix_el(El, [], Start) -->
	( { var(Start) } -> ", " ; "" ),
	rterm_to_string(El).

% hmmmm
% originally this (binary/1) included a call to exist,
% this rightly fails on lm(speeds~exprs)
% we are converting this to an operators version and we might
% need to introduce a top-level version that checks for functions
binary( Rname ) :-
     current_op( _, Assoc, Rname ),
     once( binary_op_associativity( Assoc ) ).
     % atomic_list_concat( [exists,'("',Rname,'",mode="function")'],  Atom ),
     % atom_codes( Atom, Rcodes ),
     % rexpr_to_pl_term( Rcodes, Rbool ),
     % Rbool == true.

binary_op_associativity( yfx ).
binary_op_associativity( xfy ).
binary_op_associativity( xfx ).

boolean_atom( true ).
boolean_atom( false ).

:- initialization(init_r).
