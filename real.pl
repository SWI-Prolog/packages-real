%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Author:        Nicos Angelopoulos, Vitor Santos Costa, Jan Wielemaker
%    E-mail:        Nicos Angelopoulos <nicos@gmx.co.uk>
%    Copyright (C): Nicos Angelopoulos, Universidade do Porto, VU University Amsterdam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of r..eal
%  distributed according to Perl Artistic License
%  check LICENSE file for distribution license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
     real_version/2,
     real_citation/2,
     r_char/2,
     devoff/0,
     r_wait/0,
	(<-)/2,
	op(600,xfy,~),
	op(600,yfx,'..'),
	op(400,yfx,'%x%'),
	op(400,yfx,'%%'),
	op(400,yfx,'%/%'),
	op(400,yfx,'%*%'),
	op(400,yfx,'%o%'),
	op(400,yfx,'%in%'),
	op(400,yfx,$),
	op(400,yfx,@),
	op(800,fx,@),
     op(400,xfy,=+ )
     ]).

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- use_module(library(readutil)).
:- use_module(library(debug)).

:- ( current_prolog_flag(windows,true) ->
      nl, nl,
      write( '!!!   r..eal notice: There is a known issue on Windows where <- print(x) does not print x to the terminal.' ),
      nl, nl
      ;
      true
   ).

/** <module> r..eal

A C-based  Prolog interface to R.

---++ Introduction

This library enables the communication with an R process started as a shared library. 
It is the result of the efforts of two research groups that have worked in parallel.
The syntactic emphasis on a minimalistic interface.  A single predicate (<-/2,<-/1) channels
the bulk of the interactions. In addition to using R as a shared library, r..eal uses
the c-interfaces of SWI/Yap and R to pass objects in both directions.
The usual mode of operation is to load Prolog values on to R variables and then call
an R function on these values. The return value of the called function can be either placed
on R variable or passed back to Prolog.  It has been tested extensively on current
SWI and YAP on Linux machines but it should also compile and work on MS operating systems and Macs.

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

Pass PLval to Rvar (using the c-interface when Rvar is numeric, list or list of 2 levels).
See data transfer below.  Assign contents of Rvar to PLvar using the c-interface.

---++ Syntax

There are syntactic conventions in R that make unparsable prolog code. 
Notably function and variable names are allowed to contain dots, square brackets are used
to access parts of vectors and arrays and functions are allowed empty argument tuples.
We have introduced relevant syntax which allows for easy transition between prolog and R.
Prolog constructs are converted by the library as follows:


     * =|..|= within atoms  ->  =|.|= (ex. =| as..integer(c(1,2,3)) ->  as.integer(c(1,2,3))|= )
     * =|^[]|=  after atoms -> =|[]|= (ex. =|a^[2] -> a[2] |=)
     * =|(.)|= at the end of atoms that are known R functions -> =|()|=  (ex. =|dev..off(.) -> dev.off()|= )
     * =|[]|= -> c() (which equal to R's NULL value)
     * ( f(x) :-  (..))   -> f(x) (...)
     * Lists of lists of numbers are converted to matrices. All first level lists must have the same length.
     * Filenames must be given as Prolog strings.
     * R specific operators (eg. %*% should be quoted in Prolog.
     * If you want to ensure that you quote a string, use +String.
     * If you want to ensure that you do not quote a string, use -String.
     * expressions that pose difficulty in translation can always be passed as unquoted Prolog atoms or strings.


---++ Data transfers

Following R convension we will talk about the type-of and the class-of data.
i, defined by i <- as.integer(c(1,2,3)), is of class integer vector and type integer.
R vectors are mapped to prolog lists and matrices are mapped to nested lists.
The convention works the other way around too.

There are two ways to pass prolog data to R. The more efficient one is by using
==
 Rvar <- PLval
==

We will call this the low-level interface. It is implemented as C code and transfers via C data
between R and Prolog. In what follows atomic PLval data are simply considered as singleton lists.
Flat Plval lists are translated to R vectors and lists of one level of nesting to R matrices
(which are 2 dimensional arrays in R parlance). The type of values of the vector or matrice is
taken to be the type of the first data element of the Plval according to the following :

     * integer -> integer
     * float   -> double
     * atom    -> char
     * boolean -> logical

Booleans are represented in prolog as true/false atoms.
Currently arrays of aribtrary dimensions are not supported in the low-level interface.
Note that in R a scalar is just a one element vector.  When passing non-scalars the interface will
assume the type of the object is that of the first scalar until it encounters something different.
R..eal will currently will re-start and repopulate partial integers for floats as follows: 

==
r <- [1,2,3].
R <- r.
R = [1, 2, 3].

i <- [1,2,3.1].
I <- i.
I = [1.0, 2.0, 3.1].


==

However, not all possible "corrections" are currently supported. For instance, 

==
?- c <- [a,b,c,1].
ERROR: real:set_r_variable/2: Type error: `boolean' expected, found `a'
==

In the low level interface we map prolog atoms to R strings-

==
?- x <- [abc,def].
true.

?- <- x.
[1] "abc" "def"
true.

?- X <- x.
X = [abc, def].

==

In addition Prolog data can be passed through the expression mechanism.
That is, data appearing in an arbitrary R expression will be parsed and be part of the long
string that will be passed from Prolog to R for evaluation.  
This is only advisable for short data structures. For instance,

==
     tut_4a :-
          state <- c("tas", "sa",  "qld", "nsw", "nsw"),
          <- state.
==

Through this interface it is more convenient to represent R chars by Prolog list of codes,
as in the above example.

---++ Examples

==

?- e <- numeric(.).
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

rtest :-
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
     r_wait,
	devoff.

tut6 :-
	d <- outer(0:9, 0:9),
	fr <- table(outer(d, d, "-")),
	<- plot(as..numeric(names(fr)), fr, type="h", xlab="Determinant", ylab="Frequency").

tut4b :-
     state <- [tas,sa,qld,nsw,nsw,nt,wa],
     statef <- factor(state),
     incmeans <- tapply( c(60, 49, 40, 61, 64, 60, 59), statef, mean ),
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

κρότος;~/sureal% yap -f -g "ensure_loaded(library(real))"
YAP 6.3.0 (x86_64-linux): Fri Nov 11 16:06:00 CET 2011
MYDDAS version MYDDAS-0.9.1
?- 
   findall( I, between(1,1000000,I), Is), i <- Is, K <- i, 
   length( K, Len ), write( len(Len) ), nl, fail.

len(1000000)

==

---++ Info

Many thanks to the original YapR developers for inspiration and some functions.

@author	Nicos Angelopoulos
@author	Vitor Santos Costa
@author	Jan Wielemaker
@version	0:0:7, 2012/10/09
@license	Perl Artistic License
@see		examples/for_real.pl
@see		http://www.r-project.org/

*/

%%%

init_r_env :-
	% done
	getenv('R_HOME',_), !.
% windows is windows
init_r_env :-
	current_prolog_flag(windows, true),
	( HKEY='HKEY_LOCAL_MACHINE/Software/R-core/R'; 
		HKEY='HKEY_CURRENT_USER/Software/R-core/R' ),
	catch(win_registry_get_value(HKEY,'Current Version', Version),_,fail),
	!,
	atomic_list_concat([HKEY,Version],'/',SecondKey),
	catch(win_registry_get_value(SecondKey,'InstallPath', RPath),_,fail), !,
	setenv('R_HOME',RPath), % this probably does not help (at least not XPs)
	% now we need to have the DLL in our path
     % nicos: although on xp it seems that path has to already be set.
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
     % if you have problems with R associated dlls, you might also want to add:
	% atomic_list_concat([IPath,';',RPath,'\\modules\\i386'],Path),
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

% interface predicates

%%	init_r.
%	Start an R object. This is done automatically upon loading the library. 
%    We should add some checking that none exists here
%    before starting a new one. This is currently not the case.
start_r :-
	init_r_env,
	use_foreign_library(foreign(real)),
	init_r.

%%	'<-'(+Rvar).
%%	'<-'(+Rexpr).
%
%         If Rvar is an atom and a known R object, then print Rvar on R.
%         Else treat the input as an R expression and pass it on R for interpretation.
%        (Throws result away, if expression is not a <- expression itself).
%
'<-'(X) :-
     r(X).

%%	'<-'(+PLvar,+Rexpr).
%%	'<-'(+Rexpr1,+Rexpr2).
%%	'<-'(+Rvar,+PLval).
%%	'<-'(+PLvar,+Rvar).
%
%  Evaluate Rexpr and store its return value to PLvar.
%
%  Pass Rexpr1 <- Rexpr2 to R.
%
%  Pass Prolog value PLval to Rvar. If PLval is a single value, flat list or list of depth 2
%  use the low-level interface to pass the value to an R variable (see section on data transfer).
%
%  Place value of Rvar to PLvar.
%
%  Note that all Rexpr* are first processed as described in the section about syntax before passed to R.
%
'<-'(X,Y) :-
     r(X,Y ).

%% r( R )
%   Nickname for <-(R).
%
r( RvarIn ) :-
     (  rvar_identifier(RvarIn,_,RvarCs) 
        ; (atom(RvarIn),atom_codes(RvarIn,RvarCs))
     ),
	!,
	append(["print( ",RvarCs," )"], CmdCodes),
	send_r_codes( CmdCodes ).
r( R ) :-
	rexpr_codes(R,TmpRs,Rcodes,[]),
	!,
	send_r_codes(Rcodes),
	maplist( r_remove, TmpRs ).
r( _Other ) :-
	write( user_error, 'Cannot use input to <-/1.' ), nl, nl,
	fail.

%%	r( ?L, +R ).
%    Nickname for <-(L,R).
%
%    Plvar <- Rvar.
%
r( Plvar, RvarIn ) :-
     var(Plvar),
     rvar_identifier( RvarIn, RvarIn, _ ),
     !,
     robj_to_pl_term( RvarIn, Plvar ).
%   Plvar <- Rexpr.
r( Plvar, Rexpr ) :-
     var(Plvar),
     rexpr_codes( Rexpr, TmpRs, Rcodes ),
     !,
     debug_send_codes( ["rexpr_to_pl_term(",Rcodes,",_Var)"] ),
     rexpr_to_pl_term( Rcodes, Plvar ),
     maplist( r_remove, TmpRs ).
%  Rvar <- Plval.
r( RvarIn, PlrExpr ) :-
     assignment( PlrExpr, RvarIn ),
     !.
%  Rexpr1 <- Rexpr2
r( LRexpr, RRexpr ) :-
     rexpr_codes('<-'(LRexpr,RRexpr),TmpRs,Rcodes),
     !,
     send_r_codes( Rcodes ),
     maplist( r_remove, TmpRs ).
r( _Plvar, _Rexpr ) :-
     write( user_error, 'Cannot decipher modality of <-/2. \n ' ), nl,
     fail.

%%	is_rvar(+Rvar).
%         True if Rvar is an atom and a known variable in the R environment.
is_rvar( Rvar ) :-
     is_rvar( Rvar, _ ).
%%	is_rvar(+Rvar,-RvarAtom).
%         True if Rvar is a term and a known variable in the R environment.
%         RvarAtom is the atomic representation of the Rvar term.
%
is_rvar( RvarIn, Rvar ) :-
	atom(RvarIn), !,
	is_r_variable(RvarIn),
	RvarIn = Rvar.
is_rvar( RvarIn, Rvar ) :-
     rvar_identifier( RvarIn, Rvar, _RvarAtom ),
     is_r_variable( Rvar ),
     rexpr_codes( mode(Rvar), [], Rmode ),
     rexpr_to_pl_term( Rmode, Plmode ),
     RvarModes  = [character,complex,list,logical,'NULL',numeric,raw,'S4'],
     memberchk( Plmode, RvarModes ).

%%	r_char( +Atomic, +RcharAtom ).
%   Wrap an atomic value with double quotes so it can pass as an R char type.
%
r_char( Atomic, Rchar ) :-
    atomic( Atomic ),
    !,
    atomic_list_concat( ['"',Atomic,'"'], Rchar ).

%%	devoff.
%  Close the current plot devise without any reporting. Short for <- invisible('dev.off'()').
devoff :-
	<- invisible(-'dev.off()').

%% r_wait
%         Currently only waiting for Return to be pressed.
%
r_wait :-
     write('Press Return to continue...'), nl,
     read_line_to_codes(user_input, _).

%% real_version( Version,  Date ).
%          Version and release date.
%
real_version( 0:0:9, date(2012,12,19) ).   % the oliebollen version

%% real_citation( -Atom, -Bibterm ).
% Succeeds once for each publication related to this library. Atom is the atom representation
% suitable for printing while Bibterm is a bibtex(Type,Key,Pairs) term of the same publication.
% Produces all related publications on backtracking.
real_citation( Atom, bibtex(Type,Key,Pairs) ) :-
    Atom = 'Integrative functional statistics in logic programming \n Nicos Angelopoulos, Vítor Santos Costa, Joao Azevedo, Jan Wielemaker, Rui Camacho and Lodewyk Wessels \n Proc. of Practical Aspects of Declarative Languages (PADL 2013). Accepted (January, 2013. Rome, Italy).',
    Type = inproceedings,
    Key  = 'AngelopoulosN+2012',
    Pairs = [
               author = 'Nicos Angelopoulos and Vitor Santos Costa and Joao Azevedo and Jan Wielemaker and Rui Camacho and Lodewyk Wessels',
               title  = 'Integrative functional statistics in logic programming',
               booktitle = 'Proc. of Practical Aspects of Declarative Languages}',
               year = 2013,
               month = 'January',
               address = 'Rome, Italy',
               url     = 'http://bioinformatics.nki.nl/~nicos/pbs/padl2013-real.pdf'
     ].

%%% end of interface predicates

% maybe add this to the interface ?
r_remove( Plvar ) :-
     <- remove( Plvar ).

debug_send_codes( RcodesNest ) :-
     debugging( real ),
     !,
     flatten( RcodesNest, Rcodes ),
     atom_codes( Ratom, Rcodes ),
     format( user_error, 'Sending to R: ~w\n', [Ratom] ).
debug_send_codes( _ ).

send_r_codes( Rcodes ) :-
     debug_send_codes( Rcodes ),
     send_r_command( Rcodes ).

rexpr_codes( Rterm, RTmps, Rcodes ) :-
     rexpr_codes( Rterm, RTmps, Rcodes, [] ).

assignment(PlDataIn, Rvar) :-
     % atom( Rvar ),
     rvar_identifier( Rvar, Rvar, _ ),
     functor( PlDataIn, c, _Arity ),
     send_c_vector(PlDataIn, Rvar), !.

assignment(PlDataIn, Rvar) :-
     % atom( Rvar ),
          % we would like to use rvar_identifier here, instead of atom/1
          % but a$b <- 3 does not work with set_r_variable/2.
     rvar_identifier( Rvar, Rvar, _ ),
     pl_data( PlDataIn, PlData ),
     !,
     % term_to_atom( RvarIn, RvarAtom ),
     set_r_variable(Rvar, PlData).

assignment( Rexpr, Rvar ) :-
     rvar_identifier( Rvar, _Rvar, RAssgn ),
     rexpr_codes( '<-'(-RAssgn,Rexpr), TmpRs, Rcodes ),
     !,
     send_r_codes( Rcodes ),
     maplist( r_remove, TmpRs ).

pl_data( PlData, PlData ) :-
     ( number(PlData); PlData=[_|_]; boolean_atom(PlData); PlData = @(_) ).
/*
pl_data( PlDataIn, PlData ) :-
     PlDataIn =.. [c|PlData].
*/

/** rvar_identifier( Rterm, Rvar, Rcodes ).

True if Rterm is an access term for an R variable Rvar and Rcodes
are the codes corresponding to Rterm. Note that it is not the
case that term_to_codes( Rterm, Rcodes ) holds. Rterm might contain code lists
that are contextually interpreted by R as slots or list item labels.
Or, Rterm might contain indices that we translate.

*/

rvar_identifier( Rt, Rv, Rc ) :-
     rvar_identifier_1( Rt, Rv, Ra ),
     !,
     % is_r_variable( Rv ),
     atom_codes( Ra, Rc ).

rvar_identifier_1( Rvar, Rvar, Rvar ) :-
	atom( Rvar ),
     ( catch(term_to_atom(Atom,Rvar),_,fail) ),
     Atom == Rvar.
rvar_identifier_1( A..B, Atom, Atom ) :-
	atom(B), 
	rvar_identifier_1( A, Aatom, _ ),
	atomic_list_concat( [Aatom,'.',B], Atom ).
rvar_identifier_1( A$B, Rv, C ) :-
     rname_atom( B, Batom ),
	rvar_identifier_1( A, Rv, Aatom ),
	% term_to_atom( Aatom$Batom, C ).
     atomic_list_concat( [Aatom,'$',Batom], C ).
rvar_identifier_1( A@B, Rv, C ) :-
     rname_atom( B, Batom ),
	rvar_identifier_1( A, Rv, Aatom ),
     atomic_list_concat( [Aatom,'@',Batom], C ).
	% term_to_atom( Aatom@Batom, C ).
rvar_identifier_1( A^[[B]], Rv, C ) :-
     rvar_identifier_1( A, Rv, Aatom ),
     rexpr_codes(B, [], BCs, [] ),
     atom_codes( Batom, BCs ),
     atomic_list_concat( [Aatom,'[[',Batom,']]'], C ).
     % atomic_list_concat( [Aatom,'[["',Batom,'"]]'], C ).

rvar_identifier_1( A^B, A, C ) :-
	atom( A ),
	is_list( B ),
     indices_to_string( B, BCs, [] ),
	% term_to_atom( B, Batom ),
     atom_codes( Batom, BCs ),
	atom_concat( A, Batom, C ).

/** rexpr_codes(V,_,_).

     Generate (or parse) an R expression as codes from/to a Prolog term.
*/
rexpr_codes(V,[]) -->
	{ var(V) }, !,
	{ throw(error(instantiation_error,r_interface)) }.
rexpr_codes(+A,[]) -->
	!,
	{ atom(A) -> format(codes(S), '~a', [A]) ; format(codes(S), "~s", [A]) },
	"\"", S, "\"".
rexpr_codes(-A,[]) -->
	!,
	{ atom(A) -> format(codes(S), '~a', [A]) ; format(codes(S), "~s", [A]) },
	S.
rexpr_codes(=+(A,B),List) -->
	!,
	rexpr_codes((A = +B),List).
rexpr_codes(Array,TmpRs) -->
	{ Array = [_|_] },
	array_to_c(Array,TmpV), !,
	{ TmpRs = [TmpV] }.
rexpr_codes(A,[]) -->
	{ functor(A,Name,1),arg(1,A,'.')}, !,
	add_atom(-Name), "()".
/*   This can be used if we want c() to be passed by lists, 
but it currently does not accommodate c(1:3,1:3)
rexpr_codes(A,List) -->
	{ 
         A =.. [c|B], B \== []
        },
	!,
	rexpr_codes(B,List).
        */
/* atom is already protected */
/*
rexpr_codes(A,[]) -->
	{ atom(A), is_rvar(A, _) }, !,
        add_atom(-A).
        */
rexpr_codes(A,[]) -->
	/* string */
	{ atom(A) }, !,
	add_atom(-A).
rexpr_codes(A,[]) -->
	{ number(A) }, !,
	add_number(A).
rexpr_codes(A^[[Key]], TmpRs) -->
     !,
	% rexpr_unquoted(A, [] ),
	rexpr_codes(A, Atmp ),
     "[[", rexpr_codes(Key, Ktmp), "]]",
     { append(Atmp,Ktmp,TmpRs) }.
rexpr_codes(A^List, TmpRs) -->
	{ is_list(List) }, !,
	rexpr_codes(A, TmpRs),
	% rexpr_unquoted(A, TmpRs),
	indices_to_string( List ).
rexpr_codes(A$B,TmpA) -->
	!,
	rexpr_codes( A, TmpA ),
	% rexpr_unquoted( A, TmpA ),
	"$",
	add_name( B ).
rexpr_codes(A@B,TmpA) -->
	!,
	rexpr_codes( A, TmpA ),
	% rexpr_unquoted( A, TmpA ),
	"@",
	add_name( B ).
rexpr_codes(A1..A2,TmpRs) --> !,
	% rexpr_unquoted(A1, TmpRs1),
	rexpr_codes(A1, TmpRs1),
	".",
	% rexpr_unquoted(A2, TmpRs2),
	rexpr_codes(A2, TmpRs2),
     { append(TmpRs1, TmpRs2, TmpRs) }.
% R function definition
rexpr_codes((A1 :- A2), TmpRs) -->
	!,
	rexpr_codes(A1,TmpA1),
	" ",
	rexpr_codes(A2,TmpA2),
	{append(TmpA1,TmpA2,TmpRs)}.
rexpr_codes(S,TmpRs) -->
	{ functor(S, Na, 2),  
	  binary(Na), atom_codes(Na,NaS),
          arg(1,S,A1), arg(2,S,A2)
        }, !,
        { ( (Na=(<-);Na=(=)) -> Lft = "", Rgt = ""; Lft = "(", Rgt = ")" ) },
        Lft,
	rexpr_codes(A1,TmpA1),
	" ", NaS, " ",
	rexpr_codes(A2,TmpA2),
	Rgt,
        {append(TmpA1,TmpA2,TmpRs)}.
rexpr_codes(S,TmpRs) -->
	{ S =.. [F|Args], F \== '.' },
	add_atom( -F ),
	"(",
	rexprs_codes(Args, true, F, TmpRs),
	")".

rexprs_codes([], _, _, []) --> [].
rexprs_codes([Arg|Args], Fin, Func, TmpRs) -->
	( { Fin == true } -> "" ; " ," ),
	rexpr_codes(Arg, TmpA),
	% { N1 is N+1 },
	rexprs_codes(Args, false, Func, TmpAs),
	{append(TmpA, TmpAs, TmpRs)}.

/* trying to remove this...
rexpr_unquoted(A, TmpRs) -->
	( { atom(A) } -> 
	    add_atom(-A), { TmpRs = [] }
	; 
	  rexpr_codes(A , TmpRs)
        ).
        */

/* obsolete ?
literal(1, library).
literal(1, require).
*/

indices_to_string( List ) -->
	"[",
	index_to_string( List ),
	"]".

index_to_string( [] ) --> [].
index_to_string( [H|T] ) --> 
     index_element_to_string( H ),
     index_comma( T ),
     index_to_string( T ).

index_element_to_string( * ) -->
     [].
index_element_to_string( List ) -->
     { is_list(List) },
     !,
     indices_to_string( List ).
     
index_element_to_string( Num ) -->
     { write_to_chars(Num,Codes) },
     Codes.

index_comma( [] ) --> !, [].
index_comma( _ ) -->
     !,
     ",".

/* obsolete ?
%% codes_string(Codes,Quoted).
% check a list is full of (utf ?) codes
% while replacing any " with \" to produce Quoted from Ascii
%
codes_string([],[]).
codes_string(.(C,Cs),Q) :-
	integer(C),
	% <=nicos.  char_type(C,ascii),
        % <=nicos.   \+ char_type(C,cntrl),
	char_my_utf8(C),
	sew_code( C, Q, T ),
	codes_string(Cs,T).

char_my_utf8( C ) :-
	char_type(C,graph),
	!.
char_my_utf8( C ) :-
	char_type(C,white).

%% ascii_code_sew( C, Q, T ).
%  Sew C or its quoted form on list Q with its tail returned in T.
%
sew_code( 34, [0'\\,0'"|T], T ) :- !.
sew_code( C, [C|T], T ).
*/

%% add_name( Name ).
%
% first cut in supporting places where R is expecting "names or string constants"
% as in the RHS of $ and @
%
add_name( Name ) --> 
     { ( atomic(Name)  -> Fo = '~a'; Fo = '~s' ),
	  format(codes(Out), Fo, [Name])
     },
	Out.
     
%% rname_atom( Rname, Atom ).
%
%  Holds for atomic Atom a map of Rname.
%  If Rname is a list is assumed to be a list of codes that is 
%  atom_code(/2)d to Atom.
%
rname_atom( Rname, Atom ) :-
     ( atomic(Rname) ->
          Atom = Rname
          ;
          atom_codes( Rname, Atom )
     ).

%
% a nil atom in Prolog is likely to be the empty string in R.
%
add_atom([]) --> !,
	"\"\"".
add_atom( -A ) --> 
	!,
	{ format(codes(Out), '~a', [A]) },
	Out.
add_atom([C|Codes] ) --> 
	!,
	{ format(codes(Out), '"~s"', [C|Codes]) },
	Out.
% handle atoms that are explicitly quoted (r_char)
add_atom(A) -->
	{ atom_codes(A, Codes) },
	check_quoted(A, Codes).
	
check_quoted(true, _) --> !, "TRUE".	
check_quoted(false, _) --> !, "FALSE".
check_quoted(A, _) --> { is_r_variable(A) }, !,
	{ format(codes(Codes), '~a', [A]) },
	Codes.	
check_quoted(A, _) -->
	{ format(codes(Codes), '"~a"', [A]) },
	Codes.


/* no longer used ? 
add_string_as_atom( [] ) --> [] .
add_string_as_atom( [H|Tail] ) -->
	( { H =:= "\"" } -> 
            ( { Tail == [] } -> "\"" ; "\\\"" )
        ;
        [H]
        ),
	add_string_as_atom( Tail ).
     */


add_number(El) -->
	{ number_codes(El, Codes) },
	Codes.

array_to_c(Array,Rv) -->
     { 
          fresh_r_variable(Rv),
          set_r_variable(Rv,Array),
          atom_codes(Rv,RvCodes)
     },
     RvCodes.

fresh_r_variable(Plv) :-
     between( 1, 10000, I ),
     atomic_list_concat([pl,v,I], '_', Plv),
     \+ is_rvar(Plv),
     !.

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

% error handling
:- multifile prolog:message//1.

/*
prolog:message(What) -->
     { write( here(What) ), nl, fail}.
     */

prolog:message(unhandled_exception(real_error(Message))) -->
	message(Message).

prolog:message(real_error(Message)) -->
	message(Message).

message( correspondence ) -->
     ['R was unable to digest your statement, either syntax or existance error.' - [] ].

:- initialization(start_r, now).
