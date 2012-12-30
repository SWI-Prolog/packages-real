
r..eal,  0:1:0, December 2012
---

r..eal is a c-based interface for connecting R to Prolog. 
See the documentation at doc/html/real.html for more information.
There is also a paper doc/padl2013-real.pdf and a user's guide
doc/guide.pdf .

This version works on current versions of SWI and YAP.
To get the latest git version of r..eal goto 

   git clone git://www.swi-prolog.org/home/pl/git/packages/real.git


INSTALL
---

See doc/guide.pdf for instructions about stable versions.

To install developmental versions do:

SWI-Prolog

 Install last stable r..eal with 
 ?- pack_install( real ).
 
 do 
 ?- pack_rebuild( real ).  % to force SWI to build from sources.

 And then replace c/real.c, prolog/real.pl and examples/for_real.pl 
 in the stable installation by the development version.

 and do again:

 ?- pack_rebuild( real ).

Yap: 
replace $YAP/packages/real with the development version you downloaded
and rebuilt Yap with 

configure --with-R


Test
---
 ?- [examples/for_real].
 ?- for_real.

In your prolog system.


---

Nicos Angelopoulos and Vitor Santos Costa

December, 2012.
