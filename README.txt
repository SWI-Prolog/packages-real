
Real
---

Real is a c-based interface for connecting R to Prolog. 
See the documentation at doc/html/real.html for more information.
There is also a paper doc/padl2013-real.pdf and a user's guide
doc/guide.pdf.

Real works on current versions of SWI and YAP.
As of version 1.1 there is support for using Real on SWI web-servers.


INSTALL
---

To install stable versions: 

 SWI-Prolog

   ?- pack_intall( real ).

 Yap 
   install Yap itself with
   configure --with-R

See doc/guide.pdf for further details on stable versions.

Git access
---

To get the latest git version goto 

   git clone git://www.swi-prolog.org/home/pl/git/packages/real.git

To install developmental versions do:

 SWI-Prolog (a)

 Install last stable real with 
 ?- pack_install( real ).
 
 do 
 ?- pack_rebuild( real ).  % to force SWI to build from sources.

 And then replace c/real.c, prolog/real.pl and examples/for_real.pl 
 in the stable installation by the development version.

 and do again:

 ?- pack_rebuild( real ).

 SWI-Prolog (b)
   clone real into pl-devel/packages/real
	add this to your build script
	export EXTRA_PKGS="real"
	build SWI from sources as per usual

 Yap
   replace $YAP/packages/real with the development version you downloaded
   and rebuilt Yap with 

   configure --with-R

Test
---
 SWI

   ?- [pack('pack/real/examples/for_real')]. 
   ?- for_real.

 Yap
   ?- [examples/for_real].
   ?- for_real.


---
Nicos Angelopoulos and Vitor Santos Costa
December, 2012.

Updates: Nicos Angelopoulos
Dec. 2013,
March, 2014
