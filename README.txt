
r..eal,  0:0:7, October 2012
---

r..eal is a c-based interface for connecting R to Prolog. 
See the documentation at doc/r..eal.html for more information.

This version works on current versions of SWI and YAP.
To get the latest git version of r..eal goto 

   git clone git://www.swi-prolog.org/home/pl/git/packages/real.git


INSTALL
---

Ungzip and untar the distribuition files into {swi,yap}/packages 
creating directory {swi,yap}/packages/real
 
cd into this directory

autoconf
autoheader

./configure

make 
make install



Test
---
 load 
 [real_ex].

In your prolog system.


---

Nicos Angelopoulos and Vitor Santos Costa

October, 2012.
