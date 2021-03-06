C Intermediate Language (CIL)
============================

CIL is a front-end for the C programming language that facilitates
program analysis and transformation. CIL will parse and typecheck a
program, and compile it into a simplified subset of C.

CIL supports ANSI C as well as most of the extensions of the GNU C and
Microsoft C compilers. A Perl script acts as a drop in replacement for
either gcc or Microsoft's cl, and allows merging of the source files in
your project. Other features include support for control-flow and
points-to analyses.

Installation
-----------

Run the following commands to build and install CIL:

    ./configure
    make
    make check      # very quick check, optionnal
    make install    # as root or using sudo

You only need the OCaml compiler, perl, and [ocamlfind][findlib] for the
installation.  (Of course, you also need some C compiler.)

[findlib]: http://projects.camlcity.org/projects/findlib.html

Usage
-----

You can use cilly (installed in /usr/local/bin by default) as a drop in
replacement for gcc to compile and link your programs.

You can also use CIL as a library to write your own programs.  For
instance in the OCaml toplevel using [findlib][]:

    $ ocaml
            Objective Caml version 3.12.0

    # #use "topfind";;
    [...]
    # #require "cil";;
    [...]
    # Cil.cilVersion;;           
    - : string = "1.4.0"


More documentation
------------------

The documentation is located in the doc/html/cil directory.  The API
documentation (generated by ocamldoc) is in the api subdirectory.

To (re)build the doc, you need [Hevea][] and run:

    make doc

You can also [browse the documentation online][doc].

[hevea]: http://hevea.inria.fr/ "Hevea - LaTex to HTML translator"
[doc]:   http://kerneis.github.com/cil/doc/html/cil "Cil online doc"

Test suite
----------

CIL comes with a test suite.  You need perl and [Hevea][] (because one
of the tests builds the documentation):

    cd test
    ./testcil --run

Ressources
----------

* [Mailing list](https://lists.sourceforge.net/lists/listinfo/cil-users)
* [Bug tracker](http://sourceforge.net/tracker/?group_id=138953&atid=742140)

CIL is maintained by Gabriel Kerneis <kerneis@pps.jussieu.fr> 
