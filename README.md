# Welcome to Javalib

Javalib is a library that parses Java `.class` files into *OCaml* data
structures. Javalib offers primitives to extract information from,
manipulate, and generate valid `.class` files.

It is distributed under the *GNU Lesser General Public License* (see
the [LICENSE](LICENSE) file).

## Building Javalib

### Requirements

We recommend you to install every required library and *Ocaml* version with [opam](https://opam.ocaml.org/).

- [ocaml](http://caml.inria.fr/ocaml/release.en.html) >= 4.02.3
- [ocamlfind](http://projects.camlcity.org/projects/findlib.html) >= 1.5.1
- [camlzip](https://github.com/xavierleroy/camlzip) >= 1.05
- [camlp4](https://github.com/ocaml/camlp4)
- [extlib](https://github.com/ygrek/ocaml-extlib)
- [camomile](https://github.com/yoriyuki/Camomile)

### Configuration and installation

Configuring and installing *Javalib* from sources is pretty simple.
In the main repository, execute

     $ ./configure.sh
     $ make
     $ make install

To remove the library from your system, just do

     $ make remove

## Using Javalib

Before being able to use the modules presented in the [API documentation](http://javalib.gforge.inria.fr/javalib-api), you need to `open Javalib_pack;;`.

*Javalib* has just been ported from a *svn* repository hosted by [Inria Gforge](https://gforge.inria.fr/projects/javalib/) to [GitHub](https://github.com/javalib-team/javalib/). More documentation will we available soon in the project Wiki, but in the meantime please refer to the available [website](http://sawja.inria.fr/).
