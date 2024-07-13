# Welcome to Javalib

Javalib is a library that parses Java `.class` files into *OCaml* data
structures. Javalib offers primitives to extract information from,
manipulate, and generate valid `.class` files.

It is distributed under the *GNU Lesser General Public License* (see
the [LICENSE](LICENSE) file).

## Building Javalib

### Requirements

We recommend you to install every required library and *Ocaml* version with [opam](https://opam.ocaml.org/).

- [ocaml](http://caml.inria.fr/ocaml/release.en.html) >= 4.08
- [camlzip](https://github.com/xavierleroy/camlzip) >= 1.11
- [extlib](https://github.com/ygrek/ocaml-extlib)

### Configuration and installation

Configuring and installing *Javalib* from sources is pretty simple.
In the main repository, execute

     $ dune build

To clean the sources, run

     $ dune clean

## Using Javalib

Before being able to use the modules presented in the [API documentation](https://javalib-team.github.io/javalib/doc/api/), you need to `open Javalib_pack`.

To get started with *Javalib*, read the [tutorial](https://github.com/javalib-team/javalib/wiki).
