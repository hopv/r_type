#! /bin/bash

./opam --version

eval `./opam config env`
./opam update
eval `./opam config env`
./opam install -y core.v0.14.0 jbuilder.1.0+beta20.2 menhir.20201216 ocaml-base-compiler.4.11.1 omake.0.10.3 ppx_compare.v0.14.0 ppx_deriving.5.1 ppx_fields_conv.v0.14.1 ppx_hash.v0.14.0 ppx_sexp_conv.v0.14.1 ppx_variants_conv.v0.14.1 re2.v0.14.0
eval `./opam config env`

./opam --version
