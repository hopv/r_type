#! /bin/bash

opam --version
ocamlc --version

eval $(opam config env)
opam update
eval $(opam env)

echo "installing dependencies"
opam install -y \
    jbuilder.1.0+beta20.2 menhir.20201216 omake.0.10.3 \
    core.v0.14.0 ppx_compare.v0.14.0 ppx_deriving.5.1 ppx_fields_conv.v0.14.1 \
    ppx_hash.v0.14.0 ppx_sexp_conv.v0.14.1 ppx_variants_conv.v0.14.1 \
    re2.v0.14.0
echo "done installing dependencies"
eval $(opam env)

opam --version
ocamlc --version
omake --version