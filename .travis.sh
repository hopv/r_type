#! /bin/bash

wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s .

eval `./opam config env`
./opam install -y omake.0.10.2 menhir core.v0.9.1 re2.v0.9.0 ppx_deriving.4.2 ppx_sexp_conv.v0.9.0 ppx_hash.v0.9.0 ppx_variants_conv.v0.9.0 ppx_fields_conv.v0.9.0 ppx_compare.v0.9.0 ppx_driver.v0.9.0
eval `./opam config env`

./opam --version

omake
