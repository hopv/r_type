wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s .

eval `./opam config env`
./opam install -y omake.0.10.3 menhir core re2.0.9.0 ppx_deriving.4.2.1 ppx_sexp_conv.0.9.0 ppx_hash.0.9.0 ppx_variants_conv.0.9.0 ppx_fields_conv.0.9.0 ppx_compare.0.9.0 ppx_driver.0.9.1
eval `./opam config env`

./opam --version

omake
