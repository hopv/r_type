wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s .

eval `./opam config env`
./opam install -y omake menhir core re2 ppx_deriving ppx_sexp_conv ppx_hash ppx_variants_conv ppx_fields_conv ppx_compare ppx_driver
eval `./opam config env`

./opam -version
ocaml -version

omake