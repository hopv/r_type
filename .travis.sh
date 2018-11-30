#! /bin/bash

wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s .

eval `./opam config env`
./opam update
eval `./opam config env`
opam install -y conf-m4 base-num result ocamlbuild.0.11.0 num ocamlfind.1.7.1 topkg.0.8.0 ppx_tools.5.0+4.05.0 omake.0.10.3 menhir.20161114 jbuilder.1.0+beta16 base-bytes octavius.0.2.0 spawn.v0.9.0 sexplib.v0.9.3 ppx_traverse_builtins.v0.9.0 ppx_derivers.1.0 ocaml-migrate-parsetree.1.0 ocaml-compiler-libs.v0.9.0 jane-street-headers.v0.9.0 re.1.7.0 cppo.1.6.0 base.v0.9.4 ppx_ast.v0.9.1 cppo_ocamlbuild.1.6.0 typerep.v0.9.0 stdio.v0.9.1 ppx_deriving.4.2.1 ppx_core.v0.9.3 ppx_optcomp.v0.9.0 ppx_driver.v0.9.2 variantslib.v0.9.0 ppx_metaquot.v0.9.0 ppx_let.v0.9.0 ppx_here.v0.9.1 fieldslib.v0.9.0 ppx_type_conv.v0.9.1 ppx_pipebang.v0.9.0 ppx_optional.v0.9.0 ppx_js_style.v0.9.0 ppx_inline_test.v0.9.2 ppx_fail.v0.9.0 ppx_variants_conv.v0.9.0 ppx_typerep_conv.v0.9.0 ppx_traverse.v0.9.0 ppx_sexp_conv.v0.9.0 ppx_fields_conv.v0.9.0 ppx_enumerate.v0.9.0 ppx_compare.v0.9.0 ppx_bench.v0.9.1 ppx_sexp_value.v0.9.0 ppx_sexp_message.v0.9.0 ppx_custom_printf.v0.9.0 ppx_hash.v0.9.0 ppx_assert.v0.9.0 bin_prot.v0.9.2 ppx_base.v0.9.0 ppx_expect.v0.9.0 ppx_bin_prot.v0.9.0 configurator.v0.9.0 ppx_jane.v0.9.0 core_kernel.v0.9.1 re2.v0.9.0 core
eval `./opam config env`

./opam --version

omake
