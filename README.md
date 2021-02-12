![CI](https://github.com/hopv/r_type/workflows/CI/badge.svg)

# `r_type`

A model-checker for caml programs.

`r_type` translates caml programs to Horn clauses and feeds them to a Horn clause solver, such as
[hoice][hoice] for instance.

It supports a subset of caml including higher-order functions, nested recursive calls, integers and booleans. Floats, ADTs, modules... are currently not supported. You can get a sense of the fragment `r_type` supports by looking at [our benchmarks][benchs].

# Build

Make sure you have [opam][opam] installed, ideally switched to the latest stable ocaml compiler (`4.11.1` at the time of writing). `r_type` requires the following opam libraries:

- `dune.2.8.1` (build only)
- `menhir.20201216`
- `core.v0.14.0`
- `ppx_compare.v0.14.0`
- `ppx_deriving.5.1`
- `ppx_fields_conv.v0.14.1`
- `ppx_hash.v0.14.0`
- `ppx_sexp_conv.v0.14.1`
- `ppx_variants_conv.v0.14.1`
- `re2.v0.14.0`


So, a complete setup to compile `r_type` looks like

```bash
> opam update && opam upgrade
  # Switch to 4.11.1...
> opam switch 4.11.1
> eval `opam config env`
  # Install relevant packages...
> opam install -y dune.2.8.1 menhir.20201216 core.v0.14.0 ppx_compare.v0.14.0 ppx_deriving.5.1 ppx_fields_conv.v0.14.1 ppx_hash.v0.14.0 ppx_sexp_conv.v0.14.1 ppx_variants_conv.v0.14.1 re2.v0.14.0
```

Then, simply run `omake` at the root of this repository. The binary will be located at `src/r_type.opt` (`src/r_type` is a soft link to it).

This should work, but the most up to date build workflow is always the [travis build script][travis script].

# Running

```
Usage: r_type [options]* <caml_file>
NB: r_type verifies that function `main` from the input caml
    file never fails. Hence, make sure that entry point of your
    program is a function called `main`.
Options:
   -v                                       verbose output
  --effect_analysis    [on|true|off|false]  (de)activates effect analysis
    default 'on'
  --infer              [on|true|off|false]  (de)activates inference (prints the clauses on stdout if off)
    default 'on'
  --solver             <cmd>                command running the horn clause solver, e.g. `hoice` or `z3`
    default 'hoice'
```

You will need a Horn clause solver for `r_type` to do anything, such as [hoice][hoice] or [z3][z3]. The default is hoice using the command `hoice`.

If you want to use z3, or hoice but with a different command, pass the name of the command using `--solver <cmd>` when calling `r_type`.

**NB**: by default, z3 does *not* read from `stdin`, which `r_type` requires. Make sure you pass z3 the `-in` flag:

```bash
> rtype --solver "z3 -in" path_to_my_file.ml
```

If you only want to inspect the Horn clauses encoding the correctness of your caml program, run `r_type` with `--infer off`. The clauses will be printed on `stdout`.

[benchs]: https://github.com/hopv/benchmarks/tree/master/caml/lia (hopv benchmarks)
[travis script]: https://github.com/hopv/r_type/blob/master/.travis.sh (travis build script)
[hoice]: https://github.com/hopv/hoice (hoice repository on github)
[z3]: https://github.com/Z3Prover/z3 (z3 repository on github)
[opam]: https://opam.ocaml.org/doc/Install.html (opam official page)
