# `r_type`

[![Build Status](https://travis-ci.org/hopv/r_type.svg?branch=master)](https://travis-ci.org/hopv/r_type)

A model-checker for caml programs.

`r_type` translates caml programs to Horn clauses and feeds them to a Horn clause solver, such as [hoice][hoice] for instance.

It supports a subset of caml including higher-order functions, nested recursive calls, integers and booleans. Floats and ADTs are currently not supported.

# Build

Make sure you have [opam][opam] installed, ideally switched to the latest stable ocaml compiler (`4.05.0` at the time of writing). `r_type` requires the following opam libraries:

- `omake.0.10.2` (build only)
- `menhir`
- `core`
- `re2`
- `ppx_deriving.4.2.1`
- `ppx_sexp_conv`
- `ppx_hash`
- `ppx_variants_conv`
- `ppx_fields_conv`
- `ppx_compare`
- `ppx_driver`

So, a complete setup to compile `r_type` looks like

```bash
> opam update && opam upgrade
  # Switch to 4.05.0...
> opam switch 4.05.0
> eval `opam config env`
  # Install relevant packages...
> opam install omake.0.10.2 menhir core re2 ppx_deriving.4.2.1 ppx_sexp_conv ppx_hash ppx_variants_conv ppx_fields_conv ppx_compare ppx_driver
```

Then, simply run `omake` at the root of this repository. The binary will be located at `src/r_type.opt` (`src/r_type` is a soft link to it).

This should work, but the most up to date build workflow is always the [travis build script](https://github.com/hopv/r_type/blob/master/.travis.sh).

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

[hoice]: https://github.com/hopv/hoice (hoice repository on github)
[z3]: https://github.com/Z3Prover/z3 (z3 repository on github)
[opam]: https://opam.ocaml.org/doc/Install.html (opam official page)