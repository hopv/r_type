# fp_mc

A model-checker for caml programs.

`fp_mc` translates caml programs to Horn clauses and feeds them to a Horn clause solver, such as [hoice][hoice] for instance.

It supports a subset of caml including higher-order functions, nested recursive calls, integers and booleans. Floats and ADTs are currently not supported.

# Build

Make sure you have [opam][opam] installed, ideally switched to the latest stable ocaml compiler (`4.05.0` at the time of writing). `fp_mc` requires the following opam libraries:

- `omake` (build only)
- `core`
- `re2`
- `ppx_deriving`
- `ppx_sexp_conv`
- `ppx_hash`
- `ppx_variants_conv`
- `ppx_fields_conv`
- `ppx_compare`
- `ppx_driver`

You can install them by running

```
opam install omake core re2 ppx_deriving ppx_sexp_conv ppx_hash ppx_variants_conv ppx_fields_conv ppx_compare ppx_driver
```

Then, simply run `omake` at the root of this repository. The binary will be located at `src/fp_mc.opt`.

# Running

```
Usage: fp_mc [options]* <caml_file> [-- <solver arguments>*]
  the arguments passed after the '--' are passed to the underlying horn clause solver
Options:
   -v                                       verbose output
  --effect_analysis    [on|true|off|false]  (de)activates effect analysis
    default 'on'
  --infer              [on|true|off|false]  (de)activates inference (prints the clauses on stdout if off)
    default 'on'
  --solver             <cmd>                command running the horn clause solver
    default 'hoice'
```

You will need a Horn clause solver for `fp_mc` to do anything, such as [hoice][hoice] or [z3][z3]. The default is hoice using the command `hoice`.

If you want to use z3, or hoice but with a different command, pass the name of the command using `--solver <cmd>` when calling `fp_mc`.

**NB**: by default, z3 does *not* read from `stdin`, which `fp_mc` requires. Make sure you pass z3 the `-in` flag:

```bash
> fpmc --solver z3 path_to_my_file.ml -- -in
# Passed to z3 when spawning it ~~~~~~~~~^^^
```

If you only want to inspect the Horn clauses encoding the correctness of your caml program, run `fp_mc` with `--infer off`. The clauses will be printed on `stdout`.

[hoice]: https://github.com/hopv/hoice (hoice repository on github)
[z3]: https://github.com/Z3Prover/z3 (z3 repository on github)
[opam]: https://opam.ocaml.org/doc/Install.html (opam official page)