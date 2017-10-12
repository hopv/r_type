# fp_mc

A model-checker for caml programs.

`fp_mc` translates caml programs to horn clauses and feeds them to a horn
clause solver, such as [hoice][hoice] for instance.

It supports a subset of caml including higher-order functions, nested recursive
calls, integers and booleans. Floats and ADTs are currently not supported.

[hoice]: https://github.com/hopv/hoice (hoice repository on github)