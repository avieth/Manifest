Manifest
========

Treat your Haskell datatypes as keys and values in some (possibly non-volatile)
data store. The players:

- `mget` tries to retrieve a value determined by some key.
- `mset` tries to set a value (key is implicit in the value).
- `mdel` tries to unset a value determined by some key.
- `manifest` runs a term, probably composed of `mset`, `mget`, and `mdel` terms.

Unfortunately, we must annotate `mget`, `mset`, `mdel` terms with the Manifest
type in which they are going to be used, else GHC complains of ambiguity.

See `examples/` for some examples.
