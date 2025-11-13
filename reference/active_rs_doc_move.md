# Move the active document to another directory

Wrapper around
[`rename_files2()`](https://olivroy.github.io/reuseme/reference/rename_files2.md),
but shortcut to allow renaming the active file.

## Usage

``` r
active_rs_doc_move(new = NULL, old = NULL, ...)
```

## Arguments

- new:

  A new directory

- old:

  The old file (defaults to the active RStudio document.)

- ...:

  Arguments passed on to
  [`rename_files2`](https://olivroy.github.io/reuseme/reference/rename_files2.md)

  `overwrite`

  :   whether to overwrite `new` if it already exists. Be careful.

  `force`

  :   **\[deprecated\]** Use `warn_conflicts` instead of `force = TRUE`

  `action`

  :   One of `"rename"` or `"test"`

  `warn_conflicts`

  :   One of

      - `"default"`: will be check more thoroughly depending on the
        situation. If only moving directory, and `"all"` otherwise.

      - `"all"` (larger scope: if
        `old = "data/my-streets.csv|my_streets"` will check for objects
        named `my_streets`, other files like `my-streets.R`, etc.),

      - `"exact"` will only search for `"data/my-streets.csv"` in
        documents `"none"` will not search for references in documents
        and will rename.

## Value

`new` if renaming succeeded. Mostly called for its side-effects
