# Copy the active document to the same location

The goal is to provide things that RStudio or usethis doesn't provide
natively.

## Usage

``` r
active_rs_doc_copy(new = NULL, ..., old = NULL)
```

## Arguments

- new:

  The new file name, that will be copied in the same directory as the
  [active
  document](https://olivroy.github.io/reuseme/reference/open_rs_doc.md)
  For
  [`active_rs_doc_move()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_move.md),
  a directory.

- ...:

  These dots are for future extensions and must be empty.

- old:

  The old name, defaults to the active document.

## Value

The new file name

## Details

For example, `active_rs_doc_rename()` will not happen, because it is
already easy to do so via the RStudio IDE.

## See also

[`rename_files2()`](https://olivroy.github.io/reuseme/reference/rename_files2.md)

Other document manipulation helpers:
[`active_rs_doc_delete()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_delete.md)
