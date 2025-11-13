# Rename an output or a data file and watch for references

**\[experimental\]**

This function can improve your workflow. It is inspired by
[`usethis::rename_files()`](https://usethis.r-lib.org/reference/rename_files.html),
but its scope is more oriented towards analysis script.

## Usage

``` r
rename_files2(
  old,
  new,
  warn_conflicts = c("default", "all", "exact", "none"),
  overwrite = FALSE,
  action = c("rename", "test"),
  force = deprecated()
)
```

## Arguments

- old, new:

  Old and new file names (with or without `.R` extensions).

- warn_conflicts:

  One of

  - `"default"`: will be check more thoroughly depending on the
    situation. If only moving directory, and `"all"` otherwise.

  - `"all"` (larger scope: if `old = "data/my-streets.csv|my_streets"`
    will check for objects named `my_streets`, other files like
    `my-streets.R`, etc.),

  - `"exact"` will only search for `"data/my-streets.csv"` in documents
    `"none"` will not search for references in documents and will
    rename.

- overwrite:

  whether to overwrite `new` if it already exists. Be careful.

- action:

  One of `"rename"` or `"test"`

- force:

  **\[deprecated\]** Use `warn_conflicts` instead of `force = TRUE`

## Value

`new` if renaming succeeded. Mostly called for its side-effects

## Use case

Let's say you have an analysis and work on a certain subject. You want
to rename a figure for clarity. For example, you had an input file named
`data/my-streets.csv` and you now want to rename it to

Here is what `rename_files2()` does for you, before it renames files.

1.  Look for potential name conflict

2.  Look for data frame name conflicts

3.  Sends information to clipboard

Will work well for you if you tend to name your objects using snake case
and naming objects with snake case or kebab-case.

The philosophy is to inform you of manual steps required before actually
performing file renaming.

A way to be less strict is to us
