# Opens a RStudio project in a new session

If not specified, will generate hyperlinks that call
[`usethis::proj_activate()`](https://usethis.r-lib.org/reference/proj_activate.html).
`proj_switch()` looks at `options(reuseme.reposdir)`.

## Usage

``` r
proj_switch(proj = NA, new_session = TRUE)
```

## Arguments

- proj:

  the name of a project located in the default locations or `NA`

- new_session:

  Should open in a new session?

## Value

Single logical value indicating if current session is modified.

## See also

[`usethis::proj_activate()`](https://usethis.r-lib.org/reference/proj_activate.html)

Other project management helpers:
[`proj-reuseme`](https://olivroy.github.io/reuseme/reference/proj-reuseme.md),
[`proj_file()`](https://olivroy.github.io/reuseme/reference/proj_file.md),
[`proj_path()`](https://olivroy.github.io/reuseme/reference/proj_path.md)
