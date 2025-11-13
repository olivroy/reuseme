# Specify `proj` in functions

- `proj_` Two main ways to specify proj:

- Set `options(reuseme.reposdir)` in `.Rprofile` that contains a
  character vector of paths to your projects. (i.e. `~/rrr` that
  contains all your projects)

- Specify the full path to `proj`. (like you would for usethis function)

## Usage

``` r
proj_path(proj = NA, ..., dirs = getOption("reuseme.reposdir"))

proj_list(proj = NA, dirs = getOption("reuseme.reposdir"))
```

## Arguments

- proj:

  A project path or name to match. If `NA`, returns all projects. If
  `NULL`, returns the active project.

- ...:

  character vectors, if any values are NA, the result will also be NA.
  The paths follow the recycling rules used in the tibble package,
  namely that only length 1 arguments are recycled.

- dirs:

  The directories in which we want to list projects.

## Value

A named character vector with the project name as name, and path as
value. If `proj` is supplied

## See also

Other project management helpers:
[`proj-reuseme`](https://olivroy.github.io/reuseme/reference/proj-reuseme.md),
[`proj_file()`](https://olivroy.github.io/reuseme/reference/proj_file.md),
[`proj_switch()`](https://olivroy.github.io/reuseme/reference/proj_switch.md)
