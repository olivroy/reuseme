# Access the file outline within other project

It can be used as
[`file_outline()`](https://olivroy.github.io/reuseme/reference/outline.md) +
`proj`.

## Usage

``` r
proj_file(file = NULL, path = active_rs_proj(), pattern = NULL)
```

## Arguments

- file:

  A filename or regexp to a file inside `proj`

- path:

  a project path
  [`proj_path()`](https://olivroy.github.io/reuseme/reference/proj_path.md).
  If `NULL`, will return active project.

- pattern:

  A regular expression to look for

## Value

The file outline if multiple matches are found

## See also

Other project management helpers:
[`proj-reuseme`](https://olivroy.github.io/reuseme/reference/proj-reuseme.md),
[`proj_path()`](https://olivroy.github.io/reuseme/reference/proj_path.md),
[`proj_switch()`](https://olivroy.github.io/reuseme/reference/proj_switch.md)

## Examples

``` r
try(proj_file("A non-existent file"))
#> Warning: is.na() applied to non-(list or vector) of type 'closure'
#> Error in enc2utf8(path) : argument is not a character vector
```
