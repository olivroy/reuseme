# Add a TODO list by project to a TODO.R file in the base directory

Creates or edits a `TODO.R` file to store your TODOs. By default it will
write in the current RStudio project.

## Usage

``` r
use_todo(todo, proj = proj_get2(), code = FALSE)
```

## Arguments

- todo:

  A character vector of lines to add to the TODO file. See details for
  special handling.

- proj:

  By default, the active project, an arbitrary directory, or a RStudio
  project name in the following directories `options(reuseme.destdir)`,
  uses
  [`proj_list()`](https://olivroy.github.io/reuseme/reference/proj_path.md)
  in this case. If a file, will write there.

- code:

  If `TRUE`, will render code output (default is text).

## Value

A `TODO.R` file appended with the `todo` string.

## Details

If you use `use_todo()` with a version-control repository, you may want
to use `usethis::use_git_ignore("TODO.R")` if you don't want your
`TODO.R` file

to be included in git. If using in a package directory, use
`usethis::use_build_ignore("TODO.R")` to prevent a note in
`R CMD CHECK`.

If you want to write to a global TODO, use

`options(reuseme.global_todo = fs::path("Documents"))` to write there.

## See also

[`usethis::write_union()`](https://usethis.r-lib.org/reference/write-this.html)

## Examples

``` r
if (FALSE) {
  use_todo("I need to do that")
  use_todo(c("I need to do that again", "youppi"))
  use_todo("c(x, y)", code = TRUE)
  use_todo("Here", proj = "my-analysis")
  use_todo(c("my-analysis::Here", "I am"))
  # add to a global todo.
  use_todo(c("all::Here", "I am"))
}
```
