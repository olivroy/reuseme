# Check if files referenced in source files exist in a current dir

1.  It goes through the source files (.R/.qmd etc.),

2.  It identifies data files (.csv, .xlsx) read or written

3.  Search on the system if these files exist.

## Usage

``` r
check_referenced_files(path = ".", quiet = FALSE)
```

## Arguments

- path:

  a directory to search for

- quiet:

  Whether it should print messages?

## Value

A logical

## Details

Still WIP, so you can add code for false positive as needed.

To find genuine referenced files, we exclude different paths

1.  Those created with
    [`fs::path()`](https://fs.r-lib.org/reference/path.html) or
    [`file.path()`](https://rdrr.io/r/base/file.path.html) or
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)

2.  Those that are checked for
    [`fs::file_exists()`](https://fs.r-lib.org/reference/file_access.html),
    [`file.exists()`](https://rdrr.io/r/base/files.html)

3.  Deleted with
    [`fs::file_delete()`](https://fs.r-lib.org/reference/delete.html),
    [`unlink()`](https://rdrr.io/r/base/unlink.html)
