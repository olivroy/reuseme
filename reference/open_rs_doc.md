# Open a Document in RStudio

Wrapper around
[`rstudioapi::documentOpen()`](https://rstudio.github.io/rstudioapi/reference/rstudio-documents.html),
but with `fs paths`, for consistency. If the file could not be opened, a
clickable hyperlink is displayed.

## Usage

``` r
open_rs_doc(path, line = -1L, col = -1L, move_cursor = TRUE)

active_rs_doc()

active_rs_dir()
```

## Arguments

- path:

  The path to the document.

- line:

  The line in the document to navigate to.

- col:

  The column in the document to navigate to.

- move_cursor:

  Boolean; move the cursor to the requested location after opening the
  document? s

## Value

Invisibly returns the document ids

## Details

- `active_rs_doc()` is a wrapper around
  [`rstudioapi::documentPath()`](https://rstudio.github.io/rstudioapi/reference/rstudio-documents.html)
  that handles unsaved files gracefully

- `active_rs_dir()` is a shortcut for `fs::path_dir(active_rs_doc()` and
  returns the project path if error.

## Examples

``` r
if (FALSE) {
  # open the fictious file.R at line 5
  open_rs_doc("file.R", line = 5)
}
```
