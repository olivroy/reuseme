# Remove a TODO/WORK/FIXME item from a file

Function meant to be wrapped as `{.run }` hyperlinks with
[`file_outline()`](https://olivroy.github.io/reuseme/reference/outline.md).
It basically removes a line from a file.

## Usage

``` r
complete_todo(line, file, regexp, rm_line = NULL)
```

## Arguments

- line:

  The line number (a single integer)

- file:

  Path to a file

- regexp:

  A regexp to assess that the file content has not changed.

- rm_line:

  A logical If `NULL` will remove the full line in the file (for TODO,
  or FIXME items), else for WORK, will only remove the WORK tag will
  remove only the tag (i.e. TODO, WORK, FIXME)

## Value

Writes a file with corrections, and returns the new line content
invisibly.
