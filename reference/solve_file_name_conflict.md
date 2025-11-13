# Check if outdated or non-existent file is.

If `quiet = FALSE` (default) will give a hint of where the file is
referenced.

## Usage

``` r
solve_file_name_conflict(
  files,
  regex,
  dir = ".",
  extra_msg = NULL,
  quiet = FALSE,
  what = NULL,
  new_file = NULL
)
```

## Arguments

- files:

  which files to search in

- regex:

  a regex related to the file name to search for

- dir:

  A directory where to operate

- extra_msg:

  Extra message to pass

- quiet:

  A logical, informs where the occurrences are found. (Default, `FALSE`)

- what:

  Which file conflicts we talking about

- new_file:

  (Optional) new file name

## Value

Mostly called for its side-effects, but will return the number of
matches (0 if no referenced files are problematic)
