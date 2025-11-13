# Read and clean names and set variable labels

Reads data from file, but clean names with
[`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).

## Usage

``` r
read_clean(file, ...)
```

## Arguments

- file:

  Either a path to a file, a connection, or literal data (either a
  single string or a raw vector).

  Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will be automatically
  uncompressed. Files starting with `http://`, `https://`, `ftp://`, or
  `ftps://` will be automatically downloaded. Remote gz files can also
  be automatically downloaded and decompressed.

  Literal data is most useful for examples and tests. To be recognised
  as literal data, the input must be either wrapped with
  [`I()`](https://rdrr.io/r/base/AsIs.html), be a string containing at
  least one new line, or be a vector containing at least one string with
  a new line.

  Using a value of
  [`clipboard()`](https://readr.tidyverse.org/reference/clipboard.html)
  will read from the system clipboard.

- ...:

  Arguments passed on to
  [`readr::read_csv`](https://readr.tidyverse.org/reference/read_delim.html),
  [`readxl::read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)

  `quote`

  :   Single character used to quote strings.

  `col_names`

  :   Either `TRUE`, `FALSE` or a character vector of column names.

      If `TRUE`, the first row of the input will be used as the column
      names, and will not be included in the data frame. If `FALSE`,
      column names will be generated automatically: X1, X2, X3 etc.

      If `col_names` is a character vector, the values will be used as
      the names of the columns, and the first row of the input will be
      read into the first row of the output data frame.

      Missing (`NA`) column names will generate a warning, and be filled
      in with dummy names `...1`, `...2` etc. Duplicate column names
      will generate a warning and be made unique, see `name_repair` to
      control how this is done.

  `col_types`

  :   One of `NULL`, a
      [`cols()`](https://readr.tidyverse.org/reference/cols.html)
      specification, or a string. See `vignette("readr")` for more
      details.

      If `NULL`, all column types will be inferred from `guess_max` rows
      of the input, interspersed throughout the file. This is convenient
      (and fast), but not robust. If the guessed types are wrong, you'll
      need to increase `guess_max` or supply the correct types yourself.

      Column specifications created by
      [`list()`](https://rdrr.io/r/base/list.html) or
      [`cols()`](https://readr.tidyverse.org/reference/cols.html) must
      contain one column specification for each column. If you only want
      to read a subset of the columns, use
      [`cols_only()`](https://readr.tidyverse.org/reference/cols.html).

      Alternatively, you can use a compact string representation where
      each character represents one column:

      - c = character

      - i = integer

      - n = number

      - d = double

      - l = logical

      - f = factor

      - D = date

      - T = date time

      - t = time

      - ? = guess

      - \_ or - = skip

      By default, reading a file without a column specification will
      print a message showing what `readr` guessed they were. To remove
      this message, set `show_col_types = FALSE` or set
      `options(readr.show_col_types = FALSE)`.

  `col_select`

  :   Columns to include in the results. You can use the same
      mini-language as
      [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
      to refer to the columns by name. Use
      [`c()`](https://rdrr.io/r/base/c.html) to use more than one
      selection expression. Although this usage is less common,
      `col_select` also accepts a numeric column index. See
      [`?tidyselect::language`](https://tidyselect.r-lib.org/reference/language.html)
      for full details on the selection language.

  `id`

  :   The name of a column in which to store the file path. This is
      useful when reading multiple input files and there is data in the
      file paths, such as the data collection date. If `NULL` (the
      default) no extra column is created.

  `locale`

  :   The locale controls defaults that vary from place to place. The
      default locale is US-centric (like R), but you can use
      [`locale()`](https://readr.tidyverse.org/reference/locale.html) to
      create your own locale that controls things like the default time
      zone, encoding, decimal mark, big mark, and day/month names.

  `na`

  :   Character vector of strings to interpret as missing values. Set
      this option to
      [`character()`](https://rdrr.io/r/base/character.html) to indicate
      no missing values.

  `quoted_na`

  :   **\[deprecated\]** Should missing values inside quotes be treated
      as missing values (the default) or strings. This parameter is soft
      deprecated as of readr 2.0.0.

  `comment`

  :   A string used to identify comments. Any text after the comment
      characters will be silently ignored.

  `trim_ws`

  :   Should leading and trailing whitespace (ASCII spaces and tabs) be
      trimmed from each field before parsing it?

  `skip`

  :   Number of lines to skip before reading data. If `comment` is
      supplied any commented lines are ignored *after* skipping.

  `n_max`

  :   Maximum number of lines to read.

  `guess_max`

  :   Maximum number of lines to use for guessing column types. Will
      never use more than the number of lines read. See
      [`vignette("column-types", package = "readr")`](https://readr.tidyverse.org/articles/column-types.html)
      for more details.

  `name_repair`

  :   Handling of column names. The default behaviour is to ensure
      column names are `"unique"`. Various repair strategies are
      supported:

      - `"minimal"`: No name repair or checks, beyond basic existence of
        names.

      - `"unique"` (default value): Make sure names are unique and not
        empty.

      - `"check_unique"`: No name repair, but check they are `unique`.

      - `"unique_quiet"`: Repair with the `unique` strategy, quietly.

      - `"universal"`: Make the names `unique` and syntactic.

      - `"universal_quiet"`: Repair with the `universal` strategy,
        quietly.

      - A function: Apply custom name repair (e.g.,
        `name_repair = make.names` for names in the style of base R).

      - A purrr-style anonymous function, see
        [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

      This argument is passed on as `repair` to
      [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html).
      See there for more details on these terms and the strategies used
      to enforce them.

  `num_threads`

  :   The number of processing threads to use for initial parsing and
      lazy reading of data. If your data contains newlines within fields
      the parser should automatically detect this and fall back to using
      one thread only. However if you know your file has newlines within
      quoted fields it is safest to set `num_threads = 1` explicitly.

  `progress`

  :   Display a progress bar? By default it will only display in an
      interactive session and not while knitting a document. The
      automatic progress bar can be disabled by setting option
      `readr.show_progress` to `FALSE`.

  `show_col_types`

  :   If `FALSE`, do not show the guessed column types. If `TRUE` always
      show the column types, even if they are supplied. If `NULL` (the
      default) only show the column types if they are not explicitly
      supplied by the `col_types` argument.

  `skip_empty_rows`

  :   Should blank rows be ignored altogether? i.e. If this option is
      `TRUE` then blank rows will not be represented at all. If it is
      `FALSE` then they will be represented by `NA` values in all the
      columns.

  `lazy`

  :   Read values lazily? By default, this is `FALSE`, because there are
      special considerations when reading a file lazily that have
      tripped up some users. Specifically, things get tricky when
      reading and then writing back into the same file. But, in general,
      lazy reading (`lazy = TRUE`) has many benefits, especially for
      interactive use and when your downstream work only involves a
      subset of the rows or columns.

      Learn more in
      [`should_read_lazy()`](https://readr.tidyverse.org/reference/should_read_lazy.html)
      and in the documentation for the `altrep` argument of
      [`vroom::vroom()`](https://vroom.r-lib.org/reference/vroom.html).

  `path`

  :   Path to the xls/xlsx file.

  `sheet`

  :   Sheet to read. Either a string (the name of a sheet), or an
      integer (the position of the sheet). Ignored if the sheet is
      specified via `range`. If neither argument specifies the sheet,
      defaults to the first sheet.

  `range`

  :   A cell range to read from, as described in
      [cell-specification](https://readxl.tidyverse.org/reference/cell-specification.html).
      Includes typical Excel ranges like "B3:D87", possibly including
      the sheet name like "Budget!B2:G14", and more. Interpreted
      strictly, even if the range forces the inclusion of leading or
      trailing empty rows or columns. Takes precedence over `skip`,
      `n_max` and `sheet`.

  `.name_repair`

  :   Handling of column names. Passed along to
      [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
      readxl's default is \`.name_repair = "unique", which ensures
      column names are not empty and are unique.

## Details

It keeps the variable labels attributes to have improved support in the
RStudio IDE, with gt tables and with ggplots (\>= 3.6.0).

## Examples

``` r
read_clean(system.file("extdata",  "challenge.csv", package = "readr"))
#> Rows: 2000 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl  (1): x
#> date (1): y
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 2,000 × 2
#>        x y     
#>    <dbl> <date>
#>  1   404 NA    
#>  2  4172 NA    
#>  3  3004 NA    
#>  4   787 NA    
#>  5    37 NA    
#>  6  2332 NA    
#>  7  2489 NA    
#>  8  1449 NA    
#>  9  3665 NA    
#> 10  3863 NA    
#> # ℹ 1,990 more rows
```
