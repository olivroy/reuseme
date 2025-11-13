# Print interactive outline of file sections

The outline functions return a data frame that contains details of file
location.

It also includes a print method that will provide a console output that
will include [clickable
hyperlinks](https://cli.r-lib.org/reference/links.html) in RStudio (or
if your terminal supports it). It works with both (qR)md and R files.

Outline elements include

- Any code section

- function definition (not shown in console by default)

- `TODO` items

- Parse cli hyperlinks

- Plot or table titles

- Figures caption in Quarto documents (limited support for multiline
  caption currently)

- Test names

- Indicator of recent modification

- Colored output for

- URL and gh issue detection and link creation.

By default

- `file_outline()` prints the outline the [active
  document](https://olivroy.github.io/reuseme/reference/open_rs_doc.md)
  if in RStudio

- `proj_outline()` prints the outline of the [active
  project](https://usethis.r-lib.org/reference/proj_utils.html) if in
  RStudio

- `dir_outline()` prints the outline of the [active working
  directory](https://rdrr.io/r/base/getwd.html) by default or

## Usage

``` r
file_outline(
  path = active_rs_doc(),
  pattern = NULL,
  alpha = FALSE,
  print_todo = TRUE,
  recent_only = FALSE
)

proj_outline(
  path = active_rs_proj(),
  pattern = NULL,
  dir_tree = FALSE,
  alpha = FALSE,
  recent_only = FALSE
)

dir_outline(
  path = active_rs_dir(),
  pattern = NULL,
  dir_tree = FALSE,
  alpha = FALSE,
  recent_only = FALSE,
  recurse = FALSE
)
```

## Arguments

- path:

  A character vector of file paths, a
  [project](https://olivroy.github.io/reuseme/reference/proj_path.md).
  Defaults to the [active
  file](https://olivroy.github.io/reuseme/reference/open_rs_doc.md),
  project or directory.

- pattern:

  A string or regex to search for in the outline. If specified, will
  search only for elements matching this regular expression. The print
  method will show the document title for context. Previously
  `regex_outline`

- alpha:

  Whether to show in alphabetical order

- print_todo:

  Should include TODOs in the file outline? If `FALSE`, will print a
  less verbose output with sections.

- recent_only:

  Show outline for recent files

- dir_tree:

  If `TRUE`, will print the
  [`fs::dir_tree()`](https://fs.r-lib.org/reference/dir_tree.html) or
  non-R files in the directory

- recurse:

  If `TRUE` recurse fully, if a positive number the number of levels to
  recurse.

## Value

A `outline_report` object that contains the information. Inherits
`tbl_df`.

A symbol will show for recently modified files.

## Details

`proj_outline()` and `dir_outline()` are wrapper of `file_outline()`.

In `proj_outline()`, `path` accepts project names, see
[`proj_list()`](https://olivroy.github.io/reuseme/reference/proj_path.md)
for how to set up reuseme to recognize your projects' locations.

The parser is very opinionated and is not very robust as it is based on
regexps. For a better file parser, explore other options, like
[lightparser](https://thinkr-open.github.io/lightparser/) for Quarto,
`{roxygen2}`

Will show TODO items and will offer a link to [mark them as
complete](https://olivroy.github.io/reuseme/reference/complete_todo.md).

Note that `proj_outline()` strips some test files from the outline, as
example test files (like in usethis repo) don't help understand a
project's outline. Use `dir_outline(recurse = TRUE)` to make sure these
are included in your outline.

## Examples

``` r
file <- fs::path_package("reuseme", "example-file", "outline-script.R")
file_outline(file)
#> 
#> ── `example-file/outline-script.R`  Example for `file_outline()` 
#> `i` Load packages
#> `i` Wrangle + visualize data
#> `i` A great title
#> `i` TODO improve this Vizual!

# Remove todo items
file_outline(file, print_todo = FALSE, alpha = TRUE)
#> 
#> ── `example-file/outline-script.R`  Example for `file_outline()` 
#> `i` A great title
#> `i` Load packages
#> `i` Wrangle + visualize data

# interact with data frame
file_outline(file) |> dplyr::as_tibble()
#> # A tibble: 7 × 27
#>   file     content outline_el title_el title_el_line  line has_title_el file_ext
#>   <chr>    <chr>   <chr>      <chr>            <int> <int> <lgl>        <chr>   
#> 1 /home/r… "# Exa… NA         Example…             1     1 TRUE         R       
#> 2 /home/r… "# Loa… Load pack… Example…             1     2 FALSE        R       
#> 3 /home/r… "# Wra… Wrangle +… Example…             1     5 FALSE        R       
#> 4 /home/r… "    t… A great t… Example…             1     9 FALSE        R       
#> 5 /home/r… "# TOD… TODO impr… Example…             1    11 FALSE        R       
#> 6 /home/r… "f_exa… f_example  Example…             1    14 FALSE        R       
#> 7 /home/r… "  f2_… f2_example Example…             1    18 FALSE        R       
#> # ℹ 19 more variables: is_md <lgl>, is_test_file <lgl>, is_snap_file <lgl>,
#> #   is_cli_info <lgl>, is_doc_title <lgl>, is_chunk_cap <lgl>,
#> #   is_chunk_cap_next <lgl>, is_test_name <lgl>, is_todo_fixme <lgl>,
#> #   is_section_title <lgl>, pkg_version <chr>, is_section_title_source <lgl>,
#> #   n_leading_hash <dbl>, is_second_level_heading_or_more <lgl>,
#> #   is_cross_ref <lgl>, is_function_def <lgl>, is_tab_or_plot_title <lgl>,
#> #   is_subtitle <lgl>, recent_only <lgl>

if (FALSE) { # interactive()
# These all work on the active file / project or directory.

file_outline()
proj_outline()
dir_outline(".")
# Like proj_switch(), proj_outline() accepts a project
}
```
