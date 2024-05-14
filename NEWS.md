# reuseme (development version)

* `file_outline()` works better with news files and headings at the end of files.

* `file_outline()` gives a better error for empty paths.

* `dir_outline()` no longer excludes files by default.

* `browse_pkg()` no longer opens by default and also accepts `<org>/<repo>` shortcode to open GitHub repo.

* `proj_list()` now takes care of matching project to project list.

# reuseme 0.0.1

* in `file/proj/dir_outline()` `regex_outline` is now `pattern`

* `proj_outline()` and `dir_outline()` now excludes example files

* `file_outline()` better support for todo in md files

* `file_outline()` should recognize and transform markdown links automatically with new `markup_href()` It is no longer needed to use `{.href}` in your outline headings to show a link.

* `link_issue()` has been renamed `link_gh_issue()` and now only takes care of changing gh issues in markdown links.
New `markup_href()` is more general and now in charge of creating cli links for all markdown URLs.

```r
str <- "rstudio/gt#120 and [md link](https://github.com)"
# before

link_issue(str)
#> "{.href [rstudio/gt#120](https://github.com/rstudio/gt/issues/120)} and [md link](https://github.com)"

# now
link_gh_issue(str) |> markup_href()
#> "{.href [rstudio/gt#120](https://github.com/rstudio/gt/issues/120)} and {.href [md link](https://github.com)}"
```

* `rename_files2()` should work better with Quarto books, avoiding to look in `_book` and `_execute`, and classifying `summary.qmd` as generic file name, hence not looking for this regexp when searching for conflicts.

* `file_outline()` now get function calls, but doesn't print them by default.

* `file_outline()` truncates todo items to fit on a single line.

* can now personalize recent file indicator with `options(reuseme.recent_indicator)`, by default, a clock.

* Simplified internal function from `mark_todo_as_complete()` to `complete_todo()`

* `file_outline()` now prints the first outline element on the first line (to encourage to give an informative description as first line) (todos or tests excluded)

* Improved support for escaping inline markup.

* Add a print method to `file_outline()`.

To investigate what's gone wrong, use 

```r
f <- proj_outline()
f |> as_tibble()
```

* Add `file_outline()`

* `solve_file_name_conflicts()` now returns the number of conflicts instead of `TRUE`, `FALSE`

* `rename_files2()` was rewritten. Now uses `warn_conflicts` to determine what to do. `force` is now deprecated. Is now separated in smaller functions that are easier to test and to extend.

* `complete_todo()` will save opened documents before attempting to mark as complete.

* `use_todo()` should work on all platforms now.

* `check_files_exist_in_dir()` -> `check_referenced_files()`

* Require R 4.1 and use base pipe

* `rename_files2()` is an experimental function to rename data files, file names.

* `browse_pkg()` is a new function to access the pkgdown site directly. It also prints
  links to vignettes and different parts of the website to the console.

* `slice_min_max()` has new defaults (`each = TRUE`)and argument positions (tidy design principles) (optional arguments after `...`). Gains an ascending parameter to display max before min.

* `use_todo()` allows to write to no project with use_todo("all::")

* `use_todo()` provides the handy <proj>::<todo items> as a shortcut for `use_todo("todo items", "proj") to write TODO items to other projects.

* `use_todo()` no longer fails when not in a RStudio project

* `complete_todo()` can be used multiple times sequentially and will only throw a warning (for now).

* `screenshot()` works with generic file paths that contain numbers and returns the correct message.

* `screenshot()` had internal cleanup.

* `screenshot()` has better support for Quarto blogs, and gains a `proj` argument.

  - The image will be saved in the active `posts/` folder, queried using `rstudioapi::documentPath()`.

* `summarise_with_total()` returns more factors.

* New `na_if2()` as an alternative to `dplyr::na_if()` where it transforms `x` to `NA` if 1. `x` is in certain values, or 2. if a certain logical condition is `TRUE`.
