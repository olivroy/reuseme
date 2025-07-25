# reuseme (development version)

* `file_outline()` ignores todo items in import-standalone files.
* `dir_outline()` now defaults to `active_rs_dir()`.
* New `proj_path(proj, "dir")` as a shortcut to `fs::path(proj_list(proj), "dir")`
* New `write_temp_excel()` to help with writing a quick nicely formatted Excel file to Downloads folder.

# reuseme 0.1.0

## Breaking change

* `na_if2()` gains `...` to force you to specify `values` or `expr` explicitly.

* To facilitate piping, `path` / `proj` is now the first argument of `proj_outline()`, `file_outline()`, and `dir_outline()`.

* If you previously relied on specifying pattern without naming it, you will have 
 to get used to the opposite. Naming `pattern`, but not path.

* Removed `work_only` argument from `file_outline()`, use `pattern = "WORK"` to 
  achieve the same result.
  
* In `proj_outline()`, `proj` has been renamed `path`, but still accepts a project name,
that will passed on to `proj_list()`

* Removed `dir_common` from `file_outline()` (#35)

* `file_outline()` result is now a simpler data frame. The cli links are now created in the print method. (which makes more sense for truncation)

* More safety around rstudioapi. (make `open_rs_doc()` work in Positron.)

* `use_todo("global::todo")` no longer works out of the box. You need to set `options(reuseme.global_todo = fs::path("Documents"))` explicitly (in .Rprofile) for example to make sure 
  reuseme can write in a directory.
  
* Added `file_move_temp_auto()` to automatically move a Downloads file to your project directory.

* Added `active_rs_doc_move('dir')` as a shortcut for `rename_files2(fs::path('dir', fs::path_file(active_rs_doc()), active_rs_doc())`.

* Added `file_rename_auto()` as a shortcut for `file.rename(fs::path(fs::path_dir(old_file), new_file, ext = fs::path_ext(old_file)), old_file)`

* Added `file_move_dir_auto()`  as a shortcut for `file.rename(fs::path(new_dir, fs::path_file(old_file)), old_file)`

* Added `file_copy_auto()` as a shortcut for `file.copy(fs::path(new_dir), fs::path_file(old_file), old_file)`


## Fixes

* `rename_files2()` now looks in `.Rbuildignore` to see if some files should be replaced.
* Improved regex in `link_gh_issue()`.

* `file_outline()` now recognize `describe()` test calls. 
  (probably many false positive for now) (#31)
  
* Outline elements present more than four times in a file will not be printed as they are considered placeholders. (like generic test name)

* `proj_outline()` now detects [knitr notebooks](https://rmarkdown.rstudio.com/articles_report_from_r_script.html) that use the default options. Internally, the file is transformed into a md file by stripping roxygen comments, and is processed as such. (#30)

* `proj_outline()` no longer shows `complete_todo()` links for items in non-interactive sessions. `complete_todo()` links are now only shown when calling `file_outline()` on the active file.

* `proj_list()` / `proj_switch()` no longer opens a nested project if looking for `"pkgdown"`, `"testthat"`, etc.

* `active_rs_doc_nav()` is a new function to navigate to files pane location.

* `active_rs_doc_copy()` now accepts copying md and qmd files too and no longer allows renaming Rprofile.

* `proj_file()` is better.

* `file_outline()` has more support for deeper file sections.

* Local GitHub issues show better in outline.

* `file_outline()` detects better plot titles and section titles.

* Package versions in NEWS.md are now normalized to yield better results.

* `active_rs_doc()` returns the relative path if in RStudio project.

* `summarise_with_total()` works with more than 1 group to get the total summary.

* `check_referenced_files()` now checks `_quarto.yml`.

* `check_referenced_files()` now has less false positives.

# reuseme 0.0.2

* `complete_todo()` no longer deletes the full line. It only deletes what it says it deletes (#27).

* `file_outline()` works better with news files and headings at the end of files.

* `file_outline()` gives a better error for empty paths.

* `dir_outline()` no longer excludes files by default.

* `browse_pkg()` no longer opens by default and also accepts `<org>/<repo>` shortcode to open GitHub repo.

* `proj_list()` now takes care of matching project to project list.

* `link_gh_issue()` and `markup_href()` are better.

* `file_outline()` now simplifies the outline of NEWS.md. Only major versions are listed now.

* `proj_file()` is less noisy and is a shortcut for `file_outline()` with `proj`.

* Indentation in `file_outline()` shortly possible.

* `proj_switch()` is now robust to duplicate project name.

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
