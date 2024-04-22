# reuseme (development version)

* Add `file_outline()`

* `solve_file_name_conflicts()` now returns the number of conflicts instead of `TRUE`, `FALSE`

* `rename_files2()` was rewritten. Now uses `warn_conflicts` to determine what to do. `force` is now deprecated. Is now separated in smaller functions that are easier to test and to extend.

* `mark_todo_as_complete()` will save opened documents before attempting to mark as complete.

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

* `mark_todo_as_complete()` can be used multiple times sequentially and will only throw a warning (for now).

* `screenshot()` works with generic file paths that contain numbers and returns the correct message.

* `screenshot()` had internal cleanup.

* `screenshot()` has better support for Quarto blogs, and gains a `proj` argument.

  - The image will be saved in the active `posts/` folder, queried using `rstudioapi::documentPath()`.

* `summarise_with_total()` returns more factors.

* New `na_if2()` as an alternative to `dplyr::na_if()` where it transforms `x` to `NA` if 1. `x` is in certain values, or 2. if a certain logical condition is `TRUE`.
