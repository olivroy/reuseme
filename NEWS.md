# reuseme (development version)

* `screenshot()` has better support for Quarto blogs.

  - The image will be saved in the active `posts/` folder, queried using `rstudioapi::documentPath()`.

* `use_todo()` no longer fails when not in a RStudio project
