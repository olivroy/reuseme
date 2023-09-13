# reuseme (development version)

* `use_todo()` allows to write to no project with use_todo("all::")

* `screenshot()` works with generic file paths that contain numbers and returns the correct message.

* `screenshot()` had internal cleanup.

* `use_todo()` provides the handy <proj>::<todo items> as a shortcut for `use_todo("todo items", "proj") to write TODO items to other projects.

* `screenshot()` has better support for Quarto blogs, and gains a `proj` argument.

  - The image will be saved in the active `posts/` folder, queried using `rstudioapi::documentPath()`.

* `use_todo()` no longer fails when not in a RStudio project
