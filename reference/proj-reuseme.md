# Interact with different RStudio projects

The package offers many ways to interact with different local RStudio
projects.

## Setup

To take advantage of this functionality, you first have to set
`options(reuseme.reposdir)` in your .Rprofile file. Access it with
[`usethis::edit_r_profile()`](https://usethis.r-lib.org/reference/edit.html).

I would recommend you add the following. It works better if you store
your RStudio projects in common directories.

Inspired by [usethis
options](https://usethis.r-lib.org/reference/usethis_options.html)

    if (interactive()) {
      options(reuseme.reposdir = c("~/rrr/", "~/packages", "~/rrr-work/"))

    }

## Capabilities.

Assumes that you have a project named `"learning"` A project outline

    proj_outline("learning)

Add a TODO item to the `learning` project

    use_todo("learning::Learn this")

Get file
[outline](https://olivroy.github.io/reuseme/reference/outline.md) of the
`file.R` in "learning"

    proj_file("file", "learning")

Move to a new project in the same session

    proj_switch("learning")

A lot of these features are already present in RStudio and with usethis.
However, when managing many projects, the recent projects list can be
more difficult to handle. Passing the full project name to
[`usethis::proj_activate()`](https://usethis.r-lib.org/reference/proj_activate.html)
was too long.

## See also

Other project management helpers:
[`proj_file()`](https://olivroy.github.io/reuseme/reference/proj_file.md),
[`proj_path()`](https://olivroy.github.io/reuseme/reference/proj_path.md),
[`proj_switch()`](https://olivroy.github.io/reuseme/reference/proj_switch.md)
