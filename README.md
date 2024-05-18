
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reuseme

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/reuseme)](https://CRAN.R-project.org/package=reuseme)
[![R-CMD-check](https://github.com/olivroy/reuseme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/olivroy/reuseme/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/olivroy/reuseme/branch/main/graph/badge.svg)](https://app.codecov.io/gh/olivroy/reuseme?branch=main)

<!-- badges: end -->

The goal of reuseme is to provide utility functions for project
management across RStudio projects. Sometimes, you have to manage
multiple things at once, but don’t have the time to do edits. You may
need to switch quickly to a project, add things or browse a certain file
if you have some replications across projects. Sometimes, it is hard to
do that. reuseme also aims to help me overcome things I don’t like on
Windows.

## Installation

You can install the development version of reuseme like so:

``` r
# R-universe
install.packages('reuseme', repos = c('https://olivroy.r-universe.dev', 'https://cloud.r-project.org'))

# From GitHub
pak::pak("olivroy/reuseme")
```

## Getting started

reuseme is adapted for a standard workflow, recommended in (find
resources)

- Anyone working in RStudio (recent version for hyperlink support)
- Work with RStudio projects
- Your RStudio projects are organized in a centralized location on your
  computer
- Your RStudio projects are Version controlled with git (optional, but
  recommended to avoid surprises! No need to be hosted on repositories
  like GitLab or GitHub)
- You are working on Windows (macOS is supported, but some things were
  designed on Windows)
- You use machine and human readable paths (i.e. no spaces, special
  characters) (Tip: don’t hesitate to rename your files, it can take
  away the pain in the long run!

To take advantage of reuseme, it is highly recommended to set the
following option in your `.Rprofile`

``` r
options(reuseme.reposdir = c("~/rrr", "any-other-directories-that-contain-rstudio-projects"))
```

This will enable functions like `proj_switch()`, `proj_list()`,
`use_todo()` to be optimized.

## Example

Since the package is meant for interactive use, there may not be a lot
of code. It takes advantage of [cli
hyperlinks](https://cli.r-lib.org/reference/links.html) to improve
productivity.

``` r
library(reuseme)
## basic example code
```

Most of these functions are meant to be used in RStudio, and in RStudio
Projects, as they have not been widely tested outside this context.

In interactive sessions, you can use the `screenshot()` function to
access an image in the clipboard and save it as .png in a `figures/` or
`images/`directory in your RStudio project.

reuseme helps you work across projects, with the `proj_switch()`
function. `proj_switch()` works a lot like `usethis::proj_activate()`
with the advantage of only typing the project name, instead of the full
path. By default, reuseme looks at `options(reuseme.reposdir)`, that is
a vector of paths where repositories are located. Personally, I use
`~/rrr` (for my own projects) and `~/rrr-forks` (for projects I
contribute to).

When not supplying the `proj` argument, many functions will just offer
you to choose, with a user interface built on [cli
hyperlinks](https://cli.r-lib.org/reference/links.html).

### Extend usethis functionality

usethis is fantastic to manage workflow within a project, but is harder
across projects.

If you want to work across projects with [usethis](usethis.r-lib.org),
you need to provide the full path to a project. With reuseme, just use
the project name!

<table style="width:100%;">
<caption>usethis vs reuseme</caption>
<colgroup>
<col style="width: 27%" />
<col style="width: 27%" />
<col style="width: 45%" />
</colgroup>
<thead>
<tr class="header">
<th>Workflow</th>
<th>reuseme</th>
<th>usethis</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Switch to project “cool-project”</td>
<td><code>proj_switch(proj = "cool-project")</code></td>
<td><code>proj_activate(path = "C:/users/long/path/to/cool-project")</code></td>
</tr>
<tr class="even">
<td>Write a TODO item in project “cooler-project”, while working in
“cool-project”</td>
<td><code>use_todo(todo = "I need to do this ASAP as possible", proj = "cooler-project")</code></td>
<td><code>usethis::write_union(path = "C:/Users/I/do/not/want/to/type/cooler-project/TODO.R", lines = "I need to do this ASAP as possible.")</code></td>
</tr>
<tr class="odd">
<td>Open pkgdown site link and see vignettes</td>
<td><ol type="1">
<li>`browse_pkg(“usethis”)</li>
<li>Click on the hyperlinks that correspond to your query</li>
</ol></td>
<td><ol type="1">
<li><code>browse_package("usethis")</code></li>
<li>Type the correct number that corresponds</li>
<li><code>browseVignettes("usethis")</code></li>
<li>Open it</li>
</ol></td>
</tr>
</tbody>
</table>

usethis vs reuseme

## Proposing a data analysis workflow

<!--# Write about dplyr-plus functions! -->
<!--# Write about *_identity functions -->
<!--# Write about _named functions -->

# Outline speed

Due to the growing number of criteria, regex, `file_outline()` is
slowing down a bit. I will address that.

``` r
bench::mark(
  outline <- proj_outline()
)
#> # A tibble: 1 × 6
#>   expression                     min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>                <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 outline <- proj_outline()    1.55s    1.55s     0.647    65.9MB     5.17
```

<details>
<summary>
Example outline
</summary>
<p>

``` r
outline
#> 
#> ── `inst/example-file/outline-script.R`  Example for `file_outline()`
#> `i` Load packages
#> `i` Wrangle + visualize data
#> `i` A great title
#> `i` TODO improve this Viz!- `Done✔?`
#> 
#> ── `R/dplyr-plus.R`  dplyr extra
```

    #> `i` FIXME Doesn't work, problem with symbols here- `Done✔?`
    #> `i` TODO use `check_length()` when implemented. r-lib/rlang#1618 (<https://github.com/r-lib/rlang/issues/1618>)- `Done✔?`
    #> `i` summarise with total
    #> `i` Count observations by group and compute percentage
    #> `i` Subset rows using their positions
    #> `i` Explore all rows in a random group
    #> `i` Keep rows that match one of the conditions
    #> `i` Elegant wrapper around filter and pull
    #> `i` Compute a summary for one group with the total included.
    #> `i` Transform to NA any of the condition
    #> `i` `count_pct()` lets you quickly count the unique values of one or more
    #> variables: `df |> count_pct(a, b)` It calculates the percentage by group
    #> afterwards
    #> `i` A wrapper around `dplyr::bind_rows()`, `dplyr::slice_min()`, `dplyr::slice_max()`
    #> `i` Compared to `slice_sample()` `slice_group_sample` will return all rows corresponding to a group.
    #> `i` The `filter_if_any()` function is used to subset a data frame, retaining all
    #> rows that satisfy **at least one of** your conditions.
    #> To be retained, the row must produce a value of `TRUE` for
    #> **one of the conditions**. Note that when a condition evaluates to `NA` the
    #> row will be dropped, (hence this function) unlike base subsetting with `[`.
    #> `i` It can be very useful when trying to extract a value from somewhere, and you
    #> have one col that represents the unique id.
    #> `i` This function is useful to create end tables, apply the same formula to a group and to its overall.
    #> You can specify a personalized `Total` value with the `.label` argument. You
    #> You should only use the output from `summarise_with_total()` with `tidyr::pivot_wider()`,
    #> write data to a spreadsheet, `gt::gt()` after that. Don't try to do more computing afterwards.
    #> It can also be used for plotting
    #> Changes the `.by` variable to a factor.
    #> `i` This function is similar to `dplyr::na_if()`, but it has 2 differences. the
    #> values of `y` are never recycled. There are two ways to provide the condition.
    #> As values or as a logical vector.
    #> `i` Wrapper function around [dplyr::count()]
    #> `i` The reason to be of this function is to simplify a call like
    #> 
    #> ```r
    #> # with dplyr::filter
    #> dat |> dplyr::filter(vs == 1 | is.na(vs))
    #> data |>
    #>   dplyr::mutate(cond1 = vs == 1, cond2 = is.na(vs)) |>
    #>   dplyr::filter(dplyr::if_any(starts_with("cond")))
    #> dat |> filter_if_any(vs == 1, is.na(vs))
    #> ```
    #> 
    #> Basically, this is just a shortcut to
    #> `mutate(.data, new_lgl_vars)` + `filter(if_any(new_lgl_vars))` + `select(-new_lgl_vars)`.
    #> It allows mutate_like syntax in `filter(if_any(...))`.
    #> 
    #> Caution: still doesn't work with [dplyr::across()], use the regular
    #> `filter(if_any())` syntax.
    #> `i` dplyr extensions
    #> `i` dplyr extensions
    #> `i` family dplyr extensions

    #> 
    #> ── `R/eda-identity.R`  dplyr/base identity helpers --------------------
    #> `i` base identity functions
    #> `i` dplyr identity functions with small tweaks
    #> `i` dplyr identity without tweaks
    #> `i` dplyr extensions identity
    #> `i` helpers
    #> `i` Helpers that return the same value
    #> `i` They all share the `*_identity` suffix, they are silent in non-interactive
    #> sessions. They are very handy to create clickable hyperlinks that do not
    #> modify the current state of the analysis.
    #> 
    #> They are inspired by [pillar::glimpse()], [tibble::view()].
    #> 
    #> Look at the original functions for the other parameters.
    #> 
    #> # Use cases / advantages
    #> 
    #> * Like many other reuseme functions, they are most useful in interactive sessions
    #> * print the result in interactive sessions (quiet in non-interactive.)
    #> * Create runnable hyperlinks (In August 2023, RStudio forbids runnable
    #>   hyperlinks of base functions, or non-package functions. (i.e. that don't have `::`))
    #> * Use in pipelines to explore the data
    #> * Use [rlang::is_interactive()] over [base::interactive()] as it's easier to
    #> control and test with `options(rlang_interactive)`
    #> * Use the original functions for your final results.
    #> * `count_identity()` also prints percentages.
    #> * `slice_identity()` can be useful to resolve many-to-many warnings from
    #>   dplyr join functions.
    #> 
    #> # Caution
    #> 
    #> * Don't put those at the end of a pipeline
    #> * Don't name the first argument, to avoid conflicts in case a column in the
    #>   data is named `x`.
    #> * Some functions have small tweaks
    #>   * `mutate_identity()` only prints the distinct values, and uses
    #>     `.keep = "used"`, `.before = 0`, unless specified to improve the display.
    #>   * `count_identity()` is a wrapper of [count_pct()]
    #>     (itself a wrapper of `dplyr::count()`),
    #>   * `count_identity()` may fail if there is already a variable named `n`.
    #>   * `slice_min/max_identity()` relocates the target column at the beginning.
    #>   * `filter_identity()` prints a short message if no rows are returned.
    #> 
    #> ── `R/files-conflicts.R`  Check if files referenced in source files exist in a current dir
    #> `i` TODO insert in either proj_outline, or rename_file- `Done✔?`
    #> `i` TODO probably needs a `detect_genuine_path()`- `Done✔?`
    #> `i` Helpers
    #> `i` TODO Add false positive references- `Done✔?`
    #> `i` TODO fs::path and file.path should be handled differently- `Done✔?`
    #> `i` Check if outdated or non-existent file is.
    #> `i` 1. It goes through the source files (.R/.qmd etc.),
    #> 2. It identifies data files (.csv, .xlsx) read or written
    #> 3. Search on the system if these files exist.
    #> `i` If `quiet = FALSE` (default) will give a hint of where the file is referenced.
    #> `i` Still WIP, so you can add code for false positive as needed.
    #> 
    #> 
    #> To find genuine referenced files, we exclude different paths
    #> 
    #> 1. Those created with `fs::path()` or `file.path()` or `glue::glue()`
    #> 2. Those that are checked for `fs::file_exists()`, `file.exists()`
    #> 3. Deleted with `fs::file_delete()`, `unlink()`
    #> 
    #> ── `R/import-standalone-types-check.R`
    #> `i` Scalars
    #> `i` Vectors
    #> 
    #> ── `R/open.R`  Open a Document in RStudio
    #> `i` FIXME why is this code like this?- `Done✔?`
    #> `i` TODO structure and summarise information.- `Done✔?`
    #> `i` FIXME (upstream) the color div doesn't go all the way r-lib/cli#694 (<https://github.com/r-lib/cli/issues/694>)- `Done✔?`
    #> `i` Copy the active document to the same location
    #> `i` Delete the active RStudio document safely
    #> `i` Wrapper around [rstudioapi::documentOpen()], but with `fs paths`, for consistency.
    #> If the file could not be opened, a clickable hyperlink is displayed.
    #> `i` The goal is to provide things that RStudio or usethis doesn't provide natively.
    #> `i` `r lifecycle::badge('experimental')`
    #> 
    #> Gathers informative summary about the document you are about to delete.
    #> 
    #> 
    #> Will delete more easily if file name starts with `temp-`, if file is untracked and recent.
    #> `i` * `active_rs_doc()` is a wrapper around [rstudioapi::documentPath()] that handles
    #>   unsaved files gracefully
    #> `i` For example, `active_rs_doc_rename()` will not happen, because it is already easy
    #> to do so via the RStudio IDE.
    #> `i` document manipulation helpers
    #> `i` document manipulation helpers
    #> 
    #> ── `R/outdated-pkgs.R`  Looks for outdated packages
    #> `i` TODO figure out pad :)- `Done✔?`
    #> `i` Only checks for binaries, which has the advantage of not notifying you when
    #> packages are newly available on CRAN, but without the binaries available.
    #> Installing packages from source can be longer, hence the advantage of
    #> waiting a few days for binaries.
    #> 
    #> It takes advantage of pak's capacities to allow you to install packages on
    #> Windows without restarting session.
    #> 
    #> ── `R/outline-criteria.R`
    #> `i` Add variable to outline data frame
    #> `i` TODO strip is_cli_info in Package? only valid for EDA (currently not sh…- `Done✔?`
    #> `i` FIXME try to detect all the chunk caption, but would have to figure out the end of it maybe lightparser.- `Done✔?`
    #> `i` it is 'R/outline.R'
    #> `i` outline_criteria
    #> `i` * is test title
    #> * is a todo item
    #> * is_roxygen_line
    #> * is_tab_title
    #> 
    #> ── `R/outline-roxy.R`  Extract roxygen tag
    #> `i` TODO when stable delete- `Done✔?`
    #> `i` TODO Delete when stable debugging- `Done✔?`
    #> `i` TODO Delete when stable for debugging- `Done✔?`
    #> `i` helper for interactive checking
    #> `i` Tell me what this does
    #> `i` Section to extract
    #> 
    #> Well this is a section
    #> 
    #> ── `R/outline.R`  `proj_outline()`
    #> `i` `file_outline()`
    #> `i` File outline
    #> `i` Print method
    #> `i` Step: tweak outline look as they show
    #> `i` TODO reanable cli info- `Done✔?`
    #> `i` TODO Improve performance with vctrs tidyverse/dplyr#6806 (<https://github.com/tidyverse/dplyr/issues/6806>)- `Done✔?`
    #> `i` Print interactive outline of file sections
    #> `i` TODO` items- `Done✔?`
    #> `i` `proj_outline()` and `dir_outline()` are wrapper of `file_outline()`.
    #> 
    #> The parser is very opinioneted and is not very robust as it is based on regexps.
    #> For a better file parser, explore other options, like lightparser (<https://thinkr-open.github.io/lightparser/>) for Quarto,  `{roxygen2}`
    #> 
    #> Will show TODO items and will offer a link to [mark them as
    #> complete][complete_todo()].
    #> 
    #> ── `R/proj-list.R`  Opens a RStudio project in a new session
    #> `i` TODO maybe add a max?- `Done✔?`
    #> `i` TODO improve on this message- `Done✔?`
    #> `i` Access the file outline within other project
    #> `i` Returns a named project list options
    #> `i` If not specified, will generate hyperlinks that call [usethis::proj_activate()].
    #> `proj_switch()` looks at `options(reuseme.reposdir)`.
    #> `i` It can be used as [file_outline()] + `proj`.
    #> `i` It peeks `options(reuseme.reposdir)` to find projects.
    #> `i` project management helpers
    #> `i` project management helpers
    #> `i` project management helpers
    #> 
    #> ── `R/rename-files.R`  Rename an output or a data file and watch for references
    #> `i` After here, we start doing some renaming real situations
    #> `i` Helpers
    #> `i` helpers for computing scope of renaming
    #> `i` TODO measure of string proximity- `Done✔?`
    #> `i` Prevent renaming if something is going on
    #> `i` FIXME maybe not fail while testing- `Done✔?`
    #> `i` TODO Check that old file is more recent- `Done✔?`
    #> `i` `r lifecycle::badge("experimental")`
    #> 
    #> This function can improve your workflow.
    #> It is inspired by [usethis::rename_files()], but its scope
    #> is more oriented towards analysis script.
    #> 
    #> # Use case
    #> 
    #> Let's say you have an analysis and work on a certain subject.
    #> You want to rename a figure for clarity.
    #> For example, you had an input file named `data/my-streets.csv` and you now want to
    #> rename it to
    #> 
    #> Here is what `rename_files2()` does for you, before it renames files.
    #> 
    #> 1. Look for potential name conflict
    #> 2. Look for data frame name conflicts
    #> 3. Sends information to clipboard
    #> 
    #> Will work well for you if you tend to name your objects using snake case and
    #> naming objects with snake case or kebab-case.
    #> 
    #> The philosophy is to inform you of manual steps required before actually
    #> performing file renaming.
    #> 
    #> A way to be less strict is to us
    #> 
    #> ── `R/use-todo.R`  Add a TODO list by project to a TODO.R file in the base directory

    #> `i` TODO think about maybe using todo = clipr::read_clip()- `Done✔?`
    #> `i` TODO nice to have, but would need to extract duplicates- `Done✔?`
    #> `i` Helpers
    #> `i` Remove a TODO/WORK/FIXME item from a file
    #> `i` Creates or edits a `TODO.R` file to store your TODOs.
    #> By default it will write in the current RStudio project.
    #> `i` Function meant to be wrapped as ```` hyperlinks with [file_outline()].
    #> It basically removes a line from a file.
    #> `i` If you use `use_todo()` with a version-control repository, you may want to
    #> use `usethis::use_git_ignore("TODO.R")` if you don't want your `TODO.R` file
    #> 
    #> to be included in git. If using in a package directory, use
    #> `usethis::use_build_ignore("TODO.R")` to prevent a note in `R CMD CHECK`

    #> 
    #> ── `R/utils-proj.R`  usethis adaptions utils
    #> `i` Active project / document
    #> `i` copy of usethis::is_package
    #> 
    #> ── `R/utils.R`  OS utils
    #> 
    #> ── `tests/testthat/_ref/many-titles.md`
    #> `i` TODO this is an item- `Done✔?`
    #> 
    #> ── `tests/testthat/_ref/my-analysis.md`  My doc title
    #> `i` Dashboard card
    #> `i` A long ggplot2 title
    #> 
    #> ── `tests/testthat/_ref/my-analysis.R`  Analyse my streets
    #> `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
    #> `i` data wrangling
    #> `i` Write my streets
    #> `i` TODO Create a new version- `Done✔?`
    #> `i` 'R/my-file.R'
    #> `i` Section title
    #> 
    #> ── `tests/testthat/_ref/test-roxygen-safeguard.R`  Test for roxygen parsing for no error
    #> `i` Use 'tests/testthat/_ref/test-roxygen.R' for output testing
    #> `i` Title
    #> `i` An S3 method not to be include
    #> `i` content
    #> `i` a family to include
    #> 
    #> ── `tests/testthat/_ref/test-roxygen.R`
    #> `i` Use 'tests/testthat/_ref/test-roxygen-safeguard.R' for output testing
    #> `i` Complete block for exported function with headings
    #> `i` block not to index
    #> `i` Topic to index
    #> `i` data to index
    #> `i` TODO add data block example to index- `Done✔?`
    #> `i` A title to be included
    #> `i` A title not to be included (internal function)
    #> `i` A title to be included
    #> `i` First to be included:
    #> 
    #> Content not to be included
    #> `i` A description not to be included
    #> `i` A description not to include
    #> `i` A description not to be included
    #> `i` A second-level heading in description to be included?
    #> 
    #> # A detail first level-…
    #> `i` heading not to be included
    #> 
    #> content
    #> `i` A second-level heading in description to be included?
    #> 
    #> # A detail first level-…
    #> `i` a family to include
    #> `i` a family to include
    #> 
    #> ── `tests/testthat/test-case-if-any.R`
    #> `i` case_if_any() basic work
    #> `i` wrong cases error
    #> `i` case_if_any() can use a newly created variable (#8)
    #> 
    #> ── `tests/testthat/test-dplyr-plus.R`
    #> `i` filter_if_any() errors correctly when using `by` instead of `.by`
    #> `i` filter_if_any() errors with across()
    #> `i` TODO improve this error- `Done✔?`
    #> `i` adds rows in front, but warns the user
    #> `i` summarise_with_total() keeps factors
    #> `i` na_if2() works with expr and values
    #> 
    #> ── `tests/testthat/test-eda-identity.R`
    #> `i` Returns identity
    #> `i` Side effects are what's intended in interactive sessions
    #> 
    #> ── `tests/testthat/test-link-elements.R`
    #> `i` link_gh_issue() + markup_href() work
    #> 
    #> ── `tests/testthat/test-named.R`
    #> `i` min/max/unique_named() return named output
    #> `i` max_named() and unique_named() work with unnamed vectors
    #> 
    #> ── `tests/testthat/test-open.R`
    #> `i` open_rs_doc() errors in non-interactive sessions
    #> 
    #> ── `tests/testthat/test-outline-criteria.R`  Test individual outline elements
    #> `i` No outline criteria are untested
    #> 
    #> ── `tests/testthat/test-outline-roxy.R`
    #> `i` roxy tags don't error
    #> 
    #> ── `tests/testthat/test-outline.R`
    #> `i` alpha and work_only arguments work
    #> `i` file_outline() is a data frame
    #> `i` TODO change tests for data frame size when stable (efficiency). As stil…- `Done✔?`
    #> `i` file_outline() with only title doesn't error
    #> `i` file_outline() contains function calls
    #> `i` dir_outline() works with no error
    #> 
    #> ── `tests/testthat/test-rename-files.R`
    #> `i` Helper files returns the expected input
    #> `i` force and action are deprecated
    #> 
    #> ── `tests/testthat/test-screenshot.R`
    #> `i` screenshot() does nothing in non-interactive sessions
    #> 
    #> ── `tests/testthat/test-use-todo.R`
    #> `i` Marking TODO as done detects tags
    #> `i` TODO items are correctly stripped
    #> 
    #> ── `tests/testthat/test-utils.R`
    #> `i` Windows is recognized correctly.
    #> 
    #> ── `TODO.R`
    #> `i` TODO screenshot make the behaviour different when vignettes vs articl…- `Done✔?`
    #> `i` TODO screenshot RStudio addin to insert the code directly in the qmd …- `Done✔?`
    #> `i` TODO use_family() to edit .R file to add @family data frames tags to ro…- `Done✔?`
    #> `i` TODO mutate_identity redundant if the focus pillar PR was merged. r-lib/pillar#585 (<https://github.com/r-lib/pillar/issues/585>)- `Done✔?`
    #> `i` TODO rename if many matches, separate those with the exact path.- `Done✔?`
    #> `i` TODO outline make ggtitle work- `Done✔?`
    #> `i` TODO outline show extra msg only for some, but in file outline, not i…- `Done✔?`
    #> `i` TODO outline detect help calls and apply markup. `?fs::file_show` dis…- `Done✔?`
    #> `i` TODO outline renable cli info.- `Done✔?`
    #> `i` TODO escape_markup doesn't work with complex operation {x^2} for example. Maybe if detecting something complex, use cli_escape function. escape-complex-markyp branch created to try to address this.- `Done✔?`
    #> `i` TODO outline avoid evaluating in current env.- `Done✔?`
    #> `i` TODO wrap regexps in functions- `Done✔?`
    #> `i` TODO outline remove examples from outline. Sometimes commented code i…- `Done✔?`
    #> `i` TODO outline roxygen comments processing should be left to `roxygen2::parse_file()`- `Done✔?`
    #> `i` TODO outline show key like `pak::pkg_deps_tree()` does.- `Done✔?`
    #> `i` TODO outline roxygen function title- `Done✔?`
    #> `i` TODO outline remove ggtext markup from plot title.- `Done✔?`
    #> `i` FIXME outline comments are now interpreted as section- `Done✔?`
    #> `i` TODO outline todos in qmd file inside html comment- `Done✔?`
    #> `i` TODO reframe more than one issue. nw drive- `Done✔?`
    #> `i` TODO delete generated files- `Done✔?`
    #> `i` TODO [proj_file] to accesss data (return the path in this case?)- `Done✔?`
    #> `i` TODO [check_referenced_files] doesn't check for 'R/file.R'- `Done✔?`
    #> `i` TODO explain rationale behind `work_only`. Suggest to transform to TODO…- `Done✔?`
    #> `i` TODO outline Show function call if exported + not internal + bonus if…- `Done✔?`
    #> `i` TODO title of file could be function title if it is first element [proj…- `Done✔?`
    #> 
    #> ── `R/browse-pkg.R`  Browse pkgdown site if it exists
    #> `i` A wrapper around [usethis::browse_package()] that aims at identifying the
    #> package website. It looks up for a link in DESCRIPTION.
    #> 
    #> ── `R/case-if-any.R`  case-when, but checks for all matches, returns a character
    #> `i` Each case is evaluated for **all** cases and a character vector match
    #> for each element determines the corresponding value in the output vector.
    #> If no cases match, the `.default` is used.
    #> The function allows you to assign multiple values to a character value, which
    #> can be very handy for EDA.
    #> 
    #> ── `R/escape-inline-markup.R`
    #> `i` escape inline markup in case of problems
    #> `i` Is inline markup valid?
    #> `i` inline markup internal helpers
    #> 
    #> ── `R/import-standalone-obj-type.R`
    #> `i` Return English-friendly type
    #> `i` Return OO type
    #> 
    #> ── `R/link-elements.R`  Create a markdown link to a GitHub issue
    #> `i` Create a cli href with a markdown link
    #> `i` In RStudio, links to issues are automatically recognized.
    #> This function creates intermediate markdown links to entries of the form rstudio/rstudio#1100 (<https://github.com/rstudio/rstudio/issues/1100>)
    #> `i` Transforms `[text](url)` -> `text (<url>)`
    #> `i` Note: doesn't (yet) support without <OWNER>/<REPO>
    #> 
    #> Basically trransform repo/org#xx -> repo/org#xx (<https://github.com/repo/org/issues/xx>).
    #> 
    #> Afterwards, we use [markup_href()] to create a cli link
    #> `i` inline markup internal helpers
    #> `i` inline markup internal helpers
    #> 
    #> ── `R/named.R`  Helpers that can return a named vector
    #> `i` Base R keeps names in various places, but drops them elsewhere
    #> These functions are some that I use frequently, like `max`, or `unique`
    #> 
    #> ── `R/proj-reuseme.R`  Interact with different RStudio projects

    #> `i` The package offers many ways to interact with different local RStudio projects.
    #> `i` Setup
    #> 
    #> To take advantage of this functionality, you first have to set `options(reuseme.reposdir)` in
    #> your .Rprofile file. Access it with [usethis::edit_r_profile()].
    #> 
    #> I would recommend you add the following. It works better if you store your RStudio
    #> projects in common directories.
    #> 
    #> Inspired by [usethis options][usethis::usethis_options]
    #> 
    #> ```
    #> if (interactive()) NULL
    #> ```
    #> 
    #> # Capabilities.
    #> 
    #> Assumes that you have a project named `"learning"`
    #> A project outline
    #> 
    #> ```
    #> proj_outline(proj = "learning)
    #> ```
    #> 
    #> Add a TODO item to the `learning` project
    #> 
    #> ```
    #> use_todo("learning::Learn this")
    #> ```
    #> 
    #> Get file [outline][proj_outline()] of the `file.R` in "learning"
    #> 
    #> ```
    #> proj_file("file", "learning")
    #> ```
    #> 
    #> Move to a new project in the same session
    #> 
    #> ```
    #> proj_switch("learning")
    #> ```
    #> 
    #> A lot of these features are already present in RStudio and with usethis.
    #> However, when managing many projects, the recent projects list can be more difficult
    #> to handle.
    #> Passing the full project name to `usethis::proj_activate()` was too long.
    #> `i` project management helpers

    #> 
    #> ── `R/quarto-help.R`  Show links to Quarto documentation of interest
    #> `i` Very opinionated of links I need to access periodically. Easily
    #> accessible from R console.
    #> 
    #> ── `R/reuseme-package.R`
    #> `i` reuseme: Collections of Utility Functions to Work Across Projects
    #> `i` Allows you to browse current projects, rename files safely, add screenshots to project on Windows. It is also my personal library and contains wrapper around common functions, from dplyr and readxl. It takes advantage of cli hyperlinks. Finally, it provides a custom print method for tibbles, inspired by janitor, and readr.
    #> 
    #> ── `R/screenshot.R`  Save the current image in clipboard to png in your active directory
    #> `i` The screenshot will be saved as `.png` to a directory following these rules
    #> 1. In a regular RStudio project (or a Quarto book), it will be saved to a `images/` directory
    #> 2. In a package project, it will be saved in a `man/figures` directory
    #> 3. In a Quarto Blog project, it will save in the current post's folder.
    #> 4. You can always override these defaults by setting `dir`
    #> 
    #> After using the shortcut Win + Shift + S, you can call this function!
    #> `i` If no file name is supplied, a file named `image0*.png` will be created.
    #> The function then prompts you to rename the file with a more expressive name.
    #> It will continue the numbering if a file named image exists.
    #> 
    #> Still have to validate if it works on macOS, as it is not clear whether the
    #> image goes to the clipboard by default
    #> 
    #> The maximum number of images in a folder is 99. (only padding 2), should be enough.
    #> 
    #> You should not be able to overwrite a screenshot with a generic name, only a
    #> named one as it is possible you may require to retake your screenshot.
    #> 
    #> ── `README.Rmd`
    #> `i` hello

</p>
</details>
