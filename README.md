
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reuseme

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/reuseme)](https://CRAN.R-project.org/package=reuseme)
[![R-CMD-check](https://github.com/olivroy/reuseme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/olivroy/reuseme/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/olivroy/reuseme/graph/badge.svg)](https://app.codecov.io/gh/olivroy/reuseme)
<!-- badges: end -->

The goal of reuseme is to provide utility functions for project
management across RStudio projects. Sometimes, managing multiple
projects can be challenging. reuseme also aims to simplify project
management on Windows. You may need to switch quickly to a project, add
things or browse a certain file if you have some replications across
projects. Sometimes, it is hard to do that. reuseme also aims to help me
overcome things I don’t like on Windows.

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
  recommended for avoiding surprises! No need to be hosted on
  repositories like GitLab or GitHub)
- You use machine and human-readable paths (i.e. no spaces, special
  characters) (Tip: don’t hesitate to rename your files
  (`reuseme::rename_files2()`), your future self will thank you!

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

<table style="width:99%;">
<caption>usethis vs reuseme</caption>
<colgroup>
<col style="width: 26%" />
<col style="width: 26%" />
<col style="width: 43%" />
<col style="width: 2%" />
</colgroup>
<tbody>
<tr class="odd">
<td rowspan="2"><h1 id="workflow">Workflow</h1>
<p>Switch to project “cool-project”</p></td>
<td rowspan="2"><h1 id="reuseme-1">reuseme</h1>
<p><code>proj_switch(proj = "cool-project")</code></p></td>
<td rowspan="2"><h1 id="usethis">usethis</h1>
<p><code>proj_activate(path = "C:/users/long/path/to/cool-project")</code></p></td>
<td></td>
</tr>
<tr class="even">
<td></td>
</tr>
<tr class="odd">
<td>Write a TODO item in project “cooler-project”, while working in
“cool-project”</td>
<td
colspan="3"><code>reuseme::use_todo(todo = "I need to do this ASAP as possible", proj = "cooler-project")</code>
|
<code>usethis::write_union(path = "C:/Users/I/do/not/want/to/type/cooler-project/TODO.R", lines = "I need to do this ASAP as possible.")</code></td>
</tr>
<tr class="even">
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
<td></td>
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
#> 1 outline <- proj_outline()    453ms    459ms      2.18    22.5MB     4.36
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
#> `i` TODO improve this Viz!
#> 
#> ── `R/dplyr-plus.R`  dplyr extra
#> `i` FIXME Doesn't work, problem with symbols here
#> `i` TODO use `check_length()` when implemented. r-lib/rlang#1618 (<https://github.com/r-lib/rlang/issues/1618>)
#> `i` summarise with total
#> 
#> ── `R/eda-identity.R`  ----- dplyr/base identity helpers
#> `i` base identity functions
#> `i` dplyr identity functions with small tweaks
#> `i` dplyr identity without tweaks
#> `i` dplyr extensions identity
#> `i` helpers
#> 
#> ── `R/files-conflicts.R`
#> `i` TODO insert in either proj_outline, or rename_file
#> `i` FIXME in Rbuilignore, change `^_pkgdown\.yml$` to `_pkgdown.yml` to make sure it works
#> `i` TODO probably needs a `detect_genuine_path()`
#> `i` Helpers
#> `i` TODO Add false positive references
#> `i` TODO fs::path and file.path should be handled differently
#> 
#> ── `R/import-standalone-types-check.R`
#> `i` Scalars
#> `i` Vectors
#> 
#> ── `R/open.R`
#> `i` FIXME why is this code like this?
#> `i` TODO structure and summarise information.
#> `i` FIXME (upstream) the color div doesn't go all the way r-lib/cli#694 (<https://github.com/r-lib/cli/issues/694>)
#> 
#> ── `R/outdated-pkgs.R`
#> `i` TODO figure out pad :)
#> 
#> ── `R/outline-criteria.R`
#> `i` Add variable to outline data frame
#> `i` TODO extract title in roxy comments (@title too.L)
#> `i` TODO strip is_cli_info in Package? only valid for EDA
#> `i` FIXME try to detect all the chunk caption, but would have to figure out the end of it maybe lightparser.
#> `i` it is 'R/outline.R'
#> 
#> ── `R/outline.R`  `proj_outline()`
#> `i` `file_outline()`
#> `i` File outline
#> `i` TODO expand this to apply to most generated files
#> `i` Print method
#> `i` TODO since April 2024, cli links work almost out of the box in VScode? microsoft/vscode#176812 (<https://github.com/microsoft/vscode/issues/176812>)
#> `i` Step: tweak outline look as they show
#> `i` TODO reanable cli info
#> `i` TODO Improve performance with vctrs tidyverse/dplyr#6806 (<https://github.com/tidyverse/dplyr/issues/6806>)
#> 
#> ── `R/proj-list.R`
#> `i` TODO maybe add a max?
#> `i` TODO improve on this message
#> 
#> ── `R/rename.R`
#> `i` After here, we start doing some renaming real situations
#> `i` TODO verify if path should be normalized.
#> `i` Helpers
#> `i` helpers for computing scope of renaming
#> `i` TODO measure of string proximity
#> `i` Prevent renaming if something is going on
#> `i` FIXME maybe not fail while testing
#> `i` TODO Check that old file is more recent
#> 
#> ── `R/todo.R`
#> `i` TODO think about maybe using todo = clipr::read_clip()
#> `i` TODO nice to have, but would need to extract duplicates
#> `i` Helpers
#> 
#> ── `R/utils-proj.R`  usethis adaptions utils
#> `i` Active project / document
#> 
#> ── `R/utils.R`  OS utils
#> 
#> ── `tests/testthat/_outline/knitr-notebook.R`  Crop Analysis Q3 2013
#> `i` A great section
#> 
#> ── `tests/testthat/_outline/my-analysis.md`  My doc title
#> `i` A section
#> `i` Dashboard card
#> `i` A code section
#> `i` A subsection
#> `i` A section2
#> `i` A long ggplot2 title
#> `i` A code section
#> 
#> ── `tests/testthat/_outline/my-analysis.R`  Analyse my {streets}
#> `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
#> `i` data wrangling
#> `i` Write my streets
#> `i` TODO Create a new version
#> `i` 'R/my-file.R'
#> `i` Section title
#> `i` A section
#> `i` The last section
#> 
#> ── `tests/testthat/_outline/title.md`  The title is the only outline element
#> 
#> ── `tests/testthat/_outline/titles.md`  The title is the only outline element
#> `i` Another title
#> `i` Second level
#> `i` TODO this is an item
#> `i` Last title
#> `i` `function_name()` title
#> 
#> ── `tests/testthat/_snaps/browse-pkg.md`
#> `i` browse_pkg() works
#> 
#> ── `tests/testthat/_snaps/case-if-any.md`
#> `i` case_if_any() basic work
#> `i` wrong cases error
#> 
#> ── `tests/testthat/_snaps/dplyr-plus.md`
#> `i` filter_if_any() errors with across()
#> `i` adds rows in front, but warns the user
#> `i` summarise_with_total() works
#> `i` extract_cell_value() works
#> `i` slice_min_max() works
#> `i` na_if2() works with expr and values
#> 
#> ── `tests/testthat/_snaps/eda-identity.md`
#> `i` Side effects are what's intended in interactive sessions
#> 
#> ── `tests/testthat/_snaps/escape-inline-markup.md`
#> `i` escape_markup() works with {
#> `i` replace_r_var() works
#> 
#> ── `tests/testthat/_snaps/markup.md`
#> `i` link_gh_issue() + markup_href() work
#> 
#> ── `tests/testthat/_snaps/outline-criteria.md`
#> `i` No outline criteria are untested
#> 
#> ── `tests/testthat/_snaps/outline.md`
#> `i` file_outline() works
#> `i` alpha arguments works
#> `i` file_outline() is a data frame
#> `i` pattern works as expected
#> `i` file_outline() detects correctly knitr notebooks
#> 
#> ── `tests/testthat/_snaps/proj-list.md`
#> `i` proj_file() works
#> 
#> ── `tests/testthat/_snaps/quarto-help.md`
#> `i` href_name_url() works
#> `i` quarto_help() works
#> 
#> ── `tests/testthat/_snaps/rename.md`
#> `i` rename_files2(): prevents file renaming if conflicts
#> `i` rename_files2(): is easier to test messages with no action
#> `i` rename_files2(): renames files if forced to do so
#> `i` rename_files2(): doesn't check for references if file name is short
#> `i` rename_files2(): priorizes references if name is generic or widely used in files
#> `i` rename_files2(): can accept overridden preferences
#> `i` rename_files2(): relaxes its conditions for figures
#> `i` rename_files2(): calls check_referenced_files()
#> `i` Helper files returns the expected input
#> 
#> ── `tests/testthat/_snaps/screenshot.md`
#> `i` screenshot() does nothing in non-interactive sessions
#> 
#> ── `tests/testthat/_snaps/todo.md`
#> `i` Marking a TODO item as done works
#> 
#> ── `tests/testthat/test-case-if-any.R`
#> `i` case_if_any() basic work
#> `i` wrong cases error
#> `i` case_if_any() can use a newly created variable (#8 (<https://github.com/olivroy/reuseme/issues/8>))
#> 
#> ── `tests/testthat/test-dplyr-plus.R`
#> `i` filter_if_any() errors with across()
#> `i` TODO improve this error
#> `i` adds rows in front, but warns the user
#> `i` summarise_with_total() keeps factors
#> 
#> ── `tests/testthat/test-eda-identity.R`
#> `i` Returns identity
#> `i` Side effects are what's intended in interactive sessions
#> 
#> ── `tests/testthat/test-escape-inline-markup.R`
#> `i` TODO could . stay as is?
#> `i` escape_markup() doesn't error for edge cases
#> 
#> ── `tests/testthat/test-markup.R`
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
#> ── `tests/testthat/test-outline.R`
#> `i` file_outline() is a data frame
#> `i` TODO change tests for data frame size when stable (efficiency). As still debugging, better to keep all snapshots.
#> `i` file_outline() with only title doesn't error
#> `i` file_outline() contains function calls
#> 
#> ── `tests/testthat/test-rename.R`
#> `i` rename_files2()
#> `i` force and action are deprecated
#> 
#> ── `tests/testthat/test-screenshot.R`
#> `i` screenshot() does nothing in non-interactive sessions
#> 
#> ── `tests/testthat/test-todo.R`
#> `i` Marking TODO as done detects tags
#> 
#> ── `TODO.R`
#> `i` TODO screenshot make the behaviour different when vignettes vs articles: vignettes should place it in man/figures, while articles could put it in vignettes/articles file.
#> `i` TODO screenshot RStudio addin to insert the code directly in the qmd doc. No longer needed with RStudio 2023.12
#> `i` TODO use_family() to edit .R file to add @family data frames tags to roxygen
#> `i` TODO mutate_identity redundant if the focus pillar PR was merged. r-lib/pillar#585 (<https://github.com/r-lib/pillar/issues/585>)
#> `i` TODO rename if many matches, separate those with the exact path.
#> `i` TODO outline make ggtitle work
#> `i` TODO outline show extra msg only for some, but in file outline, not in proj?
#> `i` TODO outline detect help calls and apply markup. `?fs::file_show` disregard finishing `.` (not followed by dot)
#> `i` TODO outline renable cli info.
#> `i` TODO escape_markup doesn't work with complex operation {x^2} for example. Maybe if detecting something complex, use cli_escape function. escape-complex-markyp branch created to try to address this.
#> `i` TODO outline avoid evaluating in current env.
#> `i` TODO wrap regexps in functions
#> `i` TODO outline remove examples from outline. Sometimes commented code is caught.
#> `i` TODO outline roxygen comments processing should be left to `roxygen2::parse_file()`
#> `i` TODO outline show key like `pak::pkg_deps_tree()` does.
#> `i` TODO outline roxygen function title
#> `i` TODO outline remove ggtext markup from plot title.
#> `i` FIXME outline comments are now interpreted as section
#> `i` TODO outline todos in qmd file inside html comment
#> `i` TODO reframe more than one issue. nw drive
#> `i` TODO delete generated files
#> `i` TODO [check_referenced_files] doesn't check for 'R/file.R'
#> `i` TODO browse_pkg should open by default if no vignettes are found, because there is not much to do in the R-session.
#> `i` TODO exclude _files from `proj_list()`
#> `i` TODO rename_files should be less noisy about project name file
#> `i` TODO add_to_tricks(). when detecting TRICK like complete todo, but not remove line. requires a scheme. moves the item to tricks.md at the correct place. (copy to clipboard is probably enough)
#> `i` TODO use vapply() instead of purrr::map
#> `i` TODO rename_files() should know about .covrignore too
#> `i` TODO withr::local_dir for proj_outline.
#> `i` TODO add warn_conflicts = 'none'
#> 
#> ── `NEWS.md`
#> `i` reuseme (development version)
#> 
#> ── `README.Rmd`
#> `i` reuseme
#> `i` Installation
#> `i` Getting started
#> `i` hello
#> `i` Extend usethis functionality
#> `i` Proposing a data analysis workflow
#> `i` Outline speed
#> 
#> ── `tests/testthat/_outline/NEWS.md`
#> `i` Package 202a20.9000
#> `i` Package (development)
#> `i` Pac
#> `i` Package 1.0-0
#> `i` Package 0.9-0
#> `i` Package 0.1-0
```

</p>
</details>
