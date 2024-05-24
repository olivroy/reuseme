
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
#> 1 outline <- proj_outline()    1.56s    1.56s     0.642    66.8MB     5.78
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
#> ── `LICENSE.md`  MIT License
#> 
#> ── `R/dplyr-plus.R`  dplyr extra
#> `i` FIXME Doesn't work, problem with symbols here- `Done✔?`
#> `i` TODO use `check_length()` when implemented. r-lib/rlang#1618 (<https://github.com/r-lib/rlang/issues/1618>)- `Done✔?`
#> `i` summarise with total
#> `i` with dplyr::filter
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
#> `i` Use cases / advantages
#> `i` Caution
#> 
#> ── `R/files-conflicts.R`
#> `i` TODO insert in either proj_outline, or rename_file- `Done✔?`
#> `i` TODO probably needs a `detect_genuine_path()`- `Done✔?`
#> `i` Helpers
#> `i` TODO Add false positive references- `Done✔?`
#> `i` TODO fs::path and file.path should be handled differently- `Done✔?`
#> 
#> ── `R/import-standalone-types-check.R`
#> `i` Scalars
#> `i` Vectors
#> 
#> ── `R/open.R`
#> `i` FIXME why is this code like this?- `Done✔?`
#> `i` TODO structure and summarise information.- `Done✔?`
#> `i` FIXME (upstream) the color div doesn't go all the way r-lib/cli#694 (<https://github.com/r-lib/cli/issues/694>)- `Done✔?`
#> `i` document manipulation helpers
#> `i` document manipulation helpers
#> 
#> ── `R/outdated-pkgs.R`
#> `i` TODO figure out pad :)- `Done✔?`
#> 
#> ── `R/outline-criteria.R`
#> `i` Add variable to outline data frame
#> `i` TODO strip is_cli_info in Package? only valid for EDA (currently not sh…- `Done✔?`
#> `i` FIXME try to detect all the chunk caption, but would have to figure out the end of it maybe lightparser.- `Done✔?`
#> `i` it is 'R/outline.R'
#> 
#> ── `R/outline-roxy.R`
#> `i` TODO when stable delete- `Done✔?`
#> `i` TODO Delete when stable debugging- `Done✔?`
#> `i` TODO Delete when stable for debugging- `Done✔?`
#> `i` TODO exclude S3 methods- `Done✔?`
#> `i` helper for interactive checking
#> 
#> ── `R/outline.R`  `proj_outline()`
#> `i` `file_outline()`
#> `i` File outline
#> `i` Print method
#> `i` Step: tweak outline look as they show
#> `i` TODO reanable cli info- `Done✔?`
#> `i` TODO Improve performance with vctrs tidyverse/dplyr#6806 (<https://github.com/tidyverse/dplyr/issues/6806>)- `Done✔?`
#> `i` If `work_only` is set to `TRUE`, the function will only return outline of the `# WORK` comment
#> 
#> ── `R/proj-list.R`
#> `i` TODO maybe add a max?- `Done✔?`
#> `i` TODO improve on this message- `Done✔?`
#> `i` project management helpers
#> `i` project management helpers
#> `i` project management helpers
#> 
#> ── `R/rename.R`
#> `i` After here, we start doing some renaming real situations
#> `i` Helpers
#> `i` helpers for computing scope of renaming
#> `i` TODO measure of string proximity- `Done✔?`
#> `i` Prevent renaming if something is going on
#> `i` FIXME maybe not fail while testing- `Done✔?`
#> `i` TODO Check that old file is more recent- `Done✔?`
#> `i` Use case
#> 
#> ── `R/todo.R`
#> `i` TODO think about maybe using todo = clipr::read_clip()- `Done✔?`
#> `i` TODO nice to have, but would need to extract duplicates- `Done✔?`
#> `i` Helpers
#> 
#> ── `R/utils-proj.R`  usethis adaptions utils
#> `i` Active project / document
#> 
#> ── `R/utils.R`  OS utils
#> 
#> ── `tests/testthat/_ref/many-titles.md`  The title is the only outline element
#> `i` Another title
#> `i` Second level
#> `i` TODO this is an item- `Done✔?`
#> `i` Last title
#> 
#> ── `tests/testthat/_ref/my-analysis.md`  My doc title
#> `i` A section
#> `i` Dashboard card
#> `i` A code section
#> `i` A subsection
#> `i` A section2
#> `i` A long ggplot2 title
#> `i` A code section
#> 
#> ── `tests/testthat/_ref/my-analysis.R`  Analyse my streets
#> `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
#> `i` data wrangling
#> `i` Write my streets
#> `i` TODO Create a new version- `Done✔?`
#> `i` 'R/my-file.R'
#> `i` Section title
#> 
#> ── `tests/testthat/_ref/single-title.md`  The title is the only outline element
#> 
#> ── `tests/testthat/_ref/test-roxygen-safeguard.R`  Test for roxygen parsing for no error
#> `i` Use 'tests/testthat/_ref/test-roxygen.R' for output testing
#> `i` a family to include
#> 
#> ── `tests/testthat/_ref/test-roxygen.R`
#> `i` Use 'tests/testthat/_ref/test-roxygen-safeguard.R' for output testing
#> `i` Complete block for exported function with headings
#> `i` block not to index
#> `i` Topic to index
#> `i` data to index
#> `i` TODO add data block example to index- `Done✔?`
#> `i` First to be included
#> `i` A second-level heading in description to be included?
#> `i` A detail first level-heading to be included
#> `i` A detail second-level heading to be included
#> `i` A second-level heading in description to be included?
#> `i` A detail first level-heading to be included
#> `i` A detail second-level heading to be included
#> `i` First to be included
#> `i` a family to include
#> `i` a family to include
#> 
#> ── `tests/testthat/_snaps/case-if-any.md`
#> `i` wrong cases error
#> 
#> ── `tests/testthat/_snaps/dplyr-plus.md`
#> `i` adds rows in front, but warns the user
#> 
#> ── `tests/testthat/_snaps/eda-identity.md`
#> `i` Side effects are what's intended in interactive sessions
#> 
#> ── `tests/testthat/_snaps/outline-criteria.md`
#> `i` No outline criteria are untested
#> 
#> ── `tests/testthat/_snaps/outline.md`
#> `i` alpha and work_only arguments work
#> `i` pattern works as expected
#> 
#> ── `tests/testthat/_snaps/rename.md`
#> `i` Helper files returns the expected input
#> 
#> ── `tests/testthat/_snaps/todo.md`
#> `i` Marking a TODO item as done works
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
#> ── `tests/testthat/test-rename.R`
#> `i` Helper files returns the expected input
#> `i` force and action are deprecated
#> 
#> ── `tests/testthat/test-screenshot.R`
#> `i` screenshot() does nothing in non-interactive sessions
#> 
#> ── `tests/testthat/test-todo.R`
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
#> `i` TODO rename_files should be less noisy about project name file- `Done✔?`
#> 
#> ── `R/proj-reuseme.R`
#> `i` Setup
#> `i` Capabilities.
#> `i` project management helpers
#> 
#> ── `NEWS.md`
#> `i` reuseme (development version)
#> `i` reuseme 0.0.2
#> `i` reuseme 0.0.1
#> 
#> ── `README.Rmd`
#> `i` reuseme
#> `i` Installation
#> `i` Getting started
#> `i` Example
#> `i` hello
#> `i` Extend usethis functionality
#> `i` Proposing a data analysis workflow
#> `i` Outline speed
```

</p>
</details>
