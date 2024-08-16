
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
#> 1 outline <- proj_outline()    2.69s    2.69s     0.372    26.6MB     7.82
```

<details>
<summary>
Example outline
</summary>
<p>

``` r
outline
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
#> ── `R/outdated-pkgs.R`
#> `i` TODO figure out pad :)
#> 
#> ── `R/outline.R`  `proj_outline()`
#> `i` `file_outline()`
#> `i` File outline
#> `i` TODO 1. refactor how columns are initialized to avoid this function alogether?
#> `i` TODO 2. Remove logic for is_title and put that in the print method.
#> `i` TODO We drop these because they don't serve to add much context to TODOs (they don't affect hierarchy)?
#> `i` TODO break indent cleaning into separate function and also apply after file_outline()
#> `i` TODO expand this to apply to most generated files
#> `i` Print method
#> `i` TODO Revert when applying the tree print method.
#> `i` TODO remove title_el work eventually
#> `i` TODO refactor for speed
#> `i` TODO since April 2024, cli links work almost out of the box in VScode? microsoft/vscode#176812 (<https://github.com/microsoft/vscode/issues/176812>)
#> `i` Step: tweak outline look as they show
#> `i` TODO reanable cli info
#> `i` TODO Improve performance with vctrs tidyverse/dplyr#6806 (<https://github.com/tidyverse/dplyr/issues/6806>)
#> `i` FIXME probably not useful anymore
#> 
#> ── `R/utils-proj.R`  usethis adaptions utils
#> `i` Active project / document
#> 
#> ── `R/utils.R`  OS utils
#> 
#> ── `inst/example-file/outline-script.R`  Example for `file_outline()`
#> `i` Load packages
#> `i` Wrangle + visualize data
#> `i` A great title
#> `i` TODO improve this Viz!
#> 
#> ── `tests/testthat/_outline/knitr-notebook.R`  Crop Analysis Q3 2013
#> `i` A great section
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
#> ── `tests/testthat/_outline/my-analysis.md`  My doc title
#> `i` A section
#> `i` Dashboard card
#> `i` A code section
#> `i` A subsection
#> `i` A section2
#> `i` A long ggplot2 title
#> `i` A code section
#> 
#> ── `tests/testthat/_outline/title.md`  The title is the only outline element
#> 
#> ── `tests/testthat/_outline/titles.md`  The title is the first outline element
#> `i` Another title
#> `i` Second level
#> `i` TODO this is an item
#> `i` Last title
#> `i` `function_name()` title
#> 
#> ── `tests/testthat/_snaps/browse-pkg.md`
#> `i` browse_pkg() works
#> 
#> ── `tests/testthat/_snaps/eda-identity.md`
#> `i` Side effects are what's intended in interactive sessions
#> 
#> ── `tests/testthat/_snaps/markup.md`
#> `i` link_gh_issue() + markup_href() work
#> 
#> ── `tests/testthat/_snaps/outline-criteria.md`
#> `i` No outline criteria are untested
#> 
#> ── `tests/testthat/_snaps/proj-list.md`
#> `i` proj_file() works
#> 
#> ── `tests/testthat/_snaps/screenshot.md`
#> `i` screenshot() does nothing in non-interactive sessions
#> 
#> ── `tests/testthat/_snaps/todo.md`
#> `i` Marking a TODO item as done works
#> 
#> ── `tests/testthat/test-markup.R`
#> `i` link_gh_issue() + markup_href() work
#> 
#> ── `tests/testthat/test-open.R`
#> `i` open_rs_doc() errors in non-interactive sessions
#> 
#> ── `tests/testthat/test-outline-criteria.R`  Test individual outline elements
#> `i` No outline criteria are untested
#> 
#> ── `tests/testthat/test-screenshot.R`
#> `i` screenshot() does nothing in non-interactive sessions
#> 
#> ── `tests/testthat/test-todo.R`
#> `i` Marking TODO as done detects tags
#> 
#> ── `NEWS.md`
#> `i` reuseme (development version)
```

</p>
</details>
