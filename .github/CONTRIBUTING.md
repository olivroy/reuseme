# Contributing to reuseme

This outlines how to propose a change to reuseme.
For a detailed discussion on contributing to this and other tidyverse packages, please see the [development contributing guide](https://rstd.io/tidy-contrib) and our [code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("olivroy/reuseme", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

### Debugging `proj_outline()`

* `define_outline_criteria()` and underlying `o_is_*()` for changing logic around what constitutes an outline element

* `define_outline_criteria()` if an item shows as outline, but seems like a false positive, 


* `keep_outline_element()`: if an element is **missing** from outline.

* `define_important_element()` if an element is important [^1] 

[^1]: Importance definition is in `define_important_element()`, while styling is in `construct_outline_link()`

* `display_outline_element()` if the outline element doesn't show as expected (i.e right position, but not enough or too much stripping), edit 

* `construct_outline_link()` if the link (formatting) seems incorrect.

* `print.outline_report()` if you want to improve styling



### Enhancing `proj_outline()`

Example with [ggtitle](link)


1. Create a new function in `R/outline-criteria.R` (place right before `define_outline_criteria()`)


```r
# Detects ggtitle(')
o_is_ggtitle <- function(x) {
  stringr::str_detect(x, "ggtitle\\(['\"]")
}
```

2. Add test for function

```r
# Run
use_test("outline-criteria")

# add where appropriate (at the end probably)

test_that("o_is_ggtitle() works", {
  expect_true(o_is_ggtitle("ggtitle('Main plot title')"))
})
```

2. Create new variable in `define_outline_criteria()`

```r
is_ggtitle = o_is_ggtitle(content),
```

3. Add correct criteria to `keep_outline_element()`

Be careful for markdown vs non-markdown (i.e. section title not the same)

```r
# in our case we'd go in the third section (generic) and would add
| is_ggtitle
```

4. Create new conditions on how to display in `display_outline_element()`

```r
   is_ggtitle = stringr::str_remove_all(content, "(ggplot2\\:\\:)?ggtitle\\([\"']|[\"']$")
```

5. If important, add to criteria in `define_important_element()`

6. Look at the result. Ideally, add to _ref/my-analysis.R, so it shows somehow in snapshots.

NEWS.md is handled differently than other files.
