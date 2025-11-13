# Open Files Pane at current document location

Easily navigate to active file document.

## Usage

``` r
active_rs_doc_nav(path = active_rs_doc())
```

## Arguments

- path:

  A path to file to navigate to (default active document).

## Value

NULL, called for its side effects.

## Details

Wrapper around
[executeCommand("activateFiles")](https://rstudio.github.io/rstudioapi/reference/executeCommand.html) +
[`rstudioapi::filesPaneNavigate()`](https://rstudio.github.io/rstudioapi/reference/filesPaneNavigate.html) +
[`rstudioapi::getActiveDocumentContext()`](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html)
