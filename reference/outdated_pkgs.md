# Looks for outdated packages

Only checks for binaries, which has the advantage of not notifying you
when packages are newly available on CRAN, but without the binaries
available. Installing packages from source can be longer, hence the
advantage of waiting a few days for binaries.

It takes advantage of pak's capacities to allow you to install packages
on Windows without restarting session.

## Usage

``` r
outdated_pkgs(type = c("binary", "source"))
```

## Arguments

- type:

  Type of package to look for (`"binary"` or `"source"`)

## Value

A list of packages that can be updated, with links to news, the pkgdown
site.

## Examples

``` r
# \donttest{
outdated_pkgs()
#> ✔ All packages are up to date.
# }
```
