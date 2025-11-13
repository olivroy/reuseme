# Delete the active RStudio document safely

**\[experimental\]**

Gathers informative summary about the document you are about to delete.

Will delete more easily if file name starts with `temp-`, if file is
untracked and recent.

## Usage

``` r
active_rs_doc_delete()
```

## Value

Called for side-effects. The document content invisibly if deleting and
reason.

## See also

Other document manipulation helpers:
[`active_rs_doc_copy()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_copy.md)

## Examples

``` r
if (FALSE) {
active_rs_doc_delete()
}
```
