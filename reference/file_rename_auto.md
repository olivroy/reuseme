# Move file automatically between folders

- `file_rename_auto()` automatically renames your file to a better name
  while keeping the same folder structure

- `file_move_dir_auto()` automatically moves your file while keeping the
  same file name

- `file_copy_auto()` automatically copies your file while keeping the
  same file name (Useful to copy read-only files).

## Usage

``` r
file_rename_auto(new_name, old_file = .Last.value)

file_move_dir_auto(new_dir, old_file = .Last.value)

file_copy_auto(new_dir, old_file = .Last.value, overwrite = FALSE)
```

## Arguments

- new_name, new_dir:

  New directory or file name (without extension)

- old_file:

  The old file name

- overwrite:

  Overwrite files if they exist. If this is `FALSE` and the file exists
  an error will be thrown.

## Value

The new full path name, invisibly, allowing you to call the functions
another time.

## Advantages

Instead of calling
`fs::file_move("path/to/dir/file.R", "path/to/dir/new-file.R")`, you can
just call `file_rename_auto("new-file", "path/to/dir/file.R")`

Instead of calling
`fs::file_move("path/to/dir/file.R", "path/to/new-dir/file.R")`, you can
just call `file_move_auto("new-dir", "path/to/dir/file.R")`

If the functions are used in conjunction with
[`file_move_temp_auto()`](https://olivroy.github.io/reuseme/reference/file_move_temp_auto.md),
