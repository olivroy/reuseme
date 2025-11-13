# Package index

## Worflow helpers

With these usethis-inspired functions, you can speed-up your workflow.

### Project Management

- [`file_outline()`](https://olivroy.github.io/reuseme/reference/outline.md)
  [`proj_outline()`](https://olivroy.github.io/reuseme/reference/outline.md)
  [`dir_outline()`](https://olivroy.github.io/reuseme/reference/outline.md)
  : Print interactive outline of file sections

- [`proj_file()`](https://olivroy.github.io/reuseme/reference/proj_file.md)
  : Access the file outline within other project

- [`proj_path()`](https://olivroy.github.io/reuseme/reference/proj_path.md)
  [`proj_list()`](https://olivroy.github.io/reuseme/reference/proj_path.md)
  :

  Specify `proj` in functions

- [`proj_switch()`](https://olivroy.github.io/reuseme/reference/proj_switch.md)
  : Opens a RStudio project in a new session

- [`proj-reuseme`](https://olivroy.github.io/reuseme/reference/proj-reuseme.md)
  : Interact with different RStudio projects

### Package browsing / management

- [`browse_pkg()`](https://olivroy.github.io/reuseme/reference/browse_pkg.md)
  : Browse pkgdown site if it exists
- [`quarto_help()`](https://olivroy.github.io/reuseme/reference/quarto_help.md)
  : Show links to Quarto documentation of interest
- [`outdated_pkgs()`](https://olivroy.github.io/reuseme/reference/outdated_pkgs.md)
  : Looks for outdated packages

### Interact with documents

- [`use_todo()`](https://olivroy.github.io/reuseme/reference/use_todo.md)
  : Add a TODO list by project to a TODO.R file in the base directory
- [`screenshot()`](https://olivroy.github.io/reuseme/reference/screenshot.md)
  : Save the current image in clipboard to png in your active directory
- [`rename_files2()`](https://olivroy.github.io/reuseme/reference/rename_files2.md)
  **\[experimental\]** : Rename an output or a data file and watch for
  references
- [`file_move_temp_auto()`](https://olivroy.github.io/reuseme/reference/file_move_temp_auto.md)
  : Move temporary file automatically from the R console
- [`file_rename_auto()`](https://olivroy.github.io/reuseme/reference/file_rename_auto.md)
  [`file_move_dir_auto()`](https://olivroy.github.io/reuseme/reference/file_rename_auto.md)
  [`file_copy_auto()`](https://olivroy.github.io/reuseme/reference/file_rename_auto.md)
  : Move file automatically between folders
- [`active_rs_doc_copy()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_copy.md)
  : Copy the active document to the same location
- [`active_rs_doc_delete()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_delete.md)
  **\[experimental\]** : Delete the active RStudio document safely
- [`active_rs_doc_move()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_move.md)
  : Move the active document to another directory
- [`active_rs_doc_nav()`](https://olivroy.github.io/reuseme/reference/active_rs_doc_nav.md)
  : Open Files Pane at current document location
- [`open_rs_doc()`](https://olivroy.github.io/reuseme/reference/open_rs_doc.md)
  [`active_rs_doc()`](https://olivroy.github.io/reuseme/reference/open_rs_doc.md)
  [`active_rs_dir()`](https://olivroy.github.io/reuseme/reference/open_rs_doc.md)
  : Open a Document in RStudio

## Data analysis helpers / EDA

These functions are to be used when analysing new data

### Extra goodies to work with vectors or data

Some are wrappers around dplyr functions, some fix base R
inconsistencies.

- [`min_named()`](https://olivroy.github.io/reuseme/reference/named-base.md)
  [`max_named()`](https://olivroy.github.io/reuseme/reference/named-base.md)
  [`unique_named()`](https://olivroy.github.io/reuseme/reference/named-base.md)
  : Helpers that can return a named vector
- [`slice_min_max()`](https://olivroy.github.io/reuseme/reference/slice_min_max.md)
  : Subset rows using their positions
- [`count_pct()`](https://olivroy.github.io/reuseme/reference/count_pct.md)
  : Count observations by group and compute percentage
- [`categorize()`](https://olivroy.github.io/reuseme/reference/categorize.md)
  : Categorize a vector of free text
- [`case_if_any()`](https://olivroy.github.io/reuseme/reference/case_if_any.md)
  : case-when, but checks for all matches, returns a character
- [`filter_if_any()`](https://olivroy.github.io/reuseme/reference/filter_if_any.md)
  : Keep rows that match one of the conditions
- [`filter_detect()`](https://olivroy.github.io/reuseme/reference/filter_detect.md)
  : Filter rows by pattern
- [`select_check()`](https://olivroy.github.io/reuseme/reference/select_check.md)
  : Give a more informative error in case of tidyselect errors
- [`visual_diff_df()`](https://olivroy.github.io/reuseme/reference/visual_diff_df.md)
  : Get a widget that displays diff between data frame
- [`extract_cell_value()`](https://olivroy.github.io/reuseme/reference/extract_cell_value.md)
  : Elegant wrapper around filter and pull
- [`slice_group_sample()`](https://olivroy.github.io/reuseme/reference/slice_group_sample.md)
  : Explore all rows in a random group
- [`na_if2()`](https://olivroy.github.io/reuseme/reference/na_if2.md) :
  Transform to NA any of the condition
- [`summarise_with_total()`](https://olivroy.github.io/reuseme/reference/summarise_with_total.md)
  : Compute a summary for groups with the total included.

### dplyr and base identity

These wrappers can improve interactive data analysis, always return
themselves, but are really useful as you can make your data analysis as
chatty as you want interactively.

- [`names_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`count_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`mutate_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_min_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_max_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`arrange_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`distinct_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`filter_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_sample_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`filter_if_any_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_min_max_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  [`slice_group_sample_identity()`](https://olivroy.github.io/reuseme/reference/eda-identity.md)
  : Helpers that return the same value

### read data and clean names

- [`read_clean()`](https://olivroy.github.io/reuseme/reference/read_clean.md)
  : Read and clean names and set variable labels
- [`write_temp_excel()`](https://olivroy.github.io/reuseme/reference/write_temp_excel.md)
  : Write temporary Excel to Downloads
