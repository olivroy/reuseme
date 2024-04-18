# rename_files2(): prevents file renaming if conflicts

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    Message
      x Did not rename files!
      ! Found references to "data/my-streets.csv" in project
      i Change file path to "data/my-roads.csv" in files ahead of renaming file or see `Find in Files` Replace All if confident. Copied new name to clipboard
      Also change object names to snake_case that follow the new file name.
      i Found 2 references to "my-streets/my_streets" in 'R/my-analysis.R:1:0' and 'R/my-analysis.R:2:11'.
    Output
      [1] "data/my-roads.csv"

# rename_files2(): is easier to test messages with no action

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", overwrite = TRUE,
        action = "test")
    Message
      x Did not rename files!
      ! Found references to "data/my-streets.csv" in project
      i Change file path to "data/my-roads.csv" in files ahead of renaming file or see `Find in Files` Replace All if confident. Copied new name to clipboard
      Also change object names to snake_case that follow the new file name.
      i Found 2 references to "my-streets/my_streets" in 'R/my-analysis.R:1:0' and 'R/my-analysis.R:2:11'.
    Output
      [1] "data/my-roads.csv"

# rename_files2(): renames files if forced to do so

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", warn_conflicts = "none",
        overwrite = TRUE)
    Message
      x Renamed file to 'data/my-roads.csv' by force. Be careful.

# rename_files2(): doesn't check for references if file name is short

    Code
      rename_files2("R/a.R", "R/b.R")
    Message
      v Renamed file to 'R/b.R' without issue.

# rename_files2(): priorizes references if name is generic or widely used in files

    Code
      rename_files2("data/my-streets.csv", "data-raw/my-streets.csv")
    Message
      x Did not rename files!
      ! Found references to "data/my-streets.csv" in project
      i Change file path to "data-raw/my-streets.csv" in files ahead of renaming file or see `Find in Files` Replace All if confident. Copied new name to clipboard
      i Found 1 reference to "data/my-streets.csv" in 'R/my-analysis.R:1:24'.
    Output
      [1] "data-raw/my-streets.csv"

# rename_files2(): can accept overridden preferences

    Code
      rename_files2("data/my-streets.csv", "data-raw/my-streets.csv", warn_conflicts = "all")
    Message
      x Did not rename files!
      ! Found references to "data/my-streets.csv" in project
      i Change file path to "data-raw/my-streets.csv" in files ahead of renaming file or see `Find in Files` Replace All if confident. Copied new name to clipboard
      i Found 2 references to "my-streets/my_streets" in 'R/my-analysis.R:1:0' and 'R/my-analysis.R:2:11'.
    Output
      [1] "data-raw/my-streets.csv"

# Helper files returns the expected input

    Code
      compute_conflicts_regex("x", "unknown_strategy")
    Condition
      Error in `compute_conflicts_regex()`:
      ! Not implemented a return value for "unknown_strategy"
      Make sure tests are added.
      i This is an internal error that was detected in the reuseme package.
        Please report it at <https://github.com/olivroy/reuseme/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

