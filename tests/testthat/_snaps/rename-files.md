# rename_files2(): prevents file renaming if conflicts

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    Message
      x Did not rename files!
      ! Found references to "data/my-streets.csv" in project
      i Change file path to "data/my-roads.csv" or see `Find in Files` Replace All if
        confident. Copied new name to clipboard
      i Also change object names to snake_case that follow the new file name.
      i Found 2 references to "my-streets" in 'R/my-analysis.R:1:0' and
        'R/my-analysis.R:2:11'.
      Rerun the code to make it work or use `force = TRUE`
    Output
      [1] "data/my-roads.csv"

# rename_files2(): is easier to test messages with no action

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE, action = "test")
    Message
      Here are the conflicts. Review changes carefully
      renaming file anyway
      i Found 2 references to "my-streets" in 'R/my-analysis.R:1:0' and
        'R/my-analysis.R:2:11'.
      Testing mode, did not rename file
      i Call `reuseme::check_referenced_files()` to see if there are dead links in dir.

# rename_files2(): renames files if forced to do so

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE)
    Message
      Here are the conflicts. Review changes carefully
      renaming file anyway
      i Found 2 references to "my-streets" in 'R/my-analysis.R:1:0' and
        'R/my-analysis.R:2:11'.
      x Renamed file to 'data/my-roads.csv' by force. Be careful.
      i Call `reuseme::check_referenced_files()` to see if there are dead links in dir.

# rename_files2(): doesn't check for references if file name is short

    Code
      rename_files2("R/a.R", "R/b.R")
    Message
      v Renamed file to 'R/b.R' without issue.
      i Call `reuseme::check_referenced_files()` to see if there are dead links in dir.

