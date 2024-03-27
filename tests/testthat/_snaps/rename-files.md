# rename_files2(): prevents file renaming if conflicts

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv")
    Message
      Did not rename files!
      i Make sure you change the file path to
      data/my-roads.csv
      in these locations (`new_name` copied to clipboard) or see `Find in Files`
      Replace All if confident.
      i Consider changing name snake_case objects that follow the file names
      i Use [Ctrl + C], then [Ctrl + Shift + Up] for replacing
      'R/my-analysis.R:1:0'
      'R/my-analysis.R:2:11'
      Rerun the code to make it work or use `force = TRUE`
    Output
      [1] "data/my-roads.csv"

# rename_files2(): is easier to test messages with no action

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE, action = "test")
    Message
      Here are the conflicts. Review changes carefully
      renaming file anyway
      'R/my-analysis.R:1:0'
      'R/my-analysis.R:2:11'
      Testing mode, did not rename file

# rename_files2(): renames files if forced to do so

    Code
      rename_files2("data/my-streets.csv", "data/my-roads.csv", force = TRUE)
    Message
      Here are the conflicts. Review changes carefully
      renaming file anyway
      'R/my-analysis.R:1:0'
      'R/my-analysis.R:2:11'
      v Renamed file to 'data/my-roads.csv' without issue.
      Check in source files and rename the referenced (csv, xlsx etc.) files
      accordingly.
      'R/my-analysis.R:1:24'
      'R/my-analysis.R:7:27'

