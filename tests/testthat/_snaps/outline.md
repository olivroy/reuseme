# file_outline() works

    Code
      file_outline(path = my_test_file)
    Message
      
      -- `my-analysis.R` 🦐 Analyse my streets 
    Output
      `i` Read my streets data
      `i` data wrangling
      `i` Write my streets
      `i` TODO eventually detect file.path for check_referenced_files- `Donev?`
      `i` TODO Create a new version- `Donev?`
      `i` Roxygen section
      `i` A real one
      `i` A true one
      `i` 'R/my-file.R'
      `i` Refer to google (<https://google.com>)
      `i` Section title

# Other arguments work

    Code
      file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
    Message
      
      -- `ref/my-analysis.R` 🦐 Analyse my streets 
    Output
      `i` Read my streets data
      `i` Write my streets

# file_outline() is a data frame

    Code
      outline
    Message
      
      -- `outline-script.R` 🦐 Example for `file_outline()` 
    Output
      `i` Load packages
      `i` Wrangle + visualize data
      `i` A great title
      `i` TODO improve this Viz!- `Donev?`

