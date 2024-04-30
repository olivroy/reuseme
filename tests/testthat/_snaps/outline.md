# file_outline() works

    Code
      file_outline(path = my_test_files, alpha = TRUE)
    Message
      
      -- `my-analysis.R` 🕒 Analyse my streets 
    Output
      `i` A real one
      `i` A true one
      `i` TODO Create a new version- `Donev?`
      `i` Read my streets data
      `i` Refer to google (<https://google.com>)
      `i` Roxygen section
      `i` Section title
      `i` Write my streets
      `i` data wrangling
      `i` 'R/my-file.R'
    Message
      
      -- `my-analysis.md` 🕒 My doc title 
    Output
      `i` A section
      `i` A code section
      `i` A subsection
      `i` A section2
      `i` A long ggplot2 title
      `i` A code section
      `i` Dashboard card

# Other arguments work

    Code
      file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
    Message
      
      -- `ref/my-analysis.R` 🕒 Analyse my streets 
    Output
      `i` Read my streets data
      `i` Write my streets

# file_outline() is a data frame

    Code
      outline
    Message
      
      -- `outline-script.R` 🕒 Example for `file_outline()` 
    Output
      `i` Load packages
      `i` Wrangle + visualize data
      `i` A great title
      `i` TODO improve this Viz!- `Donev?`

