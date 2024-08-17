# file_outline() works

    Code
      file_outline(my_test_files, alpha = TRUE)
    Message
      
      -- `my-analysis.R`  Analyse my {streets} 
    Output
      `i` A section
      `i` TODO Create a new version
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
      `i` Section title
      `i` The last section
      `i` Write my streets
      `i` data wrangling
      `i` 'R/my-file.R'
    Message
      
      -- `my-analysis.md`  My doc title 
    Output
      `i` A section
      `i` A code section
      `i` A subsection
      `i` A section2
      `i` A long ggplot2 title
      `i` A code section
      `i` Dashboard card
    Message
      
      -- `title.md`  The title is the only outline element 
      
      -- `titles.md`  The title is the first outline element 
    Output
      `i` Another title
      `i` Last title
      `i` Second level
      `i` `function_name()` title
      `i` TODO this is an item

# alpha arguments works

    Code
      file_outline(my_test_file, pattern = "street", alpha = TRUE)
    Message
      
      -- `outline/my-analysis.R`  Analyse my {streets} 
    Output
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Streetart>) data
      `i` Write my streets

# file_outline() is a data frame

    Code
      outline
    Message
      
      -- `outline-script.R`  Example for `file_outline()` 
    Output
      `i` Load packages
      `i` Wrangle + visualize data
      `i` A great title
      `i` TODO improve this Viz!

# pattern works as expected

    Code
      file_outline(file, pattern = "not found")
    Message
      `pattern = "not found"` did not return any results looking in 1 file.
      i Run `` `proj_file()` `` to search in file names too.

---

    Code
      file_outline(file, "Viz")
    Message
      
      -- `outline-script.R`  Example for `file_outline()` 
    Output
      `i` TODO improve this Viz!

---

    Code
      file_outline(file, "Example for")
    Message
      
      -- `outline-script.R`  Example for `file_outline()` 

# file_outline() detects correctly knitr notebooks

    Code
      file_outline(test_path("_outline", "knitr-notebook.R"))
    Message
      
      -- `knitr-notebook.R`  Crop Analysis Q3 2013 
    Output
      `i` A great section

