# file_outline() works

    Code
      file_outline(my_test_files, alpha = TRUE)
    Message
      
      -- `my-analysis.R`  
    Output
      `i` Analyse my {streets}
      `i` TODO Create a new version
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
      `i` Section title
      `i` Write my streets
      `i` 'R/my-file.R'
    Message
      
      -- `my-analysis.md`  
    Output
      `i` A section
      `i` A code section
      `i` A subsection
      `i` A section2
      `i` A long ggplot2 title
      `i` A code section
      `i` Dashboard card
      `i` My doc title
    Message
      
      -- `title.md`  
    Output
      `i` The title is the only outline element
    Message
      
      -- `titles.md`  
    Output
      `i` Another title
      `i` Last title
      `i` Second level
      `i` The title is the only outline element
      `i` `function_name()` title
      `i` TODO this is an item

# alpha arguments works

    Code
      file_outline(my_test_file, pattern = "street", alpha = TRUE)
    Message
      
      -- `outline/my-analysis.R`  
    Output
      `i` Analyse my {streets}
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Streetart>) data
      `i` Write my streets

# file_outline() is a data frame

    Code
      outline
    Message
      
      -- `outline-script.R`  
    Output
      `i` Example for `file_outline()`
      `i` Load packages
      `i` Wrangle + visualize data
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
      
      -- `outline-script.R`  
    Output
      `i` Example for `file_outline()`
      `i` TODO improve this Viz!

---

    Code
      file_outline(file, "Example for")
    Message
      
      -- `outline-script.R`  
    Output
      `i` Example for `file_outline()`

# file_outline() detects correctly knitr notebooks

    Code
      file_outline(test_path("_outline", "knitr-notebook.R"))
    Message
      
      -- `knitr-notebook.R`  
    Output
      `i` Crop Analysis Q3 2013
      `i` A great section

