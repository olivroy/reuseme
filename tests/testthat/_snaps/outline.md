# file_outline() works

    Code
      file_outline(path = my_test_files, alpha = TRUE)
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
      
      -- `my-analysis.R`  Analyse my streets 
    Output
      `i` A real one
      `i` A true one
      `i` TODO Create a new version- `Donev?`
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
      `i` Refer to google (<https://google.com>)
      `i` Roxygen section
      `i` Section title
      `i` Write my streets
      `i` data wrangling
      `i` 'R/my-file.R'
    Message
      
      -- `many-titles.md`  The title is the only outline element 
    Output
      `i` Another title
      `i` Last title
      `i` Second level
      `i` TODO this is an item- `Donev?`
    Message
      
      -- `single-title.md`  The title is the only outline element 

# alpha and work_only arguments work

    Code
      file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
    Message
      
      -- `ref/my-analysis.R`  Analyse my streets 
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
      `i` TODO improve this Viz!- `Donev?`

# pattern works as expected

    Code
      file_outline(pattern = "not found", path = file)
    Message
      `pattern = "not found"` did not return any results looking in 1 file.
      i Run `` `proj_file()` `` to search in file names too.

---

    Code
      file_outline("Viz", path = file)
    Message
      
      -- `outline-script.R`  Example for `file_outline()` 
    Output
      `i` TODO improve this Viz!- `Donev?`

---

    Code
      file_outline("Example for", path = file)
    Message
      
      -- `outline-script.R`  Example for `file_outline()` 

