# file_outline() works

    Code
      file_outline(path = my_test_files, alpha = TRUE)
    Message
      
      -- `my-analysis.md`  My doc title 
    Output
      `i` A section
      `i` A subsection
      `i` A section2
      `i` A long ggplot2 title
      `i` A code section
      `i` A long ggplot2 title with more details2
      `i` A long ggplot2 title with more details3.
      `i` Dashboard card
    Message
      
      -- `titles.md`  The title is the only outline element 
    Output
      `i` Another title
      `i` Last title
      `i` Second level
      `i` `function_name()` title
      `i` TODO this is an item- `Donev?`
    Message
      
      -- `my-analysis.R`  Analyse my {streets} 
    Output
      `i` TODO Create a new version- `Donev?`
      `i` Read my streets (<https://https://en.wikipedia.org/wiki/Street_art>) data
      `i` Section title
      `i` Write my streets
      `i` data wrangling
      `i` 'R/my-file.R'
    Message
      
      -- `title.md`  The title is the only outline element 

# alpha and work_only arguments work

    Code
      file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
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

# file_outline() works well with figure captions

    Code
      file_outline(path = test_path("_outline", "quarto-caps.md"))
    Message
      
      -- `quarto-caps.md`  title 
    Output
      `i` A long ggplot2 title with more details
      `i` Heading  <i class="cheatsheet-icon fa-solid fa-tags"></i>
      `i` A long ggplot2 title with more details
      `i` Heading2\_done
      `i` Dashboard link
      `i` Dashboard link

# file_outline() detects correctly knitr notebooks

    Code
      file_outline(path = test_path("_outline", "knitr-notebook.R"))
    Message
      
      -- `knitr-notebook.R`  Crop Analysis Q3 2013 
    Output
      `i` A great section

