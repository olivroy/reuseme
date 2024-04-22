# file_outline() works

    Code
      file_outline(path = my_test_file)
    Message
      
      -- `ref/my-analysis.R` 🦐 
    Output
      `i` Read my streets data
      `i` Write my streets
      `i` TODO eventually detect this- `Donev?`
      `i` TODO Create a new version- `Donev?`

# Other arguments work

    Code
      file_outline("street", my_test_file, alpha = TRUE, work_only = FALSE)
    Message
      
      -- `ref/my-analysis.R` 🦐 
    Output
      `i` Read my streets data
      `i` Write my streets

