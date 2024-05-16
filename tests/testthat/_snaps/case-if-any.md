# case_if_any() basic work

    Code
      case_if_any(mtcars$vs == 1 ~ "Woww", mtcars$mpg > 15 ~ "QW", mtcars$qsec > 18 ~
        "ooh lalal", .default = " ")
    Output
       [1] "QW"                "QW"                "Woww;QW;ooh lalal"
       [4] "Woww;QW;ooh lalal" "QW"                "Woww;QW;ooh lalal"
       [7] " "                 "Woww;QW;ooh lalal" "Woww;QW;ooh lalal"
      [10] "Woww;QW;ooh lalal" "Woww;QW;ooh lalal" "QW"               
      [13] "QW"                "QW"                " "                
      [16] " "                 " "                 "Woww;QW;ooh lalal"
      [19] "Woww;QW;ooh lalal" "Woww;QW;ooh lalal" "Woww;QW;ooh lalal"
      [22] "QW"                "QW"                " "                
      [25] "QW"                "Woww;QW;ooh lalal" "QW"               
      [28] "Woww;QW"           "QW"                "QW"               
      [31] " "                 "Woww;QW;ooh lalal"

# wrong cases error

    Code
      case_if_any(mtcars$vs == 1 ~ "Woww", mtcars$mpg > 15 ~ "QW", mtcars$qsec > 18 ~
        "ooh lalal", .sep = " ")
    Condition
      Error in `case_if_any()`:
      x `.sep` cannot be contained in the condition.
      i Change either the replacement text, or `.sep`
    Code
      case_if_any(mtcars$vs == 1 ~ "Woww", mtcars$mpg > 15 ~ "QW", .sep = "")
    Condition
      Error in `case_if_any()`:
      ! `.sep` must be a single string, not the empty string "".

