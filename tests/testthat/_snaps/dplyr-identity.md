# Side effects are what's intended in interactive sessions

    Code
      dplyr::starwars %>% filter_identity(name == "C3P-O", extra_msg = c(
        "Looking at C3P-0 characteristics")) %>% distinct_identity(hair_color, sex,
        extra_msg = c(
          "Looking if there is association between `hair_color` and `sex`. ",
          "Printing 15 rows."), nrows = 15)
    Message
      `reuseme::filter_identity()` returned no row.
    Output
      # A tibble: 23 x 2
         hair_color    sex           
         <chr>         <chr>         
       1 blond         male          
       2 <NA>          none          
       3 none          male          
       4 brown         female        
       5 brown, grey   male          
       6 black         male          
       7 auburn, white male          
       8 auburn, grey  male          
       9 brown         male          
      10 <NA>          male          
      11 <NA>          hermaphroditic
      12 white         male          
      13 grey          male          
      14 none          none          
      15 auburn        female        
      # i 8 more rows
      # i Use `print(n = ...)` to see more rows
    Message
      i Looking if there is association between `hair_color` and `sex`. Printing 15 rows.

