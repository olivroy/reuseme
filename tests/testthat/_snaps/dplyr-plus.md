# `filter_if_any()` doesn't work with `across()`

    Code
      dplyr::starwars %>% filter_if_any(dplyr::across(ends_with("color"), function(x)
        stringr::str_detect(x, "brown")))
    Condition
      Error in `filter_if_any()`:
      ! You didn't provide logical expressions. See `?dplyr::filter()`

# adds rows in front, but warns the user

    Code
      filter_if_any(sw, is.na(hair_color), hair_color == "brown")
    Output
      # A tibble: 23 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu~
       2 R2-D2        96    32 <NA>       white, bl~ red               33 none  mascu~
       3 Leia Or~    150    49 brown      light      brown             19 fema~ femin~
       4 Beru Wh~    165    75 brown      light      blue              47 fema~ femin~
       5 R5-D4        97    32 <NA>       white, red red               NA none  mascu~
       6 Chewbac~    228   112 brown      unknown    blue             200 male  mascu~
       7 Han Solo    180    80 brown      fair       brown             29 male  mascu~
       8 Greedo      173    74 <NA>       green      black             44 male  mascu~
       9 Jabba D~    175  1358 <NA>       green-tan~ orange           600 herm~ mascu~
      10 Wedge A~    170    77 brown      fair       hazel             21 male  mascu~
      # i 13 more rows
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>,
      #   vehicles <list>, starships <list>

