# Side effects are what's intended in interactive sessions

    Code
      f_identity(dplyr::starwars)
    Output
      # A tibble: 1 x 14
        name  height  mass hair_color skin_color eye_color birth_year sex   gender   
        <chr>  <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr>    
      1 C-3PO    167    75 <NA>       gold       yellow           112 none  masculine
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>, ...
    Message
      i Looking at C3P-0 characteristics
      `reuseme::filter_if_any()` returned no row.
      No one is named Non-existent.
    Output
      # A tibble: 5 x 14
        name      height  mass hair_color skin_color eye_color birth_year sex   gender
        <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
      1 Wat Tamb~    193    48 none       green, gr~ unknown         NA   male  mascu~
      2 Tarfful      234   136 brown      brown      blue            NA   male  mascu~
      3 Bossk        190   113 none       green      red             53   male  mascu~
      4 Taun We      213    NA none       grey       black           NA   fema~ femin~
      5 Darth Va~    202   136 none       white      yellow          41.9 male  mascu~
      # i 5 more variables: homeworld <chr>, species <chr>, films <list>, ...
    Message
      i Check random rows here
    Output
      # A tibble: 22 x 14
         name     height  mass hair_color skin_color eye_color birth_year sex   gender
         <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
       1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu~
       2 R2-D2        96    32 <NA>       white, bl~ red               33 none  mascu~
       3 Leia Or~    150    49 brown      light      brown             19 fema~ femin~
       4 Beru Wh~    165    75 brown      light      blue              47 fema~ femin~
       5 R5-D4        97    32 <NA>       white, red red               NA none  mascu~
       6 IG-88       200   140 none       metal      red               15 none  mascu~
       7 Mon Mot~    150    NA auburn     fair       blue              48 fema~ femin~
       8 Shmi Sk~    163    NA black      fair       brown             72 fema~ femin~
       9 Ayla Se~    178    55 none       blue       hazel             48 fema~ femin~
      10 Adi Gal~    184    50 none       dark       blue              NA fema~ femin~
      # i 12 more rows
    Message
      i Looking at all individuals of a sex.
    Output
      # A tibble: 19 x 14
        name      height  mass hair_color skin_color eye_color birth_year sex   gender
        <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
      1 C-3PO        167    75 <NA>       gold       yellow           112 none  mascu~
      2 Leia Org~    150    49 brown      light      brown             19 fema~ femin~
      3 Beru Whi~    165    75 brown      light      blue              47 fema~ femin~
      4 Chewbacca    228   112 brown      unknown    blue             200 male  mascu~
      5 Han Solo     180    80 brown      fair       brown             29 male  mascu~
      # i 14 more rows
    Message
      i Looking at everyone that is C3P-0 or has brown hair.
    Output
      # A tibble: 8 x 15
        height minmax name       mass hair_color skin_color eye_color birth_year sex  
         <int> <chr>  <chr>     <dbl> <chr>      <chr>      <chr>          <dbl> <chr>
      1     66 min    Yoda         17 white      green      brown            896 male 
      2     79 min    Ratts Ty~    15 none       grey, blue unknown           NA male 
      3     88 min    Wicket S~    20 brown      brown      brown              8 male 
      4     94 min    Dud Bolt     45 none       blue, grey yellow            NA male 
      5    228 max    Chewbacca   112 brown      unknown    blue             200 male 
      6    229 max    Lama Su      88 none       grey       black             NA male 
      7    234 max    Tarfful     136 brown      brown      blue              NA male 
      8    264 max    Yarael P~    NA none       white      yellow            NA male 
      # i 6 more variables: gender <chr>, homeworld <chr>, species <chr>, ...
    Message
      i Looking at people with min and max height (4 each) total = 8 rows.
    Output
      # A tibble: 23 x 2
        hair_color sex   
        <chr>      <chr> 
      1 blond      male  
      2 <NA>       none  
      3 none       male  
      4 brown      female
      # i 19 more rows
    Message
      i Looking if there is association between `hair_color` and `sex`. Printing 15 rows.

