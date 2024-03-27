# `filter_if_any()` doesn't work with `across()`

    Code
      filter_if_any(dplyr::starwars, dplyr::across(ends_with("color"), function(x)
        stringr::str_detect(x, "brown")))
    Condition
      Error in `filter_if_any()`:
      x You must provide logical expressions to `...`
      i See `?dplyr::filter()` for more information on how it works.

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

# summarise_with_total() works

    Code
      mtcars |> dplyr::group_by(vs = as.character(vs)) |> summarise_with_total(x = sum(
        mpg), .label = "All vs", .first = TRUE)
    Output
      # A tibble: 3 x 2
        vs         x
        <chr>  <dbl>
      1 All vs  643.
      2 0       299.
      3 1       344.
    Code
      mtcars |> tibble::as_tibble() |> summarise_with_total(x = sum(mpg), .by = vs,
      .label = "All vs", .first = TRUE)
    Output
      # A tibble: 3 x 2
        vs         x
        <fct>  <dbl>
      1 All vs  643.
      2 0       299.
      3 1       344.
    Code
      mtcars |> tibble::as_tibble() |> dplyr::mutate(vs = as.character(vs)) |>
        summarise_with_total(x = sum(mpg), y = mean(mpg), .by = vs, .label = "All vs",
        .first = FALSE)
    Output
      # A tibble: 3 x 3
        vs         x     y
        <chr>  <dbl> <dbl>
      1 0       299.  16.6
      2 1       344.  24.6
      3 All vs  643.  20.1

# slice_min_max() works

    Code
      slice_min_max(mtcars, mpg, n = 3)
    Output
                          minmax  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      Cadillac Fleetwood     min 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
      Lincoln Continental    min 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
      Camaro Z28             min 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
      Honda Civic            max 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
      Lotus Europa           max 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
      Fiat 128               max 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
      Toyota Corolla         max 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    Code
      slice_min_max(mtcars, mpg, n = 3, ascending = FALSE)
    Output
                          minmax  mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      Toyota Corolla         max 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
      Fiat 128               max 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
      Honda Civic            max 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
      Lotus Europa           max 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
      Camaro Z28             min 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
      Cadillac Fleetwood     min 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
      Lincoln Continental    min 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4

# na_if2() works with expr and values

    Code
      na_if2(vec)
    Condition
      Error in `na_if2()`:
      ! One of `expr` or `values` must be supplied.
    Code
      na_if2(vec, expr = c(0, 2))
    Condition
      Error in `na_if2()`:
      ! `expr` must be a logical vector the same size as x, not a double vector

