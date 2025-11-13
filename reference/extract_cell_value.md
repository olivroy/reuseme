# Elegant wrapper around filter and pull

It can be very useful when trying to extract a value from somewhere, and
you have one col that represents the unique id.

## Usage

``` r
extract_cell_value(
  data,
  var,
  filter,
  name = NULL,
  length = NULL,
  unique = FALSE
)
```

## Arguments

- data:

  A data.frame

- var:

  A variable specified as:

  - a literal variable name

  - a positive integer, giving the position counting from the left

  - a negative integer, giving the position counting from the right.

  The default returns the last column (on the assumption that's the
  column you've created most recently).

  This argument is taken by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names and column locations).

- filter:

  the filter

- name:

  The variable for the name (by default, will look for `rownames`), can
  be quoted (safer).

- length:

  A fixed length to check for the output

- unique:

  A logical. Should return unique values?

## Value

A (named) character vector (if name is specified)

## Examples

``` r
# extract the skin_color for C-3PO
extract_cell_value(
  data = dplyr::starwars,
  var = skin_color,
  filter = name == "C-3PO",
  length = 1 # ensure the length will be 1.
)
#> [1] "gold"
# will return a named vector of mpg (as mtcars has rownames.)
mtcars |>
  extract_cell_value(
    var = mpg,
    filter = vs == 0
  )
#>           Mazda RX4       Mazda RX4 Wag   Hornet Sportabout          Duster 360 
#>                21.0                21.0                18.7                14.3 
#>          Merc 450SE          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                16.4                17.3                15.2                10.4 
#> Lincoln Continental   Chrysler Imperial    Dodge Challenger         AMC Javelin 
#>                10.4                14.7                15.5                15.2 
#>          Camaro Z28    Pontiac Firebird       Porsche 914-2      Ford Pantera L 
#>                13.3                19.2                26.0                15.8 
#>        Ferrari Dino       Maserati Bora 
#>                19.7                15.0 

# Extract hair color for all people
extract_cell_value(
  data = dplyr::starwars,
  var = skin_color,
  filter = TRUE,
  name = "name" # ensure it is a named vector that corresponds to their unique ID
)
#>        Luke Skywalker                 C-3PO                 R2-D2 
#>                "fair"                "gold"         "white, blue" 
#>           Darth Vader           Leia Organa             Owen Lars 
#>               "white"               "light"               "light" 
#>    Beru Whitesun Lars                 R5-D4     Biggs Darklighter 
#>               "light"          "white, red"               "light" 
#>        Obi-Wan Kenobi      Anakin Skywalker        Wilhuff Tarkin 
#>                "fair"                "fair"                "fair" 
#>             Chewbacca              Han Solo                Greedo 
#>             "unknown"                "fair"               "green" 
#> Jabba Desilijic Tiure        Wedge Antilles      Jek Tono Porkins 
#>    "green-tan, brown"                "fair"                "fair" 
#>                  Yoda             Palpatine             Boba Fett 
#>               "green"                "pale"                "fair" 
#>                 IG-88                 Bossk      Lando Calrissian 
#>               "metal"               "green"                "dark" 
#>                 Lobot                Ackbar            Mon Mothma 
#>               "light"        "brown mottle"                "fair" 
#>          Arvel Crynyd Wicket Systri Warrick             Nien Nunb 
#>                "fair"               "brown"                "grey" 
#>          Qui-Gon Jinn           Nute Gunray         Finis Valorum 
#>                "fair"       "mottled green"                "fair" 
#>         Padmé Amidala         Jar Jar Binks          Roos Tarpals 
#>               "light"              "orange"                "grey" 
#>            Rugor Nass              Ric Olié                 Watto 
#>               "green"                "fair"          "blue, grey" 
#>               Sebulba         Quarsh Panaka        Shmi Skywalker 
#>           "grey, red"                "dark"                "fair" 
#>            Darth Maul           Bib Fortuna           Ayla Secura 
#>                 "red"                "pale"                "blue" 
#>          Ratts Tyerel              Dud Bolt               Gasgano 
#>          "grey, blue"          "blue, grey"         "white, blue" 
#>        Ben Quadinaros            Mace Windu          Ki-Adi-Mundi 
#> "grey, green, yellow"                "dark"                "pale" 
#>             Kit Fisto             Eeth Koth            Adi Gallia 
#>               "green"               "brown"                "dark" 
#>           Saesee Tiin           Yarael Poof              Plo Koon 
#>                "pale"               "white"              "orange" 
#>            Mas Amedda          Gregar Typho                 Cordé 
#>                "blue"                "dark"               "light" 
#>           Cliegg Lars     Poggle the Lesser       Luminara Unduli 
#>                "fair"               "green"              "yellow" 
#>         Barriss Offee                 Dormé                 Dooku 
#>              "yellow"               "light"                "fair" 
#>   Bail Prestor Organa            Jango Fett            Zam Wesell 
#>                 "tan"                 "tan" "fair, green, yellow" 
#>       Dexter Jettster               Lama Su               Taun We 
#>               "brown"                "grey"                "grey" 
#>            Jocasta Nu                R4-P17            Wat Tambor 
#>                "fair"         "silver, red"         "green, grey" 
#>              San Hill              Shaak Ti              Grievous 
#>                "grey"    "red, blue, white"        "brown, white" 
#>               Tarfful       Raymus Antilles             Sly Moore 
#>               "brown"               "light"                "pale" 
#>            Tion Medon                  Finn                   Rey 
#>                "grey"                "dark"               "light" 
#>           Poe Dameron                   BB8        Captain Phasma 
#>               "light"                "none"                "none" 
```
