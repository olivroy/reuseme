my_streets <- read.csv("data/my-streets.csv")
new_dat <- my_streets |>
  dplyr::mutate(
    x = 2**2
  )

# my-streets are everywhere
utils::write.csv(new_dat, "data/my-streets-clean.csv", row.names = FALSE)
