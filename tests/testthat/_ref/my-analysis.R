# Read my streets data ---------------
my_streets <- read.csv("data/my-streets.csv")
new_dat <- my_streets |>
  dplyr::mutate(
    title = "data wrangling",
    x = 2**2,
    title = NULL
  )

# my-streets are everywhere
# Write my streets ------------
utils::write.csv(new_dat, "data/my-streets-clean.csv", row.names = FALSE)

file.path("data", "my-streets.csv") # TODO eventually detect this
fs::path("data", "my-streets", ext = "csv")
# TODO Create a new version
