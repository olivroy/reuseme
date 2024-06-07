# Analyse my {streets} ---------------
## Read my [streets](https://https://en.wikipedia.org/wiki/Street_art) data -------
my_streets <- read.csv("data/my-streets.csv", silent = TRUE)
new_dat <- my_streets |>
  dplyr::mutate(
    title = "data wrangling", # "problem"
    x = 2**2,
    title = NULL
  )

# my-streets are everywhere
# Write my streets ------------
utils::write.csv(new_dat, "data/my-streets-clean.csv", row.names = FALSE)

file.path("data", "my-streets.csv") # TODO eventually detect file.path for check_referenced_files
fs::path("data", "my-streets", ext = "csv")
# system.file("file.R", package = "fs") # should not be listed.
# TODO Create a new version

# {.file R/my-file.R} ---
tab_header(title = md("**A table title**"))
cli::cli_ul("Refer to {.href [google](https://google.com)}")
# ## a commented section title -----

if (TRUE) {
  # Section title ---
  pattern <- "{1} <abbr title = '{4}'>{3}</abbr> <br> <small><p style='color:gray;'> {2}</p> </small>"
}
