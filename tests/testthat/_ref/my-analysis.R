# Analyse my streets ---------------
## Read my streets data ---------------
my_streets <- read.csv("data/my-streets.csv")
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
# TODO Create a new version

#' ## Roxygen section
#'
#'
#' gt::tab_header(title = "A real one") |>
#'   gt::tab_header(subtitle = "A true one")

# {.file R/my-file.R} ---

cli::cli_ul("Refer to {.href [google](https://google.com)}") # not rendered properly currently.
# ## a commented section title -----

if (TRUE) {
  # Section title ---
}
