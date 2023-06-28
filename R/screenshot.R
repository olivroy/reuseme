#' Save the image in your clipboard to png  in a `figures/` dir
#'
#' Creates a `figures/` directory if in RStudio Project. Will create a `man/figures` in a package.
#' After using the shortcut Win + Shift + S, you can call this function
#'
#' Still have to validate if it works on MacOS, as it is not clear whether the image goes to the clipboard by default
#' @param file A file name, ideally `-` (kebab-case). (extension ignored)
#'
#' @return invisibly returns the full image path
#' @export
#' @examples
#' \dontrun{
#' screenshot(file = "my-new-image")
#' }
screenshot <- function(file = NULL) {
  # https://z3tt.github.io/graphic-design-ggplot2/tips-to-improve-your-ggplot-workflow.html#save-ggplot-output-with-the-correct-dimensions
  # Could wrap ggsave also
  check_string(file, allow_null = TRUE)
  if (!rlang::is_interactive()) {
    cli::cli_warn("This function is only meant to be used interactively.")
    return(invisible(NULL))
  }
  rlang::check_installed("magick")
  proj_dir <- ifelse(is_pkg(), "man/figures/", "figures/")
  if (!fs::dir_exists(proj_dir)) {
    cli::cli_abort(c(
      x = "The directory where we want to save the image, {proj_dir} doesn't exist.",
      i = "Run {.run fs::dir_create(\"{proj_dir}\")} to create it.",
      "Then, rerun {.fun screenshot} to save the screenshot"
    ))
  }
  is_generic <- is.null(file)
  file <- file %||% "image" %>% fs::path_ext_remove()
  if (file == "image") {
    files_named_image <- fs::dir_ls(proj_dir, type = "file", regexp = "image.+png")
    increment <- if (rlang::has_length(files_named_image, 0)) {
      0
    } else {
      stringr::str_extract(files_named_image, "\\d+") %>%
        as.numeric() %>%
        max()
    }
    file <- glue::glue("image-{stringr::str_pad(increment + 1, width = 2, pad = '0')}")
  }
  file <- fs::path("figures", file, ext = "png")
  screen <- tryCatch(
    magick::image_read("clipboard:"),
    error = function(e) {
      cli::cli_abort(
        c("x" = "The clipboard must contain an image."),
        parent = NA
      )
    }
  )
  magick::image_write(image = screen, path = file, format = "png", comment = "screenshot")

  bullets <- c(
    "Use with quarto, Rmd (source mode) with",
    '![]({file}){{fig-alt="" width="70%"}}'
  )
  if (is_generic) {
    bullets <- c(
      bullets,
      "i" = "Consider using a more precise name",
      "rename_file('{file}', 'figures/better-name.png')",
      "i" = "See {.help reuseme::rename_file} for details."
    )
  }

  cli::cli_inform(bullets)
  invisible(file)
}
