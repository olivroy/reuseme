#' Save the current image in clipboard to png in your active directory
#'
#' @description
#' The screenshot will be saved as `.png` to a directory following these rules
#' 1. In a regular RStudio project (or a Quarto book), it will be saved to a `images/` directory
#' 2. In a package project, it will be saved in a `man/figures` directory
#' 3. In a Quarto Blog project, it will save in the current post's folder.
#' 4. You can always override these defaults by setting `dir`
#'
#' After using the shortcut Win + Shift + S, you can call this function!
#'
#' @details
#' If no file name is supplied, a file named `image0*.png` will be created.
#' The function then prompts you to rename the file with a more expressive name.
#' It will continue the numbering if a file named image exists.
#'
#' Still have to validate if it works on macOS, as it is not clear whether the image goes to the clipboard by default
#'
#' @param file A file name, ideally `-` (kebab-case). (extension ignored) (optional, default is `image.png`)
#' @param dir A directory (optional), to override the directory rules mentioned in the description.
#' @return The full image path, invisibly.
#' @export
#' @examples
#' if (FALSE) {
#'   # Add an image to the clipboard
#'   # Run the following
#'   screenshot(file = "my-new-image")
#' }
#'
screenshot <- function(file = NULL, dir = NULL) {
  # https://z3tt.github.io/graphic-design-ggplot2/tips-to-improve-your-ggplot-workflow.html#save-ggplot-output-with-the-correct-dimensions
  # Could wrap ggsave also
  check_string(file, allow_null = TRUE)

  if (!rlang::is_interactive()) {
    cli::cli_warn("Remove {.fn screenshot} from scripts. It is only meant to be used interactively.")
    return(invisible(NULL))
  }

  img_dir <- if (!is.null(dir)) {
    if (!fs::is_dir(dir) || !fs::dir_exists(dir)) {
      cli::cli_abort(c(x = "{.arg dir} must be `NULL` or a valid directory."))
    }

    dir
  } else if (is_pkg()) {
    "man/figures/"
  } else if (is_quarto_blog()) {
    check_active_qmd_post()
  } else {
    "images/"
  }

  if (!fs::dir_exists(img_dir)) {
    cli::cli_abort(c(
      x = "The directory where we want to save the image, {img_dir} doesn't exist.",
      i = "Run {.run fs::dir_create(\"{img_dir}\")} to create it.",
      "Then, rerun {.fun screenshot} to save the screenshot"
    ))
  }

  rlang::check_installed("magick")

  is_generic_file_name <- is.null(file)
  file <- file %||% "image"
  file <- fs::path_ext_remove(file)

  if (file == "image") {
    files_named_image <- fs::dir_ls(
      path = img_dir,
      type = "file",
      regexp = "image.+png",
      recurse = FALSE
    )

    increment <- if (!rlang::has_length(files_named_image)) {
      0
    } else {
      increment_val <- stringr::str_extract(files_named_image, "\\d+")
      max(as.numeric(increment_val))
    }

    file <- glue::glue("image-{stringr::str_pad(increment + 1, width = 2, pad = '0')}")
  }

  img_path <- fs::path(img_dir, file, ext = "png")

  screen_shot <- tryCatch(
    magick::image_read("clipboard:"),
    error = function(e) {
      cli::cli_abort(
        c("x" = "The clipboard must contain an image."),
        parent = NA
      )
    }
  )

  magick::image_write(
    image = screen_shot,
    path = img_path,
    format = "png",
    comment = "screenshot"
  )

  img_path_chr <- as.character(img_path)

  bullets <- c(
    "Use with quarto, Rmd (source mode) with",
    '![]({img_path_chr}){{fig-alt="" width="70%"}}'
  )

  if (is_generic_file_name) {
    bullets <- c(
      bullets,
      "i" = "Consider using a more precise name",
      "rename_file('{img_path_chr}', '{img_dir}/better-name.png')",
      "i" = "See {.help reuseme::rename_file} for details."
    )
  }

  cli::cli_inform(bullets)
  invisible(file)
}
