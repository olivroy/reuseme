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
#' Still have to validate if it works on macOS, as it is not clear whether the
#' image goes to the clipboard by default
#'
#' The maximum number of images in a folder is 99. (only padding 2), should be enough.
#'
#' You should not be able to overwrite a screenshot with a generic name, only a
#' named one as it is possible you may require to retake your screenshot.
#' @param file A file name, ideally `-` (kebab-case). (extension ignored) (optional, default is `image.png`)
#' @param proj A project name
#' @param dir A directory (optional), to override the directory rules mentioned in the description. inside `proj`.
#' @return The full image path, invisibly.
#' @export
#' @examples
#' if (FALSE) {
#'   # Add an image to the clipboard
#'   # Run the following
#'   screenshot(file = "my-new-image")
#' }
#'
screenshot <- function(file = NULL, proj = proj_get(), dir = NULL) {
  # https://z3tt.github.io/graphic-design-ggplot2/tips-to-improve-your-ggplot-workflow.html#save-ggplot-output-with-the-correct-dimensions
  # Could wrap ggsave also

  if (!rlang::is_interactive()) {
    cli::cli_warn("Remove {.fn reuseme::screenshot} from scripts. It is only meant to be used interactively.")
    return(invisible())
  }


  check_string(file, allow_null = TRUE)
  is_active_proj <- identical(proj, proj_get2())

  proj_path <- proj_path(proj)


  if (!is_rstudio()) {
    cli::cli_warn("This feature may not work as excepted outside RStudio.")
  }

  # Making dir a full path if not in active project.
  if (!is.null(dir)) {
    dir_rel <- dir
    if (!is_active_proj) {
      dir <- fs::path(proj_path, dir_rel)
    }

    if (!fs::is_dir(dir) || !fs::dir_exists(dir)) {
      cli::cli_abort(c(x = "{.arg dir} must be {.code NULL} or a valid directory within {.arg proj}."))
    }
  }

  img_dir <- if (!is.null(dir)) {
    if (is_active_proj) {
      dir
    } else {
      fs::path(proj_path, dir_rel)
    }
  } else if (is_pkg(proj_path)) {
    "man/figures"
  } else if (is_quarto_blog(proj_path)) {
    if (is_active_proj) {
      get_active_qmd_post(base_path = proj_path)
    } else {
      cli::cli_abort(c("You are trying to add a screenshot to a Quarto blog.", "Either open the RStudio project or supply {.arg dir} to write a screenshot in the directory."))
    }
  } else if (is_active_proj) {
    "images"
  } else if (!is_active_proj && !is_pkg(proj_path)) {
    fs::path(proj_path, "images")
  } else {
    cli::cli_abort("Not a supported case.")
  }

  img_dir_rel <- fs::path_rel(img_dir, start = proj_path)



  if (is_quarto_blog(proj_path)) {
    file_index_qmd <- fs::path(img_dir, "index.qmd")
    if (!fs::file_exists(file_index_qmd)) {
      cli::cli_abort("In a Quarto blog, {.arg dir} must be a relative path to a Quarto Post. i.e. index.qmd.")
    }
  }

  if (!fs::dir_exists(img_dir)) {
    cli::cli_abort(c(
      x = "The directory where we want to save the image, {img_dir} doesn't exist.",
      i = "Run {.run fs::dir_create(\"{img_dir}\")} to create it.",
      "Then, rerun {.fun reuseme::screenshot} to save the screenshot"
    ))
  }

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

    if (rlang::has_length(files_named_image)) {
      increment_val <- stringr::str_extract(files_named_image, "image-(\\d{2})", group = 1)
      increment <- max(as.numeric(increment_val))
    } else {
      increment <- 0
    }

    file <- glue::glue("image-{stringr::str_pad(increment + 1, width = 2, pad = '0')}")
  }
  img_file <- fs::path(file, ext = "png")

  img_path <- fs::path(img_dir, img_file)

  if (fs::file_exists(img_path) && is_generic_file_name) {
    cli::cli_abort("You cannot overrite a screenshot. Please change `file`")
  }

  rlang::check_installed("magick")
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
  img_path_rel_chr <- as.character(fs::path_rel(img_path, proj_path))
  img_file_chr <- as.character(img_file)
  img_dir_rel_chr <- as.character(img_dir_rel)
  img_dir_chr <- as.character(img_dir)
  proj_chr <- as.character(proj)
  change_project_command <- "[{proj_chr}](reuseme::proj_switch('{proj_chr}'))"

  bullets <- if (is_active_proj) {
    if (length(fs::dir_ls(".", regexp = "qmd|Rmd")) > 0) {
      "Use with Quarto, Rmd (source mode) with"
    }
  } else {
    "Use with Quarto, Rmd (source mode) in {.run [{proj_chr}](reuseme::proj_switch('{proj_chr}'))}"
  }

  if (is_quarto_blog(proj_path)) {
    bullets <- c(
      bullets,
      # cli bug r-lib/cli#683
      '![]({fs::path_file(img_path_chr)}){{fig-alt="" width="70%"}}'
    )
  } else {
    bullets <- c(
      bullets,
      '![]({img_path_rel_chr}){{fig-alt="" width="70%"}}'
    )
  }



  if (is_generic_file_name) {
    bullets <- c(
      bullets,
      "i" = "Consider using a more precise name",
      "reuseme::rename_files2('{img_path_chr}', '{img_dir_chr}/better-name.png', warn_conflicts = 'none')",
      "i" = "See {.help reuseme::rename_files2} for details."
    )
  }

  cli::cli_inform(bullets)
  invisible(img_path_rel_chr)
}
