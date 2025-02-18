#' Check if files referenced in source files exist in a current dir
#'
#' 1. It goes through the source files (.R/.qmd etc.),
#' 2. It identifies data files (.csv, .xlsx) read or written
#' 3. Search on the system if these files exist.
#'
#' Still WIP, so you can add code for false positive as needed.
#'
#' @param path a directory to search for
#' @param quiet Whether it should print messages?
#' @returns A logical
#' @export
#' @keywords internal
#' @details
#' To find genuine referenced files, we exclude different paths
#'
#' 1. Those created with `fs::path()` or `file.path()` or `glue::glue()`
#' 2. Those that are checked for `fs::file_exists()`, `file.exists()`
#' 3. Deleted with `fs::file_delete()`, `unlink()`
check_referenced_files <- function(path = ".", quiet = FALSE) {
  # TODO insert in either proj_outline, or rename_file
  if (path == "." || fs::is_dir(path)) {
    # FIXME in Rbuilignore, change `^_pkgdown\.yml$` to `_pkgdown.yml` to make sure it works
    path <- fs::dir_ls(path = path, recurse = TRUE, regexp = "\\.(R|md|ya?ml|builignore)$", all = TRUE, type = "file")
    # FIXME Support _pkgdown when you want it. will likely require adjustments, not worth the effort for now.
    path <- fs::path_filter(path = path, regexp = "_files|tests/testthat|_book|_freeze|_site|.github|_pkgdown|\\.revealjs\\.md$|\\.html\\.md$", invert = TRUE) # need to do this in 2 places
  } else if (fs::path_ext(path) %in% c("R", "yml", "yaml", "Rmd", "md", "qmd", "Rmarkdown", "gitignore", "Rbuildignore")) {
    path <- path
  } else {
    cli::cli_abort("Wrong specification.")
  }

  # TODO probably needs a `detect_genuine_path()`
  # Returns
  referenced_files <- get_referenced_files(path)

  files_detected <- unique(referenced_files)
  references_a_non_existent_file <- !(fs::file_exists(files_detected) | file.exists(files_detected)) # to avoid burden for now.

  if (!any(references_a_non_existent_file)) {
    if (!quiet) cli::cli_inform(c("v" = "All referenced files in current dir exist."))
    return(invisible())
  }

  # maybe order (so link to location) isn't quite right when many are found?
  non_existent_files <- files_detected[references_a_non_existent_file]
  if (quiet) {
    cli::cli_warn(
      c(
        "Found {length(non_existent_files)} referenced file{?s} in folder.",
        i = "See {.help reuseme::check_referenced_files} for more info.",
        "There are locations in source files (qmd, Rmd, R) where a non-existent file (.csv, .xlsx etc.) is referenced.",
        "Run {.code check_referenced_files(quiet = FALSE)} to see where this file is referenced.",
        "{.file {non_existent_files}}"
      ),
      call = expr(check_referenced_files())
    )
  } else {
    solve_file_name_conflict(
      files = path,
      regex = paste0(non_existent_files, collapse = "|"),
      extra_msg = "Check in source files and rename the referenced (csv, xlsx etc.) files accordingly.",
      quiet = quiet,
      what = "to non-existent files"
    )
  }
  invisible(non_existent_files)
}

#' Check if outdated or non-existent file is.
#'
#' If `quiet = FALSE` (default) will give a hint of where the file is referenced.
#'
#' @param files which files to search in
#' @param regex a regex related to the file name to search for
#' @param dir A directory where to operate
#' @param extra_msg Extra message to pass
#' @param what Which file conflicts we talking about
#' @param quiet A logical, informs where the occurrences are found. (Default, `FALSE`)
#' @param new_file (Optional) new file name
#' @return Mostly called for its side-effects, but will return the number of matches
#' (0 if no referenced files are problematic)
#' @export
#' @keywords internal
solve_file_name_conflict <- function(files, regex, dir = ".", extra_msg = NULL, quiet = FALSE, what = NULL, new_file = NULL) {
  # Remove potential regex conflicts
  regex <- stringr::str_replace_all(regex, "\\\\|\\)|\\(|\\}|\\{|\\?|\\$|~|\\*|\\^", ".")

  if (dir != ".") {
    cli::cli_abort("Don't know how to do this.")
  }

  bullets_df <-
    rlang::set_names(files) |>
    purrr::map(\(x) readLines(x, encoding = "UTF-8", warn = FALSE)) |>
    purrr::map(\(x) tibble::enframe(x, name = "line_number", value = "content")) |>
    dplyr::bind_rows(.id = "file")

  if (!is.null(new_file)) {
    nchar_new_file <- nchar(new_file)
    bullets_df$content <- gsub(new_file, paste0("x", nchar_new_file), bullets_df$content, fixed = TRUE)
  }

  bullets_df <- bullets_df[grepl(regex, bullets_df$content), ]

  if (nrow(bullets_df) == 0) {
    return(0)
  }

  verbose <- !quiet
  if (verbose) {
    lines_match <- bullets_df$line_number
    # Derive column match
    start_end_pos <- stringr::str_locate(bullets_df$content, regex)
    cols_match <- dplyr::coalesce(
      start_end_pos[, "start"] - 1L,
      1L
    )
    # Create hyperlinks from lines and columns
    bullets <- stringr::str_glue(
      "{{.file {bullets_df$file}:{lines_match}:{cols_match}}}"
    )

    # Will truncate 20 (revert up if you don't like)
    display_msg <- NULL
    bullets_to_display <- cli::ansi_collapse(bullets)

    f_inform <- if (length(bullets) > 20) {
      cli::cli_warn
    } else {
      cli::cli_inform
    }

    # Remove duplicated Found x references
    which_bullet_to_replace <- stringr::str_subset(extra_msg, stringr::fixed("Found references to"), negate = TRUE)
    # possibly just move up our
    # extra_msg[i] <-
    f_inform(c(
      extra_msg,
      "i" = paste0("Found {length(bullets)} reference{?s} ", what, " in ", bullets_to_display, "."),
      display_msg
    ))
  } else {
    cli::cli_inform(
      c(
        extra_msg,
        "run {.run reuseme::check_referenced_files(quiet = TRUE)} to see where the conflicts are."
      ),
      .frequency = "always", .frequency_id = "nonexistantfiles"
    )
  }

  length(bullets)
}

# Helpers ----------------

# Read content of file (with file path)

get_referenced_files <- function(files) {
  # Create a list of genuine referenced files
  # TODO Add false positive references
  # TODO fs::path and file.path should be handled differently
  potential_files_refs <- purrr::map(files, \(x) readLines(x, encoding = "UTF-8", warn = FALSE)) |>
    purrr::list_c(ptype = "character") |>
    stringr::str_subset(pattern = "\\:\\:dav.+lt|\\:\\:nw_|g.docs_l.n|target-|\\.0pt|https?\\:", negate = TRUE) |> # remove false positive from .md files
    stringr::str_subset(pattern = "file.path|fs\\:\\:path\\(|path_package|system.file|withr", negate = TRUE) |> # Exclude fs::path() and file.path from search since handled differently.
    stringr::str_subset(pattern = "file.[(exist)|(delete)]|glue\\:\\:glue|unlink|write_union|open_rs_doc|use_build_ign|use_git", negate = TRUE) |> # don't detect where we test for existence of path or construct a path with glue
    stringr::str_subset(pattern = "[(regexp)|(pattern)]\\s\\=.*\".*[:alpha:]\"", negate = TRUE) |> # remove regexp = a.pdf format
    stringr::str_subset(pattern = "grepl?\\(|stringr|g?sub\\(", negate = TRUE) |> # avoid regexp
    stringr::str_subset(pattern = stringr::fixed("nocheck"), negate = TRUE) |> # nocheck is a way to add
    stringr::str_subset(pattern = "standalone-.+\\.R", negate = TRUE) # Avoid import standalone file ref.

  # Extract file name as string
  ref_files_strings <- extract_string_file_names(potential_files_refs)

  # Extract file name from line content (to catch cli files)
  ref_files_unquoted <- extract_plain_file_names(potential_files_refs)

  ref_files_strings <- rlang::set_names(ref_files_strings)
  ref_files_unquoted <- rlang::set_names(ref_files_unquoted[!is.na(ref_files_unquoted)])

  # Returns unquoted and string potential file names
  c(ref_files_strings, ref_files_unquoted)
}

# Extract file name as string (if they are used in R code)
extract_string_file_names <- function(lines) {
  lines |> # remove nocheck and unlink statements (refers to deleted files anywa)
    stringr::str_subset(stringr::fixed("\"")) |>
    stringr::str_trim() |>
    stringr::str_extract_all("\"[^\"]+\"") |>
    unlist() |>
    stringr::str_remove_all("\",?") |>
    stringr::str_subset(pattern = "\\.\\w{1,6}$") |> # file pattern
    stringr::str_subset(pattern = "\\.plot|\\.fr$|\\.frame|\\.obs$|\\.\\d{2,}$", negate = TRUE) |> # Manually add file exts that are not file exts.
    stringr::str_subset(pattern = "tmp|temp", negate = TRUE) |> # remove common file names that are not very nice
    stringr::str_subset(pattern = "https?", negate = TRUE) |> # doesn't check for files read online.
    stringr::str_subset(pattern = "\\@.+\\.", negate = TRUE) |> # email addresses or containing @
    stringr::str_subset(pattern = stringr::fixed("_fichiers/"), negate = TRUE) |> # manually remove false positive
    stringr::str_subset(pattern = "\n", negate = TRUE) |> # remove things with line breaks
    stringr::str_subset(pattern = "^\\.[:alpha:]{1,4}$", negate = TRUE) |> # remove reference to only file extensions
    stringr::str_subset(pattern = "\\.\\d+$", negate = TRUE) # remove 0.000 type
}

# For debugging I'd need to have a function that takes the content of a line
# that returns what file path it returns
extract_plain_file_names <- function(lines) {
  # the the .file file-ref
  ref_files_unquoted <- lines |>
    stringr::str_subset("[:alnum:](\\.[Rq]md|\\.R|\\.ya?ml|\\.csv|\\.xlsx)") |> # update below too
    stringr::str_subset("read_.+\\(|excel_sheets|= FALSE|= TRUE", negate = TRUE) |> # Avoid the read_excel read_ (will be caught in strings)
    stringr::str_remove("^\\s+\\-\\s+") |> # trim _quarto.yml
    stringr::str_remove("href:\\s") |>
    # remove pipe
    stringr::str_remove("\\s*\\||\\>") |>
    # Remove assignment
    stringr::str_remove(".+<-\\s")

  # Remove everything after the extension


  ref_files_unquoted <- stringr::str_replace(ref_files_unquoted, "\\{\\.path", "{.file")
  ref_files_unquoted <- stringr::str_replace(ref_files_unquoted, "write\\.csv", "write_csv")
  ref_files_unquoted <- stringr::str_replace(ref_files_unquoted, "(\\.([Rq]md|R|ya?ml|csv|xlsx)(?!\\})).+", "\\1")
  # remove leading comment
  ref_files_unquoted <- stringr::str_remove(ref_files_unquoted, "^[#\\s']+")

  # Remove leading quote
  ref_files_unquoted <- stringr::str_remove(ref_files_unquoted, "^\"")
  ref_files_unquoted3 <- dplyr::case_when(
    stringr::str_detect(ref_files_unquoted, "file/file") ~ NA_character_,
    grepl("{.file", ref_files_unquoted, fixed = TRUE) ~ stringr::str_extract(ref_files_unquoted, "\\{.file ([^\\}]+)\\}", group = 1),
    # let's assume that if we don't use the cli file, we don't use space in our file
    stringr::str_detect(ref_files_unquoted, "`") ~ stringr::str_remove(ref_files_unquoted, "[^`]+`"),
    # TODO refine extraction rules as needed. for now, we are ignoring.
    stringr::str_length(ref_files_unquoted) > 100 ~ NA_character_,
    stringr::str_count(ref_files_unquoted, "\\s") >= 3 ~ stringr::str_remove_all(ref_files_unquoted, ".+\\s"),
    .default = ref_files_unquoted
  )
  # remove false positive that contains a character that makes no sense
  ref_files_unquoted4 <- ref_files_unquoted3[!grepl("=|\\:\\:", ref_files_unquoted3)]
  ref_files_unquoted4
}
