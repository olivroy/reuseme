# pkgload::load_all()

#' Extract roxygen tag
#'
#' Tell me what this does
#'
#' # Section to extract
#'
#' Well this is a section
#'
#' @md
#' @param file A file
#' @returns A named list with name = file:line, and element is the section title
extract_roxygen_tag_location <- function(file = testthat::test_path("_ref", "test-roxygen.R"), tag) {
  suppressMessages(aa <- roxygen2::parse_file(file))
  # browser()
  # don't parse noRd tags
  aa <- purrr::discard(aa, \(x) roxygen2::block_has_tags(x, "noRd"))
  # Return early if no roxy tags
  if (length(aa) == 0) {
    return(character(0L))
  }
  # browser()
  pos <- purrr::map(aa, \(x) roxygen2::block_get_tags(x, tags = tag))
  if (all(lengths(pos) == 0)) {
    return(character(0))
  }
  aa <- aa[lengths(pos) > 0]
  pos <- purrr::list_flatten(pos)
  objects <- purrr::map(
    aa,
    \(x) x$object$topic
  )
  if (any(lengths(objects) == 0)) {
    name_tag <- purrr::map(
      aa,
      \(x) roxygen2::block_get_tag_value(x, "name")
    )
    for (i in seq_along(objects)) {
      if (is.null(objects[[i]])) {
        if (!is.null(name_tag[[i]])) {
          objects[[i]] <- name_tag[[i]]
        } else {
          objects[[i]] <- "no-topic"
        }
      }
    }
    if (any(lengths(objects) == 0)) {
      # should not happen. I chose "no-topic" instead.
      cli::cli_abort("Could not resolve object or topic names.")
    }
  }


  # browser()
  pos <- purrr::set_names(pos, file)

  # browser()
  val <- withCallingHandlers(
    purrr::map2(pos, objects, \(x, obj_name) {
    el <- x$val
    el_has_names <- !is.null(names(el))

    if (length(el) == 1 && !el_has_names) {
      el <- paste0(
        el, "____", obj_name
      )
      names(el) <- x$line
      return(el)
    }
    if (tag %in% c("description", "details") && !el_has_names) {
      # TODO when stable delete
      # print(x$val)
      # print(el_has_names)
      # cli::cli_inform("return early (no headings)")
      return(NULL)
    }
    # use raw instead
    lines <- stringr::str_split_1(x$raw, "\n")
    # browser()
    keep <- which(o_is_section_title(lines))

    if (length(keep) == 0L) {
      # TODO Delete when stable debugging
      # cli::cli_inform(" No section title detected")
      return(NULL)
    }
    # line position.
    line_pos <- x$line + seq_along(lines) - 1L
    final_lines_to_include <- lines[keep]
    # Will not make this transformation and will consider roxygen comments to be
    # final_lines_to_include <- stringr::str_remove(final_lines_to_include, "^#+\\s")

    final_lines_to_include <- paste0(final_lines_to_include, "____", obj_name)
    names(final_lines_to_include) <- line_pos[keep]
    # TODO Delete when stable for debugging
    # if (length(final_lines_to_include) != 1) {
    #   cli::cli_warn("el resulted to {.val {final_lines_to_include}}", "using first element for now")
    # }
    final_lines_to_include
  }),
  error = function(e) {
    cli::cli_abort(
      "For tag = {tag}, obj_name = {objects}, wrong size, should be {length(pos)}"
    )

  })

  # rlang::set_names(val, nam)
  # merge line number and file name
  # I wonder if purrr make it easy to do tidyverse/purrr#1064
  # list(x = c(el1 = 1), x = c(el2 = 2, el3 = 3))
  #> list(x = c(el1 = 1, el2 = 2, el3 = 3))
  val <- val |> purrr::compact()

  if (FALSE) {
    val <- unlist(val)
    names(val) <- stringr::str_replace(names(val), "\\.(\\d+)$", ":\\1")
  } else {
    # purrr::list_flatten(
    #  name_spec = "{outer}:{inner}"
    # )
    val <- vctrs::list_unchop(
      val,
      name_spec = "{outer}:{inner}",
      ptype = "character"
    )
  }


  # hack to keep tag
  if (length(val) > 0) {
    names(val) <- paste0(names(val), "____", tag)
  }
  val
}

titles_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "title"))

section_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "section"))
subsection_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "subsection"))

desc_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "description"))

details_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "details"))

family_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "family"))
concept_list <- purrr::map(fs::dir_ls("R"), \(x) extract_roxygen_tag_location(x, tag = "concept"))
roxy_parsed <- vctrs::vec_c(
  titles_list,
  section_list,
  desc_list,
  details_list,
  family_list,
  concept_list,
  .name_spec = "{outer}:{inner}",
) |>
  vctrs::list_unchop(
    name_spec = "{inner}"
  ) |>
  tibble::enframe() |>
  tidyr::separate_wider_delim(
    cols = name,
    names = c("file", "tag"),
    delim = "____"
  )
roxy_parsed |>
  tidyr::separate_wider_delim(
    cols = value,
    delim = "____",
    names = c("outline_el", "topic"),
  ) |>
  tidyr::separate_wider_delim(
    file,
    delim = ":",
    names = c("file", "line")
  ) |>
  dplyr::relocate(
    topic, outline_el, file, line, tag
  ) |>
  dplyr::mutate(
    is_md = tag %in% c("subsection", "details", "description", "section")
  )
