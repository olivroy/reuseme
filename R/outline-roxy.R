#' Extract roxygen tag
#'
#' Tell me what this does
#'
#' # Section to extract
#'
#' Well this is a section
#'
#' @noRd
#' @param file A list of roxy blocks
#' @returns A named list with name = file:line, and element is the section title
#' @examples
#' extract_roxygen_tag_location(tag = "title")
extract_roxygen_tag_location <- function(file, tag) {
  # suppressMessages(aa <- roxygen2::parse_file(file))
  # browser()
  aa <- file
  pos <- purrr::map(aa, \(x) roxygen2::block_get_tags(x, tags = tag))
  # browser()
  if (all(lengths(pos) == 0L)) {
    return(character(0L))
  }
  aa <- aa[lengths(pos) > 0L]
  pos <- pos[lengths(pos) > 0L]
  objects <- purrr::map(
    aa,
    \(x) {
      if (!is.null(x$object$topic)) {
        return(x$object$topic)
      }
      object_call <- as.character(x$call)
      if (length(object_call) == 1) {
        return(object_call)
      }
      if (length(object_call) > 1) {
        return(paste0(object_call[2], "()"))
      }
      NULL
    }
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
          objects[[i]] <- NA_character_
        }
      }
    }
    if (any(lengths(objects) == 0)) {
      # should not happen. I chose NA instead.
      cli::cli_abort("Could not resolve object or topic names.")
    }
  }

  # double object name
  for (i in seq_along(pos)) {
    l <- length(pos[[i]])
    if (l > 1) {
      # to repeat object name to be same length as `pos`
      objects[[i]] <- as.list(rep(objects[[i]][1], length.out = l))
    }
  }
  # Unnest to make it easier.
  pos <- purrr::list_flatten(pos)
  objects <- purrr::list_flatten(objects)
  if (length(objects) != length(pos)) {
    print(objects)
    print(pos)
    cli::cli_abort(
      c(
        "Could not resolve pos and objects to be the same length.",
        "pos = {length(pos)}, objects = {length(objects)}."
      ),
      .internal = TRUE
    )
  }

  pos <- purrr::set_names(pos, pos$file)

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
      # final_lines_to_include <- sub("^#+\\s", "", final_lines_to_include)

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
        "For tag = {tag}, obj_name = {objects}, wrong size, should be {length(pos)}, not {length(objects)}.",
        parent = e
      )
    }
  )

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

join_roxy_fun <- function(file) {
  # Assuming that only @keywords internal is used, when keywords is specified.
  # Will probably have to handle other cases, but this is not recommended.
  # https://roxygen2.r-lib.org/reference/tags-index-crossref.html
  parsed_files <- purrr::map(
    file,
    # discard if noRd or has keywords.
    \(x) purrr::discard(x, \(y) roxygen2::block_has_tags(y, c("keywords", "noRd")))
  )
  # TODO exclude S3 methods
  # Return early if no roxy tags
  if (length(parsed_files) == 0) {
    return(character(0L))
  }
  if (is.null(names(parsed_files))) {
    # browser()
    parsed_files <- parsed_files |> purrr::set_names(purrr::map_chr(parsed_files, \(x) x$file))
    # cli::cli_abort("parsed files must be named at this point.")
  }
  # parsed_files <- set_names(parsed_files, \(x))
  titles_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "title"))

  section_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "section"))
  subsection_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "subsection"))

  desc_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "description"))

  details_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "details"))

  family_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "family"))
  concept_list <- purrr::map(parsed_files, \(x) extract_roxygen_tag_location(x, tag = "concept"))
  roxy_parsed <- vctrs::vec_c(
    titles_list,
    section_list,
    subsection_list,
    desc_list,
    details_list,
    family_list,
    concept_list # ,
    # .name_spec = "{outer}:::::{inner}",
  ) |>
    vctrs::list_unchop(
      name_spec = "{outer}.....{inner}"
    ) |>
    tibble::enframe() |>
    tidyr::separate_wider_delim(
      cols = name,
      names = c("file_line", "tag"),
      delim = "____"
    )

  roxy_parsed <- roxy_parsed |>
    tidyr::separate_wider_delim(
      cols = value,
      delim = "____",
      names = c("content", "topic"),
    )
  if (!all(grepl("\\.{5}", roxy_parsed$file_line, fixed = FALSE))) {
    problems <- which(!grepl("\\.{5}", roxy_parsed$file_line, fixed = FALSE))
    # rowser()
    # roxy_parsed
    cli::cli_abort("Malformed file line at {problems}.")
  }
  roxy_parsed <- roxy_parsed |>
    tidyr::separate_wider_delim(
      file_line,
      delim = ".....",
      names = c("file", "line")
    )

  if (nrow(roxy_parsed) == 0) {
    return(roxy_parsed)
  }
  roxy_parsed1 <- roxy_parsed |>
    dplyr::relocate(
      file, topic, content, line, tag
    ) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::mutate(content = dplyr::case_when(
      # only keep the first line of section, subsection, family and concept tags.
      tag %in% c("family", "concept") ~ stringr::str_extract(content, "^(.+)(\n)?", group = 1),
      # Remove code blocks...
      tag %in% c("details", "description") ~ stringr::str_remove_all(content, "```[^`]+```"),
      # Also remove : from section
      tag %in% c("section", "subsection") ~ stringr::str_extract(content, "^(.+)\\s?\\:\\s?\n?", group = 1),
      .default = content
    )) |>
    tidyr::separate_longer_delim(content, delim = "\n")

  roxy_parsed1$topic <- dplyr::na_if(roxy_parsed1$topic, "NA")
  r <- roxy_parsed1 |>
    dplyr::mutate(
      n = dplyr::n(),
      # error if something is length 0.
      line = seq(from = line[1], length.out = n[1], by = 1),
      .by = id,
      content = dplyr::case_when(
        # remove markup.
        # avoid \code{} in tags. r-lib/roxygen2#1618
        tag %in% c("title", "section", "subsection") ~ stringr::str_remove_all(content, "\\}+|\\\\+[:alpha:]+\\{+|\\{$"),
        .default = content
      )
    ) |>
    dplyr::filter(nzchar(content), !stringr::str_detect(content, "`r\\s")) |>
    dplyr::select(-id, -n)
  # remove link...
  r$content <- stringr::str_remove_all(r$content, "\\\\[^\\\\]+\\]\\{")
  # FIXME escape markup see next line
  # To test it add a fix tag to next line and try to figure it out...
  # to fix figure out why #' @section Escaping `{` and  `}` : isn't parsing. Workaround to remove it.
  r |> dplyr::filter(!grepl("^Escaping", content))
}

# helper for interactive checking -----------


active_doc_parse <- function(doc = active_rs_doc()) {
  doc <- purrr::set_names(doc)
  parsed <- purrr::map(doc, \(x) roxygen2::parse_file(x, env = NULL))
  parsed |>
    join_roxy_fun() |>
    define_outline_criteria_roxy()
}
