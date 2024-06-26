# From @violetcereza https://github.com/olivroy/reuseme/issues/28#issuecomment-2128290268
outline_data <- proj_outline() |>

  # Convert the many is_ columns into mutually exclusive "outline row types"
  tidyr::pivot_longer(
    names_to = "type", names_prefix = "is_", c(
      dplyr::starts_with("is_"), -is_md, -is_second_level_heading_or_more
    )
  ) %>%
  # # Double check that types are mututally exclusive
  # filter(all(value == F), .by = c(file_short, title, line_id))
  # filter(sum(value) > 1, .by = c(file_short, title, line_id))
  dplyr::filter(value) %>%
  # We drop these because they don't serve to add much context to TODOs (they don't affect heirarchy)
  dplyr::filter(type != "tab_or_plot_title") %>%

  # Some useful definitions!
  dplyr::mutate(
    title = dplyr::coalesce(outline_el, title_el),
    file_short = fs::path_file(file),
    n_leading_hash = type %>% dplyr::case_match(
      c("todo_fixme", "tab_or_plot_title") ~ NA,
      .default = n_leading_hash
    )
  ) %>%

  # For each file, stick a item at the top of the outline
  dplyr::group_by(file, file_short) %>%
  group_modify(\(data, group) data %>% add_row(
    .before = 0,
    n_leading_hash = -1,
    title = group$file_short,
    type = "file"
  )) %>%

  mutate(
    # Processing how title displays based on type
    print_title = dplyr::case_match(
      type,
      "todo_fixme" ~ link,
      .default = link_rs_api
    ) %>% dplyr::coalesce(title) %>% purrr::map_chr(cli::format_inline),

    # Assign TODO items (and other items missing n_leading_hash)
    # to be indented under the last seen header level
    indent = dplyr::coalesce(n_leading_hash, zoo::na.locf0(n_leading_hash+1)),

    # If there are any headers that skip an intermediate level, pick them up
    skip_level = indent > dplyr::lag(indent)+1,
    skip_level_should_be = ifelse(skip_level, dplyr::lag(indent)+1, NA),
    skip_level_adjust = dplyr::case_when(
      # All the items below on the outline should be adjusted backwards
      skip_level ~ skip_level_should_be-indent,
      # Unless we reach a point on the outline where we're back up in
      # the hierachy, so stop adjusting.
      indent <= zoo::na.locf0(skip_level_should_be) ~ 0
    ) %>%
      # Carry the adjustments to later rows
      zoo::na.locf0() %>% dplyr::coalesce(0),

    indent = indent + skip_level_adjust
  ) |>
  dplyr::ungroup() |>
  dplyr::select(title, type, n_leading_hash, indent, print_title)

dat_ready_to_tree <- outline_data %>%
  dplyr::mutate(
    # Give items IDs so titles do not have to be unique
    item_id = as.character(dplyr::row_number()),
    indent_wider = indent,
    x = TRUE
  ) %>%

  # We need these wide cumsum `header1` type fields to determine which items belong to which parents
  tidyr::pivot_wider(names_from = indent_wider, values_from = x, values_fill = FALSE, names_prefix = "header") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("header"), cumsum)) %>%

  # For each row, pick the IDs of all direct children from the outline
  purrr::pmap(function(...) with(list(..., childdata = .), tibble(
    title,
    print_title,
    indent,
    item_id,
    type,
    parent_level_id = get(stringr::str_c("header", indent)),
    children_ids = childdata %>%
      dplyr::rename(childindent = indent) %>%
      dplyr::filter(
        childindent == indent+1,
        cumsum(childindent == indent) == parent_level_id
      ) %>%
      dplyr::pull(item_id) %>% list()
  ))) %>%
  purrr::list_rbind() %>%
  # View()
  dplyr::select(item_id, children_ids, print_title)

dat_ready_to_tree |>
  cli::tree()

# browse-pkg
dat_ready_to_tree |>
  cli::tree("5")

dat_ready_to_tree |>
  cli::tree("10")
dat_ready_to_tree |>
  dplyr::filter(purrr::map_lgl(children_ids, \(x) length(x) > 0))

dat_ready_to_tree |>
  cli::tree("13")
