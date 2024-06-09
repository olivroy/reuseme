# TODO [screenshot] make the behaviour different when vignettes vs articles: vignettes should place it in man/figures, while articles could put it in vignettes/articles file.
# TODO [screenshot] RStudio addin to insert the code directly in the qmd doc. No longer needed with RStudio 2023.12
# TODO use_family() to edit .R file to add @family data frames tags to roxygen
# TODO mutate_identity redundant if the focus pillar PR was merged. r-lib/pillar#585
# TODO [rename] if many matches, separate those with the exact path.
# TODO [outline] make ggtitle work
# TODO [outline] show extra msg only for some, but in file outline, not in proj?
# TODO [outline] detect help calls and apply markup. `?fs::file_show` disregard finishing `.` (not followed by dot)
# TODO escape_markup doesn't work with complex operation {{x^2}} for example. Maybe if detecting something complex, use cli_escape function. escape-complex-markyp branch created to try to address this.
# TODO [outline] avoid evaluating in current env.
# TODO wrap regexps in functions
# TODO items should not truncate leading code when marking as complte trunc-todo-code
# TODO [outline] remove examples from outline. Sometimes commented code is caught.
# TODO [outline] roxygen comments processing should be left to {.fn roxygen2::parse_file}
# TODO [outline] show key like {.fn pak::pkg_deps_tree} does.
# TODO [outline] remove ggtext markup from plot title.
# FIXME [outline] comments are now interpreted as section
# TODO [outline] todos in qmd file inside html comment
# TODO reframe more than one issue. nw drive
# TODO [delete] generated files
# TODO [proj_file] to accesss data (return the path in this case?)
# TODO [check_referenced_files] doesn't check for {.file R/file.R}
# TODO browse_pkg should open by default if no vignettes are found, because there is not much to do in the R-session.
# TODO exclude _files from `proj_list()`
# TODO [outline] Show function call if exported + not internal + bonus if has family tag! rstudio/rstudio#14766
# TODO title of file could be function title if it is first element [proj_outline]
# TODO rename_files should be less noisy about project name file
# TODO add_to_tricks(). when detecting TRICK like complete todo, but not remove line. requires a scheme. moves the item to tricks.md at the correct place. (copy to clipboard is probably enough)
# TODO [outline] just create an `exclude` argument that will take an option? (exclude can be files or expressionsm, or elements.)
# TODO [outline] remove snaps from outline and add a link in the test file instead?
# TODO [outline] family should be displayed differently..
# TODO [outline] find a way to make print bookmarks..
# TODO [outline] escape some content in headings see {.file tests/testthat/_outline/quarto-caps.md} for examples.
