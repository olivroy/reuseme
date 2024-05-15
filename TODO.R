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
# TODO [outline] news heading should not all show by default.
# TODO [outline] remove examples from outline. Sometimes commented code is caught.
# TODO [outline] roxygen comments processing should be left to {.fn roxygen2::parse_file}
# TODO [outline] show key like {.fn pak::pkg_deps_tree} does.
# TODO [outline] roxygen function title
# TODO [outline] remove ggtext markup from plot title.
# FIXME outline comments are now interpreted as section
# TODO outline todos in qmd file inside html comment
# TODO reframe more than one issue. nw drive
# TODO [proj file] should not suggest snaps and test if R exists. (rare that I want to modify tests from another project)
# TODO don't show emoji if all have emojis
# TODO [delete] accept to delete test if not in tests/ folder
# TODO [delete] if in Downloads/ or Desktop/, this is a reason to delete
# TODO [delete] if file_outline fails for a reason or another, should not throw error?
# TODO delete generated files 
