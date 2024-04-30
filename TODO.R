# TODO [screenshot] make the behaviour different when vignettes vs articles: vignettes should place it in man/figures, while articles could put it in vignettes/articles file.
# TODO [screenshot] RStudio addin to insert the code directly in the qmd doc. No longer needed with RStudio 2023.12
# TODO use_family() to edit .R file to add @family data frames tags to roxygen
# TODO mutate_identity would not be required if the focus pillar PR was merged. r-lib/pillar#585
# TODO [rename] if many matches, separate those with the exact path.
# TODO [outline] make ggtitle work
# TODO [outline] show extra msg only for some, but in file outline, not in proj?
# TODO detect url automatically, like link_issue instead of needing {.url }
# TODO [outline] detect help calls and apply markup. `?fs::file_show` disregard finishing `.` (not followed by dot)
# TODO escape_markup doesn't work with complex operation {{x^2}} for example. Maybe if detecting something complex, use cli_escape function. escape-complex-markyp branch created to try to address this.
# TODO [outline] avoid evaluating in current env.
# TODO wrap regexps in functions
